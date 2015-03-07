---
layout: post
title: 使用cowboy和relx搭建Web服务02 - handler和代码自动更新
description: "Build Web service with cowboy and relx 02"
category: erlang
comments: true
share: true
---

继续记录使用cowboy搭建服务器遇到的各种坑，算是备忘吧。在[上一篇Blog](http://blog.imaou.com/erlang/2015/02/24/build_web_service_with_cowboy_and_relx_1.html)里说到了如何创建简单的cowboy handler，现在来重点介绍下cowboy的几种handler写法。

## REST handler

这个算是最简单的，官方教程里例子还可以使用。主要是默认的几个导出函数名，可以在[REST handlers](http://ninenines.eu/docs/en/cowboy/HEAD/guide/rest_handlers/)部分找到。

下面只用`content_types_provided`演示下如何接受html(默认)以及json的返回格式：

~~~erlang
-module(rest_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).
-export([get_json/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, get_html},
		{<<"application/json">>, get_json}
	], Req, State}.

get_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.

get_json(Req, State) ->
	Body = jsx:encode(#{
		<<"body">> => <<"This is REST!">>
	}),
	{Body, Req, State}.
~~~

这个handler会处理请求头部的Accept，根据需要的格式调用对应处理函数：

~~~sh
curl -i -H "Accept: application/json" http://localhost:8080/rest
~~~

关于其他函数的应用，以及如何区分处理GET/POST请求，可以参考：[`canillita_news_handler.erl`](https://github.com/inaka/canillita/blob/master/src/canillita_news_handler.erl)

## websocket handler

官方websocket的例子太过简单，这里完善了下对网页端请求的处理，并且可以根据输入做出不同响应：

~~~erlang
-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
	message_position = 0
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, #state{}}.

websocket_handle({text, RawData}, Req, State) ->
	Data = re:replace(RawData, "(^\\s+)|(\\s+$)", "", [{return, list}]),
	parse_message(Data, Req, State);
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), <<"How are you doing?">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

parse_message("help", Req, State) ->
	Body = {text, <<">>> WebSocket help:\n"
					"  WebSocket is a protocol providing full-duplex "
					"communications channels over a single TCP connection."
					"The WebSocket protocol was standardized by the IETF as RFC 6455 in 2011, "
					"and the WebSocket API in Web IDL is being standardized by the W3C.\n">>},
	{reply, Body, Req, State};
parse_message("exit", Req, State) ->
	{shutdown, Req, State};
parse_message(Command, Req, State) ->
	Body = {text, binary:list_to_bin(["WS> ", Command, "\n"])},
	{reply, Body, Req, State}.
~~~

首先需要注意的是init/3，查了好久才知道原来要改成这样，并且返回：`{upgrade, protocol, cowboy_websocket}`。这样当handler初始化完成，就可以接受连接了。

`websocket_init/3`貌似没有被调用，而`websocket_info/3`也是到后来我才知道，这个是响应erlang消息用的，所以核心只有`websocket_handle/3`。

`websocket_handle`匹配除了text好像还有binary，不过从[网页](https://github.com/upbit/zserver/blob/master/priv/static/websocket.html)`websocket.send(data)`过来的就被识别为text了，暂时还不明白怎么回事：

~~~erlang
websocket_handle({text, RawData}, Req, State) ->
	Data = re:replace(RawData, "(^\\s+)|(\\s+$)", "", [{return, list}]),
	parse_message(Data, Req, State);
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.
~~~

另外因为是在input里获取的data，匹配到的RawData带有换行符，于是找到个用正则trim字符串的方法，最后传给`parse_message/3`去响应。偷懒就直接用list匹配了，更安全的方法应该是尝试转为atom吧。

最后如果不是help或exit就返回一个包含原字符串的binary。

## loop handler

### event handler

这是摸索了最久的一种handler，官方例子各种不好用，连上去自己给自己丢消息有意思吗？不过还是照着做了个，访问/event会去接收eventsource里的event-stream：

~~~erlang
-module(event_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(state, {
	count::integer()
}).


init(_Type, Req, _Opts) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	{ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
	erlang:send_after(1000, self(), {message, "Init Tick"}),
	{loop, Req2, #state{count=0}, hibernate}.

info({message, Msg}, Req, State) ->
	ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Msg, "\n\n"], Req),
	erlang:send_after(1000, self(), {message, lists:flatten(io_lib:format("Tick(~p)~n", [State#state.count]))}),
	{loop, Req, State#state{count = State#state.count + 1}, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

id() ->
	{Mega, Sec, Micro} = erlang:now(),
	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
	integer_to_list(Id, 16).
~~~

首先要理解`text/event-stream`是怎么回事，拿curl访问/eventsource会得到如下返回：

~~~sh
curl -i http://localhost:8080/eventsource
HTTP/1.1 200 OK
transfer-encoding: chunked
connection: keep-alive
server: Cowboy
date: Sat, 07 Mar 2015 15:25:54 GMT
content-type: text/event-stream

id: 510B46AD5D6D1
data: Init Tick

id: 510B46AE51CF7
data: Tick(0)

id: 510B46AF4631D
data: Tick(1)

~~~

这是一个长连接请求，在连接创建时会先进入init/3里，返回`cowboy_req:chunked_reply`表示以chunked方式返回数据。注意init函数的返回`{loop, Req, #state{}}.`，loop会让handler进入循环状态，最后的hibernate会使进程hibernation直到有消息到达。

为了触发info/3操作，于是这里首先在init里send_after了一个`{message, "Init Tick"}`，于是一秒后info/3被调用。

`info({message, Msg}`匹配到这个消息，使用`cowboy_req:chunk`发送了一个带id和data的返回数据。这个是eventsource定义的格式，data部分会被忠实的显示在/event页面上。

### cowboy_loop_handler

#### init

实际使用中肯定不会如此简单，往往我们希望在一个接口上提供数据的推送(GET)，并且还能接收要推送的内容(POST)。这里融合REST和loop来实现这样一个handler：

~~~erlang
-module(loop_handler).
-behaviour(cowboy_loop_handler).

%% cowboy handler callbacks
-export([
		init/3,
		allowed_methods/2,
		content_types_accepted/2,
		info/3,
		terminate/3
	]).

-record(state, {}).

init(_Type, Req, _Opts) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, _} ->
			{upgrade, protocol, cowboy_rest};
		{<<"GET">>, Req1} ->
			Req2 = chunk_start(Req1),
			ok = pg2:join(notify_group, self()),
			{loop, Req2, #state{}, hibernate}
	end.
~~~

首先导出cowboy handler的回掉，注意除了info还加了`allowed_methods`。在init中首先取请求的method，如果是POST就转为cowboy_rest处理，不然调用chunk_start准备推送信息：

~~~erlang
chunk_start(Req) ->
	Headers = [
		{<<"content-type">>, <<"text/event-stream">>},
		{<<"connection">>, <<"keep-alive">>}
	],
	{ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
	Data = jsx:encode(#{
		<<"messages">> => <<"connected">>,
		<<"timestamp">> => timestamp()
	}),
	ok = cowboy_req:chunk(["data: ", Data, "\n\n"], Req2),
	Req2.
~~~

这里直接推送`data: {json}`格式的数据，使用jsx:encode可以方便的转化record为json。

#### POST请求的处理

接着来看POST处理部分，既然是REST handler，那么少不了这样的定义：

~~~erlang
%% only allowed post for REST
allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json">>, handle_post}], Req, State}.
~~~

这里先用`allowed_methods`限制只接受POST请求(正常情况也是如此)，并且用`content_types_accepted`将输入限制为json，并转交handle_post处理：

~~~erlang
handle_post(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	case jsx:decode(Body) of
		Data ->
			notify_all(Data),
			{true, Req1, State}
	end.
~~~

处理也很简单，从请求中取出Body的json数据并decode为对象，接着调用`notify_all(Data)`发给其他进程

#### pg2 notify

细心的你应该注意到init里的`pg2:join(notify_group, self())`了。因为cowboy为每个连接分配了一个进程，在初始化中又使用pg2模块将这些进程添加到notify_group里，这样就得到一个类似在线(进程)列表的东西，可以用`pg2:get_members(notify_group)`取到：

~~~erlang
notify_all(Msg) ->
	lists:foreach(
		fun(Listener) ->
			lager:info("notify ~p: ~p", [Listener, Msg]),
			Listener ! {message, Msg}
		end, pg2:get_members(notify_group)).
~~~

`notify_all/1`里对每个notify_group里的进程（以GET方式连进来的请求），都发送了一个`{message, Msg}`消息给对应进程的loop handler处理。

最后就是info里将进程收到的消息，以chunk的方式发给连接在自己上面的客户端：

~~~erlang
info({message, Msg}, Req, State) ->
	Data = jsx:encode(#{
		<<"messages">> => Msg,
		<<"timestamp">> => timestamp()
	}),
	ok = cowboy_req:chunk(["data: ", Data, "\n\n"], Req),
	{loop, Req, State, hibernate}.
~~~

注: pg2相对全局的register好处是不会广播到飞组外的进程，减小foreach的规模。

## 在relx里集成sync和其它自带app

### observer

observer是Erlang里的利器，不过relx默认打包的release却没有加载它。网上找了好久才找到解决办法，修改relx.config如下：

~~~erlang
{release, {zserver_release, "1"}, [zserver,
		syntax_tools, compiler, sync,
		debugger,
		observer, runtime_tools, wx
	]}.
{extended_start_script, true}.
~~~

observer要添加`observer, runtime_tools, wx`三个库，类似debuger和tools也是加在release这里。

### sync

sync是个动态更新代码的利器，不过官方的relx部分写的比较简单，实际配置还要进行如下操作：

1. relx.config里加上`syntax_tools, compiler, sync`，加sync是为了让sync自动运行；
2. erlang.mk里relx-rel的命令，需要加上-d参数：`@$(RELX) -d -c $(RELX_CONFIG) $(RELX_OPTS)`
3. rel/sys.config里，改成下面这样：

~~~erlang
[
	{sync, [
		{src_dirs, {replace, [{"./lib/zserver-0.1.0/src/", [{outdir, "./lib/zserver-0.1.0/ebin/"}]}]}}
	]}
].
~~~

虽然第二步-d使用dev模式生成软连接，但运行的代码却是在zserver_release下的，所以src要指向`./lib/zserver-版本/`这个软连接下。这样sync就能找到要reload的代码了：

~~~erlang
(zserver@127.0.0.1)2> sync_scanner:info().
Sync Info...
ok
(zserver@127.0.0.1)3> Modules: [chatroom_manager,chatroom_server,zserver_sup,ranch_acceptor,
          ranch_acceptors_sup,ranch_conns_sup,ranch_listener_sup,ranch_tcp,
          ranch,lager_default_formatter,cowboy,lager_msg,cowboy_router,
          lager_stdlib,zserver_app,lager_format,lager_default_tracer,glc_code,
          glc_lib,gr_manager,gr_counter,lager_trunc_io,gr_param,glc_ops,glc,
          error_logger_lager_h,lager_file_backend,lager_console_backend,lager,
          lager_backend_throttle,lager_handler_watcher,lager_crash_log,
          lager_handler_watcher_sup,lager_util,lager_config,lager_sup,
          lager_app,cowboy_clock,cowboy_sup,cowboy_app,gr_manager_sup,
          gr_param_sup,gr_counter_sup,gr_sup,gr_app,sync_notify,sync_utils,
          sync_scanner,sync_options,sync,ranch_server,ranch_sup,ranch_app,
          erts_internal,erlang,erl_prim_loader,prim_zip,zlib,prim_file,
          prim_inet,prim_eval,init,otp_ring0]
Source Dirs: ["./lib/zserver-0.1.0/src/"]
Source Files: ["./lib/zserver-0.1.0/src/chat_handler.erl",
               "./lib/zserver-0.1.0/src/chatroom_manager.erl",
               "./lib/zserver-0.1.0/src/chatroom_server.erl",
               "./lib/zserver-0.1.0/src/event_handler.erl",
               "./lib/zserver-0.1.0/src/loop_handler.erl",
               "./lib/zserver-0.1.0/src/main_handler.erl",
               "./lib/zserver-0.1.0/src/rest_handler.erl",
               "./lib/zserver-0.1.0/src/ws_handler.erl",
               "./lib/zserver-0.1.0/src/zserver_app.erl",
               "./lib/zserver-0.1.0/src/zserver_sup.erl"]

~~~

不过实际中也遇到`sync_scanner`什么也不输出的情况，目前找到的办法是重新`make && make restart`一下。而且实际中也遇到第一次修改了代码只编译但不reload的情况，实在是不明白什么原因引起的...

正常的话修改了代码就会在log里看到提示，beam被自动热更新到app上了。

项目已经上传到[upbit/zserver:775bb68](https://github.com/upbit/zserver/tree/775bb686aee77659eb07d7796ccbc41b74762573)，注意clone后先make deps下载依赖库。
