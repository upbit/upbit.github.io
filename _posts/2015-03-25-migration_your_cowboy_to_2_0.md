---
layout: post
title: 升级Cowboy代码到2.0.0-pre.1的那些曲折事
description: "Migration your Cowboy to 2.0.0-pre.1"
category: erlang
comments: true
share: true
---

之前不小心敲了distclean折腾的死去活来，等重新clone cowboy时发现relx默认的版本是1.0.0，难怪cowboy/master中的examples都无法正确运行。今天抽空跑了下示例，果然之前会失败的程序都正常了。不过2.0的变动挺大的，需要改不少东西。

总结下区别吧，从1.0.0到2.0.0-pre.1主要有下面几个变化：

### init/2

init函数从`init(Type, Req, Opts)`变成了`init(Req, Opts)`，并且`cowboy_req:reply`的返回值从{ok, Req}变成了Req，所以main_handler变成了这样：

~~~erlang
-export([init/2]).

init(Req, Opts) ->
	Body = jsx:encode(#{
		<<"messages">> => [
			hello, world
		]
	}),
	Req1 = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>},
		{<<"connection">>, <<"close">>}
	], << Body/binary, <<"\n">>/binary >>, Req),
	{ok, Req1, Opts}.
~~~

没有了handle(Req, State)的处理部分，普通的处理直接在init/2里返回{ok, Req, Opts}就结束了。

### cowboy_rest

REST handler不再需要`{upgrade, protocol, cowboy_rest}`：

~~~erlang
init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.
~~~

改成了下面这样：

~~~erlang
init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.
~~~

### cowboy_websocket

类似websocket也是在init/2里返回`{cowboy_websocket, Req, Opts}`即可。

另外去掉了`websocket_init/3`，从代码上推测Opts即是原来的#state{}参数，其他没什么区别。

### eventsource

看了下例子，好像没有了那些behaviour。从eventsource看维持长连接用的是`{cowboy_loop, Req, undefined, 5000, hibernate}`。新改了个chat_handler如下：

~~~erlang
-module(chat_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, info/3]).
-export([handle_post/2]).

init(Req, Opts) ->
	case cowboy_req:method(Req) of
		<<"POST">> ->
			{cowboy_rest, Req, Opts};
		<<"GET">> ->
			random:seed(erlang:now()),
			Req1 = chunk_start(Req),
			ok = send_event(Req1, info, <<"(´・ω・`) I am ready."/utf8>>),
			{cowboy_loop, Req1, Opts, hibernate}
	end.

%% only allowed post for REST
allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.
content_types_accepted(Req, State) ->
	{[{<<"application/x-www-form-urlencoded">>, handle_post}], Req, State}.

%%
info({message, Message}, Req, State) ->
	ok = send_message(Req, Message),
	{ok, Req, State, hibernate}.

%% POST

handle_post(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req, [{length, 4096}, {read_length, 4096}, {read_timeout, 3000}]),
	#{type := Type, data := Data} = cowboy_req:match_body_qs([
		{type, fun erlang:is_binary/1, <<"message">>},
		data
	], Body),
	lager:debug("type ~p data ~p", [Type, Data]),
	{true, Req1, State}.


%% Internal functions - chunk

%% @doc Send event-stream header to client
chunk_start(Req) ->
	Headers = [
		{<<"content-type">>, <<"text/event-stream">>},
		{<<"connection">>, <<"keep-alive">>}
	],
	cowboy_req:chunked_reply(200, Headers, Req).

send_message(Req, Data) ->
	send_event(Req, message, Data).

-spec send_event(term(), atom() | list(), binary()) -> ok.
send_event(Req, Event, Data) when is_atom(Event) ->
	send_event(Req, atom_to_list(Event), Data);
send_event(Req, Event, Data) when is_list(Event), is_binary(Data) ->
	EventBinary = binary:list_to_bin(["event: ", Event, "\n"]),
	IdBinary = binary:list_to_bin(["id: ", gen_timestamp_id(), "\n"]),
	Response = <<
		EventBinary/binary, IdBinary/binary,
		<<"data: ">>/binary, Data/binary, <<"\n\n">>/binary
	>>,
	cowboy_req:chunk(Response, Req).


%% Internal functions - utils

notify_all(Message) ->
	lists:foreach(
		fun(Listener) ->
			lager:debug("notify ~p: ~p", [Listener, Message]),
			Listener ! {message, Message}
		end, pg2:get_members(notify_group)).

gen_timestamp_id() ->
	{M, S, U} = erlang:now(),  
	lists:concat([M * 1000000 + S, ".", U]).
~~~

针对eventsource做了些封装，比如推送不同类型的event。注意chunk写utf8字符串时需要加/utf8，并且用binary传参。

另外：`cowboy_req:match_body_qs`是我自己增加的函数。cowboy 2.0里提供了一个方便的`match_qs`函数，可以返回一个map来match请求参数。但这个函数只能对qs里的内容做出解析，需要在`cowboy_req`里增加如下函数：

~~~erlang
-export([match_body_qs/2]).

-spec match_body_qs(cowboy:fields(), binary()) -> map().
match_body_qs(Fields, Body) ->
	filter(Fields, kvlist_to_map(Fields, cow_qs:parse_qs(Body))).
~~~

这样就可以欢快的处理body的内容了。
