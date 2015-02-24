---
layout: post
title: 使用cowboy和relx搭建Web服务01 - 框架搭建
description: "Build Web service with cowboy and relx 01"
category: erlang
comments: true
share: true
---

好久没更新blog了，最主要的还是因为懒... 不过每次看到Chrome收藏夹里[坚强2002的Erlang分类](http://www.cnblogs.com/me-sa/category/304370.html)，都为自己还没写上1/10的章节而汗颜。于是又把之前学习cowboy的内容给整理了下。

## cowboy

cowboy是个很流行的webserver，并且性能比老牌的mochiweb[要强不少](http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/)。不过照着官方User Guide写了个静态文件的例子，居然无法正确访问。。。估计是部分文档没有更新，建议参考[examples](https://github.com/ninenines/cowboy/tree/master/examples)直接获取例子。

## erlang.mk

cowboy官方推荐用[erlang.mk](https://github.com/ninenines/erlang.mk)和[relx](https://github.com/erlware/relx)来管理工程。下面我们一步步来创建新的Web服务器框架：

~~~sh
# 首先创建目录 (注意: 目录名就是后面app的名字)
mkdir zserver
cd zserver/

# 下载最新的erlang.mk
wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

# 生成OTP应用的模板与Makefile
make -f erlang.mk bootstrap bootstrap-rel
~~~

如果是用的以前的erlang.mk，可以使用如下命令更新：

~~~sh
$ make erlang-mk
git clone https://github.com/ninenines/erlang.mk .erlang.mk.build
Cloning into '.erlang.mk.build'...
~~~

## Makefile

接着修改Makefile，修改成下面这样（注意最下面4行要用TAB而不是空格）：

~~~makefile
PROJECT = zserver
DEPS = cowboy lager jsx
include erlang.mk

ERLC_OPTS = +debug_info +'{parse_transform,lager_transform}'

RELX_EXPORTS = start foreground stop restart reboot ping console console_clean attach escript
$(RELX_EXPORTS)::
	./_rel/$(PROJECT)_release/bin/$(PROJECT)_release $@
tail::
	tail -n 120 $(shell ls -1 ./_rel/$(PROJECT)_release/log/erlang.log* | tail -n 1)
~~~

1. `DEPS = cowboy lager jsx`增加对cowboy,lager,jsx的依赖，lager是日志库，jsx则用于处理json内容
2. `ERLC_OPTS = +debug_info +'{parse_transform,lager_transform}'`是增加debug_info和lager_transform的支持
3. `RELX_EXPORTS = start foreground stop restart reboot ping console console_clean attach escript`是导出rel脚本中的控制命令，这样就可以用`make {start|stop}`来控制app的启停。实际的控制脚本在`./_rel/zserver_release/bin/zserver_release`
4. `tail`用于查看app最新的日志，当然也可以将`tail -n 120`改成`tail -f`来持续输出log内容

配置好DEPS，运行`make deps`就会从github上clone对应的依赖库。实际这个命令是通过`.erlang.mk.packages.v2`[文件中的配置](https://github.com/ninenines/erlang.mk/blob/master/packages.v1.tsv)来运行的。不知道可不可以像Homebrew那样tap到自己的repo，这样方便配置一些第三方依赖库。

## zserver.app.src

获取完依赖库，接着在app.src中applications部分，加上对cowboy等库的依赖：

~~~erlang
{application, zserver, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		cowboy,
		jsx,
		lager
	]},
	{mod, {zserver_app, []}},
	{env, []}
]}.
~~~

## zserver_app.erl

接着初始化cowboy。首先配置cowboy的路由表。在start/2中加入`cowboy_router:compile`：

~~~erlang
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, zserver, "index.html"}}
		]}
	]),
~~~

cowboy_static是提供一个静态内容，这里将"/"映射到zserver的priv目录下的index.html。

### priv/

这里严重缺乏文档，摸索了好久才明白，`priv`是指`_rel/zserver_release/lib/zserver-0.1.0/`下的priv目录，其中的内容会在make时自动与根目录的priv同步。所以回到zserver/的根目录，`mkdir priv`后在其中创建一个index.html：

~~~html
<html>
  <head>
    <title>Sample "Hello, World" Application</title>
  </head>
  <body bgcolor=white>
    <table border="0" cellpadding="10">
      <tr>
        <td>
          <h1>Sample "Hello, World" Application</h1>
        </td>
      </tr>
      <tr>
        <td>
          <center>
            <a href="/m">
              <img src="/static/image/github.png" style="width: 33%; height: 33%"/>
            </a>
          </center>
        </td>
      </tr>
    </table>
  </body>
</html>
~~~

make后就可以在_rel的对应priv下看到这个index.html了。当然不可能只提供这一个静态文件，下面加上对`/static/image/github.png`的处理：

~~~erlang
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, zserver, "index.html"}},
			{"/static/[...]", cowboy_static, {priv_dir, zserver, "static"}}
		]}
	]),
~~~

`"/static/[...]"`表示static后面的所有请求都进入这个处理，而之后priv_dir指定提供一个目录而不仅仅是一个文件，最后的参数"static"指向priv/static目录。

因此最终priv下的目录结构如下：

~~~
priv/
├── index.html
└── static
    ├── css
    ├── image
    │   └── github.png
    └── js
~~~

这样对/static/image/github.png的访问就被顺利映射到/priv/static/image/github.png了。

## custom handler

除了提供静态文件的访问，当然最重要的是动态处理请求。在Dispatch中加入`{"/main", main_handler, []}`，将"/main"映射到`main_handler.erl`中继续处理。接着像例子中一样启动cowboy server，最终`zserver_app.erl`看起来像这样：

~~~erlang
-module(zserver_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/main", main_handler, []},

			%% static handlers
			{"/", cowboy_static, {priv_file, zserver, "index.html"}},
			{"/static/[...]", cowboy_static, {priv_dir, zserver, "static"}}
		]}
	]),
	CowboyOptions = [
		{env, [{dispatch, Dispatch}]},
		{compress, true}
	],
	cowboy:start_http(http_listener, 100, [{port, 8080}], CowboyOptions),
	zserver_sup:start_link().

stop(_State) ->
	ok.
~~~

## main_handler.erl

使用`cowboy_http`模板创建`main_handler.erl`：`make new t=cowboy_http n=main_handler`

接着修改生成的`src/main_handler.erl`为如下内容：

~~~erlang
-module(main_handler).
-behaviour(cowboy_http_handler).

%% cowboy_http_handler callbacks
-export([
	init/3,
	handle/2,
	terminate/3
]).

-record(state, {
}).

%% ===================================================================
%% cowboy_http_handler callbacks
%% ===================================================================

init(_Type, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req0, State = #state{}) ->
	Body = jsx:encode(#{
		<<"messages">> => [
			hello, world
		],
		<<"timestamp">> => timestamp()
	}),
	{ok, Req1} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>},
		{<<"connection">>, <<"close">>}
	], << Body/binary, <<"\n\n">>/binary >>, Req0),
	{ok, Req1, State}.

terminate(_Reason, _Req, #state{}) ->
	ok.

%% ===================================================================
%% Internal
%% ===================================================================

timestamp() ->
	{M, S, _} = os:timestamp(),  
	M * 1000000 + S.
~~~

最常见的是返回json数据，所以这里演示下如何用jsx封装json返回一个atom数组(会转成字符串)和时间戳。注意jsx:encode是不带换行符的，建议返回Body前用`<< Body/binary, <<"\n\n">>/binary >>`加2个回车在末尾。

附上完整的源码包：[`cowboy_zserver_src1.tar.gz`]({{ site.url }}/assets/download/cowboy_zserver_src1.tar.gz)

## 运行

make后就可以用`make start`启动app了。如果没有错误，就可以在[http://localhost:8080](http://localhost:8080/)看到index.html的内容：

![index hello]({{ site.url }}/images/201502/zserver_index.png)

点击url或直接访问/main会返回main_handler中的json数据：

![json response]({{ site.url }}/images/201502/zserver_json_response.png)

## 关于gzip

测试发现，就算初始化时指定了`{compress, true}`，如果返回数据太少也会不启用gzip。随着往返回Body里增加内容，`curl -i --compressed http://localhost:8080/main`就能正确看到gzip的结果了。

ps:越接触OTP越是一头雾水，回头看看以前觉得是天书的《Erlang/OTP并发编程实战》发现对现阶段是大有益处。目前还是潜下心来啃完这本再说...
