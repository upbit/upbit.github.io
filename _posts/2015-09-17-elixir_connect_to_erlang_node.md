---
layout: post
title: Elixir - 连接Erlang nodes并调用其中的函数
description: "Elixir - link to Erlang nodes, and call remote functions by rpc"
category: elixir
comments: true
share: true
---

越来越觉得Elixir乃至[Phoenix](http://www.phoenixframework.org/)是代替Erlang神器，类ruby的语法和强大的release包管理等插件，确实节省了大量的开发时间。而且PhoenixFramework提供了一套较为完整的Web框架，使构建一个WebServer乃至资源更新/live_reload都做的井井有条。不过老服务依然是跑在Erlang节点上的，有些像cache等还必须与Erlang节点通信。于是这两天写了个rpc的GenServer封装，方便像调本地函数一样call远程节点(Erlang/Elixir)的函数。

## 使用iex连接erlang节点

其实使用iex连接erlang节点很简单，我们先来构造环境：

~~~bash
# 用erl启动一个api@192.168.8.1的节点，cookie为api_cookie (注意修改IP为你的服务器IP)
$ erl -name api@192.168.8.1 -setcookie api_cookie
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.0.2  (abort with ^G)
(api@192.168.8.1)1>

# 复习下erl怎么连接到上面的节点 (注意IP和cookie)
$ erl -hidden -name foo@127.0.0.1 -remsh api@192.168.8.1 -setcookie api_cookie
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.0.2  (abort with ^G)
(api@192.168.8.1)1>
~~~

接着换用iex连接Erlang节点，使用`iex --help`可以看到参数和erl稍有不同：

~~~bash
$ iex --hidden --name foo@127.0.0.1 --remsh api@192.168.8.1 --cookie api_cookie
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Could not find IEx on remote node api@192.168.8.1. Aborting...
~~~

如果没有错误，你会立刻得到这样的输出。大意是在远程Erlang的node上没有找到IEx（这是当然的，不过说明节点是可以连通的）

## 从iex手动连接Erlang节点

既然没有找到IEx，那直接用`Node.connect`连接总可以了吧，于是：

~~~bash
# 用iex启动一个节点，注意不加hidden以方便确认连接情况
$ iex --name "foo@127.0.0.1"
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (1.0.5) - press Ctrl+C to exit (type h() ENTER for help)
# 先ping一下目标节点，实际调用的是:net_adm.ping (注意节点名需要传atom，被这个坑了好久)
iex(foo@127.0.0.1)1> Node.ping(:"api@192.168.8.1")
:pong
# 设置cookie，注意Elixir的set_cookie只接受atom
iex(foo@127.0.0.1)2> Node.set_cookie(:api_cookie)
true

# 连接到集群，之后就可以用list确认节点列表
iex(foo@127.0.0.1)3> Node.connect(:"api@192.168.8.1")
true
iex(foo@127.0.0.1)4> Node.list
[:"api@192.168.8.1"]
~~~

此时erlang节点侧也可以用`nodes()`确认到iex的这个节点。

## 像调本地函数一样使用远程资源

确保可以连接后，就有各种办法可以互相调用了。这里选最简单的`:rpc.call/5`，对于调用本机Erlang节点资源来说足够了。先来看看rpc.call的说明：

~~~erlang
call(Node, Module, Function, Args, Timeout) -> Res | {badrpc, Reason}

Types:
    Node = node()
    Module = module()
    Function = atom()
    Args = [term()]
    Res = Reason = term()
    Timeout = timeout()

Evaluates apply(Module, Function, Args) on the node Node and returns the corresponding value Res, or {badrpc, Reason} if the call fails. Timeout is a timeout value in milliseconds. If the call times out, Reason is timeout.

If the reply arrives after the call times out, no message will contaminate the caller's message queue, since this function spawns off a middleman process to act as (a void) destination for such an orphan reply. This feature also makes this function more expensive than call/4 at the caller's end.
~~~

类似`apply(Module, Function, Args)`的语法，只不过在目标Node上执行该函数。

不过既然是elixir，我们可以写得更方便一点，比如隐含连接信息（这里使用场景是连接本地IP对应的api节点），并且提供宏来方便调用：

~~~elixir
defmodule ApiBridge do
  use ExActor.GenServer, export: :api

  # 导出rpc宏，可以像写本地调用一样调远程函数，例如：rpc :erlang.node()
  defmacro rpc(exp, timeout \\ 1000) do
    
    quote do call(unquote(module), unquote(function), unquote(args), unquote(timeout)) end
  end

  defstart start_link, gen_server_opts: [name: :api] do
    # 从config中取得连接信息
    set_cookie
    rpc_node = rpc_nodename('api')
    # 启动时尝试ping节点（这里还有待改进，比如检查cookie以及定期检查是否可以联通）
    case Node.ping(rpc_node) do
      :pang -> raise RuntimeError, message: "Failed to connect to node #{rpc_node}"
      :pong -> initial_state(%{:rpc_node => rpc_node})
    end
  end

  defcall call(module, function, args, timeout \\ 1000), state: %{:rpc_node => rpc_node} do
    :rpc.call(rpc_node, module, function, args, timeout)
    |> reply
  end

  defcast stop, do: stop_server(:normal)

  ## priv
  defp config(key) do
    Application.get_env(:server, ApiBridge)[key]
  end

  defp rpc_nodename(prefix) do
    # 如果有配置rpc_node则使用该IP地址，不然则使用本机eth0的IP
    str_ip = case to_string(config(:rpc_node)) do
      nil -> {:ok, [{:addr, tmp_ip}]} = :inet.ifget('eth0', [:addr]); :inet_parse.ntoa(tmp_ip)
      conf_ip -> conf_ip
    end
    String.to_atom(to_string(prefix) <> "@" <> str_ip)
  end
  defp set_cookie do
    # 要求在config/prod.secret.exs中提供cookie信息，不然抛出RuntimeError
    atom_cookie = case config(:cookie) do
      nil -> raise RuntimeError, message: "Couldn't get node cookie, please set cookie in config/#{Mix.env}.exs"
      cookie -> cookie |> to_string |> String.to_atom
    end
    Node.set_cookie(atom_cookie)
  end
end
~~~

接着在config里加入如下配置，并将`worker(ApiBridge, []),`加入到Supervisor的children列表中：

~~~elixir
config :server, ApiBridge,
  #rpc_node: "192.168.8.1",      # comment this line to get eth0 ip for default
  cookie: "api_cookie"
~~~

之后就可以在代码里这样调用Erlang api节点的函数了：

~~~elixir
import ApiBridge

# 在远程调用前直接加上rpc宏
rpc :remote_module_on_api_server.test_function(args)

# 指定rpc超时时间
rpc(:erlang.now(), 100)
~~~

这里仅演示了最简单的方法，rpc模块中还有很多函数可以调用，以提供更丰富的调用方式。通过这种方法，可以方便的与Erlang集群进行通信，确保新系统和老系统可以共同服役直到慢慢被新系统取代。
