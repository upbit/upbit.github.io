---
layout: post
title: 关于erlang:send的 nosuspend / noconnect 细节
description: "Something about erlang:send - nosuspend / noconnect"
category: erlang
comments: true
share: true
---

erlang的send确实比较方便，不过实际使用时可能会遇到[网络不通导致消息队列暴涨](http://blog.csdn.net/mycwq/article/details/42845385)的情况，并且关于nosuspend/noconnect的实现还有些细节需要深究。

## 起因

今天看代码遇到`erlang:send_nosuspend([noconnect])`的用法：

~~~erlang
broadcast(ServerName, Message) ->
  case pg2:get_members(ServerName) of
    E = {error, {no_such_group, ServerName}} -> throw(E);
    Pids ->
      lists:foreach(fun(P) ->
        case erlang:send_nosuspend(P, Message, [noconnect]) of
          false -> lager:debug("~p failed to send a message to remote pid ~p from ~p", [ServerName, P, node()]);
          true -> nothing_to_do
        end
      end, Pids)
  end.
~~~

这段很简单，不过和常规的broadcast不太一样，里面用的是`erlang:send_nosuspend([noconnect])`。于是跟着上面blog的讲解看了下erlang beam的实现部分。

### nosuspend

send_nosuspend实际是调用的send([nosuspend])：

~~~erlang
% erlang.erl
send_nosuspend(Pid, Msg, Opts) ->
  case erlang:send(Pid, Msg, [nosuspend|Opts]) of
    ok -> true;
    _  -> false
  end.
~~~

对应调用的时bif.c的`send_3()`函数。`send_3() -> remote_send()`最内层会调用一个叫`erts_dsig_prepare`的函数，用于准备发送异步的消息给其他节点。

~~~cpp
/*
 * erts_dsig_prepare() prepares a send of a distributed signal.
 * One of the values defined below are returned. If the returned
 * value is another than ERTS_DSIG_PREP_CONNECTED, the
 * distributed signal cannot be sent before apropriate actions
 * have been taken. Apropriate actions would typically be setting
 * up the connection.
 */

/* Connected; signal can be sent. */
#define ERTS_DSIG_PREP_CONNECTED	0
/* Not connected; connection needs to be set up. */
#define ERTS_DSIG_PREP_NOT_CONNECTED	1
/* Caller would be suspended on send operation. */
#define ERTS_DSIG_PREP_WOULD_SUSPEND	2
/* System not alive (distributed) */
#define ERTS_DSIG_PREP_NOT_ALIVE	3

ERTS_GLB_INLINE int erts_dsig_prepare(ErtsDSigData *,
				      DistEntry *,
				      Process *,
				      ErtsDSigPrepLock,
				      int);
~~~

`erts_dsig_prepare`在最后参数`no_suspend`非0时，会检查待发送队列是否超过限制，超过则返回`ERTS_DSIG_PREP_WOULD_SUSPEND -> SEND_YIELD`告知端口正忙。

在`send_3()`的返回值处理里，可以看到如果没有设置nosuspend则会立即`BIF_RET(am_nosuspend);`，即直接返回一个atom的nosuspend。nosuspend还有两种情况，一种是在无连接或远端节点不可用时`SEND_TRAP`（这个状态的细节在后面的noconnect讲解）；另一种则是可以发送消息的`ERTS_DSIG_PREP_CONNECTED`状态。

`ERTS_DSIG_PREP_CONNECTED`时会先用`erts_dsig_send_msg -> dsig_send`把消息放入发送队列，并返回0后等待`port_task`工作线程来处理。如果`erts_dsig_send_*`阻塞(`ERTS_DSIG_SEND_YIELD`)则会返回`SEND_YIELD_RETURN`，此时`send_3()`也是对外返回nosuspend。

结合上面blog中关于消息队列堆积的解释，可以知道使用nosuspend时，需要注意send会在繁忙时返回nosuspend；此时如果继续调用send则会造成消息的继续堆积，直到erlang检测到远端节点不可用为止。

### noconnect

在`erts_dsig_prepare`中还有2种情况，会导致Trap动作：`ERTS_DSIG_PREP_CONNECTED/ERTS_DSIG_PREP_NOT_ALIVE -> SEND_TRAP`

结合`send_3()`函数对`SEND_TRAP`的处理，noconnect在遇到节点无法连接的情况，会直接返回noconnect。否则会调用`BIF_TRAP3(dsend3_trap, p, to, msg, opts);`进入一个Trap操作，在下一次调度时调用dsend/3：

~~~erlang
dsend(Pid, Msg, Opts) when erlang:is_pid(Pid) ->
  case net_kernel:connect(erlang:node(Pid)) of
  	true -> erlang:send(Pid, Msg, Opts);
  	false -> ok
  end;
~~~

在dsend中会先阻塞的调用`net_kernel:connect`，如果远端节点无法连接则会一直等待到超时。

了解了这些细节，再来看`erlang:send_nosuspend(P, Message, [noconnect])`就比较好理解了：即相对send，遇到网络抖动或发送端口繁忙时将消息无条件放入发送队列，并且在进一步网络中断或无连接时放弃发送。
