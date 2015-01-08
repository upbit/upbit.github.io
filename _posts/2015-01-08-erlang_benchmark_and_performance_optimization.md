---
layout: post
title: Erlang代码的性能测试与调优
description: "Erlang benchmark and performance optimization"
category: erlang
comments: true
share: true
---

今天逛GitHub发现一段[Erlang的性能测试代码](https://github.com/wasted/scala-vs-erlang/blob/master/erlang/client.erl)，于是把之前的矩阵转置程序测试了下。经过一系列调整得到如下结论：

1. 尽可能去掉 `length(L)>0` 这种函数调用，用 `L=/=[]` 代替；
2. **在可以用 `[H|T]` 代替 `++` 时，优先使用前一种写法** (虽然看起来很糟糕，不过性能上提升很大！)；
3. `lists:append()`替换成`++`，`lists:map()`改用自己实现，性能上几乎没有太多提升，可以直接使用；
4. 同样的判断条件下，`case`比`if`的性能略差，能用if时还是用if吧。

## 测试过程

### 0: 原始程序

~~~erlang
-module(mt).
-export([transpose0/1, benchmark/1]).

transpose0(L) -> transpose0(L, [], [], []).

transpose0([], Lhead, Ltail, Lret) ->
  case length(Ltail) of
    0 -> lists:append(Lret, [Lhead]);
    _ -> transpose0(Ltail, [], [], lists:append(Lret, [Lhead]))
  end;
transpose0([H|T], Lhead, Ltail, Lret) ->
  case length(H) > 0 of
    true ->
      [TH|TT] = H,
      case length(TT) > 0 of
        true -> transpose0(T, lists:append(Lhead, [TH]), lists:append(Ltail, [TT]), Lret);
        false -> transpose0(T, lists:append(Lhead, [TH]), Ltail, Lret)
      end;
    false -> transpose0(T, lists:append(Lhead, [H]), Ltail, Lret)
  end.

%% ====================================================================
%% Benchmark functions
%% ====================================================================

benchmark(Loop) ->
  M = [[1,3,5,7,9],[2,4,6,8,10],[3,6,9,12],[4,8,12,16]],
  Start=now(),
  lists:foreach(fun (_X) -> transpose0(M) end, lists:seq(1,Loop)),
  Finish=now(),
  print_results(Loop,Start,Finish).

print_results(Loop,Start,Finish) ->
  io:format("Test ~p took ~p seconds~n",[Loop, elapsedTime(Start,Finish)]),
  io:format("Throughput=~p per sec~n",[throughput(Loop,Start,Finish)]).

elapsedTime(Start,Finish) -> timer:now_diff(Finish, Start) / 1000000.

throughput(Size,Start,Finish) -> Size / elapsedTime(Start,Finish).
~~~

都按默认的矩阵M，计算100w次调用所消耗的时间：

~~~erlang
1> mt:benchmark(1000000).
Test 1000000 took 4.330299 seconds
Throughput=230930.93571598636 per sec
ok
2> mt:benchmark(1000000).
Test 1000000 took 4.140271 seconds
Throughput=241530.0834172449 per sec
ok
~~~

### 1: 去掉length()，换成if判断

看了下代码，估计是`case`的原因。稍微优化了下，改成`if`来判断：

~~~erlang
transpose1([], Lhead, Ltail, Lret) ->
  if
    Ltail =/= [] -> transpose1(Ltail, [], [], lists:append(Lret, [Lhead]));
    Ltail == [] -> lists:append(Lret, [Lhead])
  end;
transpose1([H|T], Lhead, Ltail, Lret) ->
  if
    H =/= [] ->	[TH|TT] = H,
    if
      TT =/= [] -> transpose1(T, lists:append(Lhead, [TH]), lists:append(Ltail, [TT]), Lret);
      TT == [] -> transpose1(T, lists:append(Lhead, [TH]), Ltail, Lret)
    end;
    H == [] -> transpose1(T, Lhead, Ltail, Lret)
  end.
~~~

~~~erlang
3> mt:benchmark(1000000).
Test 1000000 took 2.600908 seconds
Throughput=384481.11198089283 per sec
ok
4> mt:benchmark(1000000).
Test 1000000 took 2.654778 seconds
Throughput=376679.33062576235 per sec
ok
~~~

将`case`语句换成`if`后，100w次调用的耗时大幅减少了，估计是`length()`开销太大。

### 2: 用++替换lists:append()

既然是库函数消耗性能，继续尝试将`lists:append()`换成`++`的方式：

~~~erlang
transpose1([], Lhead, Ltail, Lret) ->
  if
    Ltail =/= [] -> transpose1(Ltail, [], [], Lret ++ [Lhead]);
    Ltail == [] -> Lret ++ [Lhead]
  end;
transpose1([H|T], Lhead, Ltail, Lret) ->
  if
    H =/= [] -> [TH|TT] = H,
    if
      TT =/= [] -> transpose1(T, Lhead ++ [TH], Ltail ++ [TT], Lret);
      TT == [] -> transpose1(T, Lhead ++ [TH], Ltail, Lret)
    end;
    H == [] -> transpose1(T, Lhead, Ltail, Lret)
  end.
~~~

~~~erlang
5> mt:benchmark(1000000).
Test 1000000 took 2.162367 seconds
Throughput=462456.1880568839 per sec
ok
6> mt:benchmark(1000000).
Test 1000000 took 2.158168 seconds
Throughput=463355.9574602163 per sec
ok
~~~

运行结果证实了这个猜想，使用`++`的语法比调用`lists:append()`要快。网上也看到有[文章](http://blog.csdn.net/zhongruixian/article/details/9417201)说lists:append()的底层就是调用的`++`。

### 3: 极限优化，用[H|T]替换++

不过既然想到`++`的语法，那么自然会想到用`[H|T]`的方式来附加数据。只不过测试中发现点小问题：

~~~erlang
transpose3([], Lhead, Ltail, Lret) ->
  if
    Ltail =/= [] -> transpose3(Ltail, [], [], [Lhead|Lret]);
    Ltail == [] -> [Lhead|Lret]
  end;
transpose3([H|T], Lhead, Ltail, Lret) ->
  if
    H =/= [] -> [TH|TT] = H,
      if
        TT =/= [] -> transpose3(T, [TH|Lhead], [TT|Ltail], Lret);
        TT == [] -> transpose3(T, [TH|Lhead], Ltail, Lret)
      end;
    H == [] -> transpose3(T, Lhead, Ltail, Lret)
  end.
~~~

测试发现，虽然性能是极大幅度提升了，但输出不对。`[H|T]`是insert(0)而不是想要的append()，输出的内容都逆序了...

~~~erlang
7> mt:benchmark(1000000).
Test 1000000 took 0.650597 seconds
Throughput=1537049.8173216293 per sec
ok
8> M1 = [[1,3,5,7],[2,4],[6],"\b"].
[[1,3,5,7],[2,4],[6],"\b"]
9> mt:transpose3(M1).
[[7],[5],[3,4],[8,6,2,1]]
~~~

这里自然想到用`lists:reverse()`在外面逆序一次，不过因为里面元素也需要逆序，还需要map下：

~~~erlang
%transpose3(L) -> transpose3(L, [], [], []).
transpose3(L) ->
  lists:reverse(lists:map(fun(E) -> lists:reverse(E) end, transpose3(L, [], [], []))).
~~~

~~~erlang
10> mt:benchmark(1000000).
Test 1000000 took 1.245538 seconds
Throughput=802865.9101528818 per sec
ok
11> mt:benchmark(1000000).
Test 1000000 took 1.211493 seconds
Throughput=825427.7985923155 per sec
ok
~~~

测试发现性能损失一倍，不过结果算是正确了。

### 4: 去掉[H|T]中的lists:map()

不过想想，lists:map()也是库调用，何不干脆在内部附加逆序操作呢：

~~~erlang
transpose3(L) -> transpose3(L, [], [], []).

transpose3([], Lhead, Ltail, Lret) ->
  %io:format("Head:~p, Tail:~p~n", [Lhead, Ltail]),
  if
    Ltail =/= [] -> transpose3(lists:reverse(Ltail), [], [], [lists:reverse(Lhead)|Lret]);
    Ltail == [] -> lists:reverse([Lhead|Lret])
  end;
transpose3([H|T], Lhead, Ltail, Lret) ->
  %io:format("[~p | ~p]: ~p~n", [H, T, Lret]),
  if
    H =/= [] -> [TH|TT] = H,
      if
        TT =/= [] -> transpose3(T, [TH|Lhead], [TT|Ltail], Lret);
        TT == [] -> transpose3(T, [TH|Lhead], Ltail, Lret)
      end;
    H == [] -> transpose3(T, Lhead, Ltail, Lret)
  end.
~~~

在 transpose3([], Lhead, Ltail, Lret) 中，对每次取完的结果逆序一遍，包括Lhead和Ltail；最后返回时也将外层Lret逆序输出，结果和上面map版的相同。测试下性能：

~~~erlang
12> mt:benchmark(1000000).
Test 1000000 took 1.138952 seconds
Throughput=878000.1264320182 per sec
ok
13> mt:benchmark(1000000).
Test 1000000 took 1.104822 seconds
Throughput=905123.1782133231 per sec
ok
~~~

虽然提升不明显，不过确实优于lists:map()的实现，而且更易懂一些。至于去掉`lists:reverse()`，暂时还没找到更好的办法，如果`[Lhead|TH]`可以在List的尾部追加TH该多好。

## 结论

1. 尽可能去掉 `length(L)>0` 这种函数调用，用 `L=/=[]` 代替；
2. **在可以用 `[H|T]` 代替 `++` 时，优先使用前一种写法** (虽然看起来很糟糕，不过性能上提升很大！)；
3. `lists:append()`替换成`++`，`lists:map()`改用自己实现，性能上几乎没有太多提升，可以直接使用；
4. 同样的判断条件下，`case`比`if`的性能略差，能用if时还是用if吧。

如果还有更高效的思路，欢迎讨论:)
