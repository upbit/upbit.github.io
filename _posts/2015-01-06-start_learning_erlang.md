---
layout: post
title: 开始学习Erlang，顺带记录下之前几个问题的erl实现
description: "Start Learning Erlang"
category: study
comments: true
share: true
---

一直觉得Erlang的语法确实比较诡异，这个在看过Elixir后尤甚。折腾了一天环境和rebar后，发现在MacOS搭建Erlang开发环境也不是那么复杂，之后弄明白怎么引入模块就可以开始练手了。

## Erlang的开发环境搭建

MacOS上Homebrew里都有，直接安装即可：

~~~sh
$ brew update
$ brew install erlang rebar

# 当然顺带可以安装下Elixir，有个ier可以玩
$ brew install elixir
~~~

接着是IDE。习惯了Sublime Text，于是到[SublimErl](https://github.com/ostinelli/SublimErl)，不过作者好像没怎么更新了，而且不知道为什么无法build。后来换用[Sublime-Erlang](https://github.com/fjl/Sublime-Erlang)，虽然这个自动补全方面不如SublimErl，不过支持ST3。直接clone到Packages目录下，删掉自带的Erlang即可完成安装。

试了下build，貌似是调用的 `rebar compile`，暂时不明白rebar的玩法，没有任何输出... 还以为可以像python一样直接看到运行结果，看来还是少不了要开个终端。

## Erlang

### 快速排序实现

在看[Erlang并发编成 程序3.1](http://svn.liancheng.info/cpie-cn/trunk/.build/html/part-i/chapter-3.html#sort)时见到个qsort的实现：

~~~erlang
-module(sort).
-export([sort/1]).

sort([]) -> [];
sort([Pivot|Rest]) ->
  {Smaller, Bigger} = split(Pivot, Rest),
  lists:append(sort(Smaller), [Pivot|sort(Bigger)]).

split(Pivot, L) ->
  split(Pivot, L, [], []).

split(Pivot, [], Smaller, Bigger) ->
  {Smaller,Bigger};
split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
  split(Pivot, T, [H|Smaller], Bigger);
split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
  split(Pivot, T, Smaller, [H|Bigger]).
~~~

想起面试时提到的思路，于是自己实现了一遍。首先在当前目录下创建一个 sort1.erl，文件名需要和module里的名字一致，接着实现qsort：

~~~erlang
-module(sort1).
-export([qsort/1]).

qsort([]) -> [];
qsort(L) ->
  Pivot = hd(L),
  {Smaller, Bigger} = {
    lists:filter(fun(E) -> E<Pivot end, tl(L)),
    lists:filter(fun(E) -> E>=Pivot end, tl(L))
  },
  qsort(Smaller) ++ [Pivot] ++ qsort(Bigger).
~~~

第一行是在输入为 [] 时匹配，输出 []。第二行则是关键，先仿照Python版的qsort取得L的第一个元素作为 Pivot，接着用filter过滤出 Smaller, Bigger 两个数组，递归的调用 qsort() 继续排序，最后将结果 ++ 到返回中。

lists:filter() 的解释是在[这里](http://blog.csdn.net/zhangjingyangguang/article/details/7377787)看到的，当然官方文档里也有。用了filter方便很多，看来熟练使用lists里的函数也是很有必要的。

在erl中引入模块，测试结果如下：

~~~erlang
1> L = [2,5,6,4,3,1].
[2,5,6,4,3,1]
% 引入sort1.erl，当然也可以写成 c("sort1.erl").
2> c(sort1).
{ok,sort1}
% 调用sort1中导出的qsort/1
3> sort1:qsort(L).
[1,2,3,4,5,6]
~~~

### 矩阵转置

之前面试的一道题，用这两天学的erlang语法实现了一遍。不过也许是if语句用的不熟，判断list是否为[]都改成 `case length(L)>0 of` 了...

~~~erlang
-module(mt).

%% ====================================================================
%% API functions
%% ====================================================================
-export([transpose/1]).

%% 矩阵转置
transpose(L) -> transpose(L, [], [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================

transpose([], Lhead, Ltail, Lret) ->
  case length(Ltail) of
    % Ltail == [] 时，说明处理完毕，附加上最后的Lhead并返回Lret中的结果
    0 -> lists:append(Lret, [Lhead]);
    % Ltail还有数据，继续递归。这里将之前Lhead组成一个元素，加入Lret返回中
    _ -> transpose(Ltail, [], [], lists:append(Lret, [Lhead]))
  end;

transpose([H|T], Lhead, Ltail, Lret) ->
  case length(H) > 0 of
    true ->
      % 当H是list时，TT有可能是[]，此时append会导致多余的[]加入Ltail
      [TH|TT] = H,
      case length(TT) > 0 of
        % TT非[]，将TT加入Ltail等待后面递归
        true -> transpose(T, lists:append(Lhead, [TH]), lists:append(Ltail, [TT]), Lret);
        % TT==[]，此时直接丢弃TT的内容
        false -> transpose(T, lists:append(Lhead, [TH]), Ltail, Lret)
      end;
    % H不是list，直接附加到Lhead后面
    false -> transpose(T, lists:append(Lhead, [H]), Ltail, Lret)
  end.
~~~

运行结果：

~~~erlang
1> M = [[1,5,7,9],[2,6],[3,8],[4]].
[[1,5,7,9],[2,6],[3,8],[4]]
2> mt:transpose(M).
[[1,2,3,4],[5,6,8],[7],"\t"]      % "\t"是9，不知道怎么才能输出数值...
~~~

感觉对 TT 为[]的处理不太好，而且肯定不是最简单的写法。总之是实现了，后面等看懂了map等函数，再来想想二面里提到的方法。
