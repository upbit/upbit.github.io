---
layout: post
title: Erlang程序设计(第二版)作业笔记3 - lists
description: "Programming Erlang 2nd note 03"
category: erlang
comments: true
share: true
---

第九章后面有个阅读公共库[lists.erl](https://github.com/erlang/otp/blob/maint/lib/stdlib/src/lists.erl)源码的习题，看了才发现erlang居然能写得如此精妙！比如prefix，判断某个list是否为另一个list的前缀：

~~~erlang
%% prefix(Prefix, List) -> (true | false)

-spec prefix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) when is_list(List) -> true;
prefix([_|_], List) when is_list(List) -> false.
~~~

第一个模式`prefix([X|PreTail], [X|Tail])`，注意其中的`[X|...], [X|...]`，这个模式就是检查X是否相同，相同则将Tail继续递归；第二个模式检查PreTail为空时，说明已经全部匹配，返回true；最后的模式当PreList任意元素不匹配时，返回false。

suffix则是另一个思路。之前有定义nthtail用于跳过前N个元素取末尾，因此可以直接算出两个list的长度差，然后从插值开始的偏移取末尾，判断是否与Suffix相等：

~~~erlang
%% suffix(Suffix, List) -> (true | false)

-spec suffix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

suffix(Suffix, List) ->
    Delta = length(List) - length(Suffix),
    Delta >= 0 andalso nthtail(Delta, List) =:= Suffix.
~~~

### seq

seq/2是个很实用的函数，自己实现也不复杂，不过很少能想到项lists里这样加速的：

~~~erlang
seq(First, Last)
    when is_integer(First), is_integer(Last), First-1 =< Last ->
    seq_loop(Last-First+1, Last, []).

seq_loop(N, X, L) when N >= 4 ->
     seq_loop(N-4, X-4, [X-3,X-2,X-1,X|L]);
seq_loop(N, X, L) when N >= 2 ->
     seq_loop(N-2, X-2, [X-1,X|L]);
seq_loop(1, X, L) ->
     [X|L];
seq_loop(0, _, L) ->
     L.
~~~

seq_loop中会一次减4去生成，最后处理输入L长度>=2，1，0的情况。seq/3带步长并且可以是负数，有兴趣可以自己写个看看。

### delete

delete是个匹配规则，第一个Item匹配后返回Rest，否则进入第二个规则继续：

~~~erlang
%% delete(Item, List) -> List'
%%  Delete the first occurrence of Item from the list L.

-spec delete(Elem, List1) -> List2 when
      Elem :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) ->
    [H|delete(Item, Rest)];
delete(_, []) -> [].
~~~

### unzip

unzip没有什么特殊的，只不过注意list因为是指向头部的单向链表，所以为了性能是先把结果附加在头部，然后用reverse逆序：

~~~erlang
%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn]}, for a list [{X0, Y0},
%% {X1, Y1}, ..., {Xn, Yn}].

-spec unzip(List1) -> {List2, List3} when
      List1 :: [{A, B}],
      List2 :: [A],
      List3 :: [B],
      A :: term(),
      B :: term().

unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.
~~~

前面的代码还好，不过看到sort就直接昏了，暂时休战...
