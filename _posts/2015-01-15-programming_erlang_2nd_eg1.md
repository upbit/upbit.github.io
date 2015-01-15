---
layout: post
title: Erlang程序设计(第二版)作业笔记1
description: "Programming Erlang 2nd note 01"
category: erlang
comments: true
share: true
---

最近看Erlang程序设计(第二版)，做完书上几个作业感觉果然对内容熟悉很多，特此记录下写法，以免忘记。

## CH4

(2) 实现自己的 my_tuple_to_list(): 要点是用 element() 访问tuple，用lists:seq()生成列表推导的索引源；
(5/6) 实现even/1, odd/1, filter/2；
(7) 用2种方法实现split()，将Evens/Odds分堆；

~~~erlang
-module(ch4).
-compile([export_all]).

odd_and_evens_acc([], Odds, Evens) ->
	{Odds, Evens};
odd_and_evens_acc([H|T], Odds, Evens) ->
	case (H rem 2) of
		1 -> odd_and_evens_acc(T, [H|Odds], Evens);
		0 -> odd_and_evens_acc(T, Odds, [H|Evens])
	end.

my_tuple_to_list({}) ->
	[];
my_tuple_to_list(T) ->
	[ element(I, T) || I <- lists:seq(1, tuple_size(T)) ].

even(N) -> N rem 2 == 1.
odd(N) -> N rem 2 == 0.

filter(F, L) ->
	[ X || X <- L, F(X) == true ].

split1([]) ->
	[];
split1(L) ->
	Evens = filter(fun even/1, L),
	Odds = filter(fun odd/1, L),
	{Evens, Odds}.

split2(L) ->
	{Evens, Odds} = split_acc(L, [], []),
	{lists:reverse(Evens), lists:reverse(Odds)}.
split_acc([], Evens, Odds) ->
	{Evens, Odds};
split_acc([H|T], Evens, Odds) ->
	case H rem 2 of
		1 -> split_acc(T, [H|Evens], Odds);
		0 -> split_acc(T, Evens, [H|Odds])
	end.
~~~

## CH5

(2) 实现 map_search_pred()，返回第一个使Pred()==true的 {Key, Value}

最开始以为很容易，不过看过作者Joe的帖子 [Some Performance Measurements On Maps](http://joearms.github.io/2015/01/08/Some_Performance-Measurements-On-Maps.html) 后发现，这样实现在list很大时性能很差(需要全部遍历一遍)。看完学到种新的办法，throw结果然后在外面catch，这样就可以避免遍历全部list了，对比见 test()：

~~~erlang
-module(ch5).
-compile([export_all]).

make(0, M, _) -> M;
make(K, M, I) -> make(K-1,maps:put(I,I+1,M),I+2).

% find first key for Pred(Key, Value) = ture
map_search_pred(Map, Pred) ->
	[H|_] = [ {Key, Value} || {Key, Value} <- maps:to_list(Map), Pred(Key, Value) =:= true ],
	H.

catch_map_search_pred(Map, Pred) ->
	(catch map_search_pred(Map, Pred)).

test() ->
	M = make(20000, #{}, 1),
	Fs = fun(K,_) -> K=:=1999 end,
	Ft = fun(K,V) when K=:=1999 -> throw({K,V}); (_,_) -> false end,
	{
	 timer:tc(ch5, map_search_pred, [M, Fs]),
	 timer:tc(ch5, catch_map_search_pred, [M, Ft]),
	 null
	}.
~~~

另外map的语法还有个坑，62页的模式匹配在 Erlang 17 里，`2> #{ bron => B } = Henry8.`要改成`2> #{ bron := B } = Henry8.`才能通过(感谢Adam指点)。另外那个统计字符的例子实际无法运行，作者说：

~~~
#52256: what's up with count_characters/3?
	I'm guessing
		count_characters(T, #{ H => 1 }, X));
	should be
		count_characters(T, X#{ H => 1 });

Joe Armstrong says: This is problematic. Keys in maps used outside the map are currently illegal
  but I hope this situation will change in a future not too distant release of Erlang.
  At this stage I don't want to change the text - even though it is incorrect.
~~~

好吧，那等以后版本再来玩玩。

## CH7

(1) 逆转二进制型里的字节序

这个问题其实没那么容易，首先书上没说明怎么拼接两个二进制型。首先是拆分：`<< H,T/binary >>`，接着将H拼接到Acc的头部 `<< <<H>>/binary, Ret/binary >>`，注意/binary：

~~~erlang
-module(ch7).
-compile([export_all]).

binary_reverse1(B) ->
	binary_reverse_acc1(B, <<>>).
binary_reverse_acc1(<<>>, Ret) ->
	Ret;
binary_reverse_acc1(<< H,T/binary >>, Ret) ->
	binary_reverse_acc1(T, << <<H>>/binary, Ret/binary >>).

foldl(_, _, <<>>) ->
	<<>>;
foldl(F, Acc, B) when is_binary(B) ->
	<< H:8, T/binary >> = B,
	Ret = foldl(F, F(H, Acc), T),
	<< << H >>/binary, Ret/binary >>.

foldr(_, _, <<>>) ->
	<<>>;
foldr(F, Acc, B) when is_binary(B) ->
	<< H:8, T/binary >> = B,
	Ret = foldr(F, F(H, Acc), T),
	<< Ret/binary, << H >>/binary >>.

% match << Len/2, Data/Len >>
% <<Len:16, Bin1/binary>>
% <<Str:Len/binary-unit:8, Rest/binary>>
~~~

另外按Adam所说，尝试实现了二进制版的foldl/foldr，有fold就可以直接用单行 Acc 累加了。

最后在[Erlang二进制数据处理](http://www.cnblogs.com/me-sa/archive/2011/12/25/erlang0024.html)中常用的match Len/Data的方法，写法上注意`Str:Len/binary-unit:8`就行，剩余部分放到Rest里。
