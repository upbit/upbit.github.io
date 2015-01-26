---
layout: post
title: Erlang程序设计(第二版)作业笔记2
description: "Programming Erlang 2nd note 02"
category: erlang
comments: true
share: true
---

更新下本周的习题答案，涉及dict的使用和ETS。因为代码中涉及到ETS，提前看了19章的内容。另外ets:fun2ms使用时最大的教训，是`-include_lib("stdlib/include/ms_transform.hrl").`，为了`parse_transform`问题卡了2天...

## CH8

第八章这个习题比较有意思，找出已加载模块的导出函数中，名字出现最多的函数(直接想必然是module_info了，不过第二多的是什么确实需要写代码来查看)。另一个问题是输出没有歧义的函数，
即只在一个模块中出现过的函数名。最开始理解错了，进而衍生出一个更复杂的问题：**返回模块中只有1种定义的函数**

~~~erlang
-module(dt).
-compile([export_all]).

get_loaded_module_exports() ->
	Mods = [ M || {M,_} <- code:all_loaded() ],
	List = lists:map(fun(Mod) -> [ {Mod,F,A} || {F,A} <- Mod:module_info('exports') ] end, Mods),
	lists:concat(List).

% 返回所有模块中出现次数最多的函数(单模块内按函数名去重)
most_exported_function() ->
	% 先按 {模块,函数名} 去重，返回 函数名 组成的list
	UFuncList = lists:usort([ {M,F} || {M,F,_A} <- get_loaded_module_exports() ]),
	% 统计函数名F在各模块中的出现次数
	CountFuncs = dict:to_list(most_exported_acc(UFuncList, dict:new())),
	% 按Count排序并返回Head
	[H|_T] = lists:sort(fun({_F1,C1}, {_F2,C2}) -> C1 > C2 end, CountFuncs),
	H.

% 也可以用no_ambiguity_functions中的 lists:foldl 写法代替
most_exported_acc([], DictAcc) ->
	DictAcc;
most_exported_acc([{_M,F}|T], DictAcc) ->
	most_exported_acc(T, dict:update(F, fun(C) -> C+1 end, 1, DictAcc)).

% 返回没有歧义(只在一个函数里出现)的函数列表
no_ambiguity_functions() ->
	UFuncList = lists:usort([ {M,F} || {M,F,_A} <- get_loaded_module_exports() ]),
	CountFuncs = dict:to_list(lists:foldl(fun({_M,F}, Dict) -> dict:update(F, fun(C) -> C+1 end, 1, Dict) end, dict:new(), UFuncList)),
	[ F || {F,C} <- CountFuncs, C=:=1 ].
~~~

## CH19

ETS的使用作业，将系统目录下lib的导出函数，写入ETS中：

~~~erlang
-module(ch19).
-compile([export_all]).

-define(ETS_TABLE_NAME, ch19_table).
% needed by ets:fun2ms: http://www.erlang.org/doc/man/ets.html#fun2ms-1
-include_lib("stdlib/include/ms_transform.hrl").

get_module_lists() ->
	{ok, Mod_dirs} = file:list_dir(code:lib_dir()),
	% 注意这里将字符串转成atom，用tokens去掉后面 -x.y.z 的版本号
	[ list_to_atom(hd(string:tokens(X, "-"))) || X <- Mod_dirs ].

get_module_exports(Mod) ->
	% 如果函数参数个数不确定，可以换成：erlang:apply(Mod, module_info, ['exports'])
	case catch [ { {Mod,F,A}, 0 } || {F,A} <- Mod:module_info('exports'), F=/=module_info ] of
		{'EXIT', _} -> [];
		X -> X
	end.

init_ets() ->
	Exports = [ get_module_exports(X) || X <- get_module_lists() ],
	% 创建共享的有名set
	ets:new(?ETS_TABLE_NAME, [public, named_table, set]),
	lists:map(fun(L) -> ets:insert(?ETS_TABLE_NAME, L) end, Exports),
	ok.

% 以match方式输出ETS内容
match_module_exports(Mod) ->
	% lists:flatten(io_lib:format("~p~p", [atom1, atom2])) 用于将两个atom拼接为字符串
	[ lists:flatten(io_lib:format("~p/~p", [F, A])) || [F,A] <- ets:match(?ETS_TABLE_NAME, { {Mod,'$1','$2'}, '_' })].

% 更快的select方式。遇到 parse_transform 错误时，注意引用ms_transform.hrl
select_module_exports(Mod) ->
	Filter = ets:fun2ms(fun({ {M,F,A}, _ }) when M=:=Mod -> [F,A] end),
	Result = ets:select(?ETS_TABLE_NAME, Filter),
	[ lists:flatten(io_lib:format("~p/~p", [F, A])) || [F,A] <- Result ].
~~~

主要学到了以下几点：

1. ets:fun2me的函数，需要引用ms_transform.hrl（不然会报parse_transform错误）；
2. ETS的key可以是tuple，最初我还在找拼接/拆分字符串的方法...；
3. 多个atom拼接成字符串，可以用`lists:flatten(io_lib:format("~p", [...]))`；

另外还有两个**遗留问题**：

**一是当`Mod:module_info/1`函数不存在时，不用catch怎么处理？**听Adam说server代码里除了HTTPServer最外层会框一层try/catch，基本不会使用异常处理（因为和erlang的"任其崩溃"原则相违背？感觉像以前听人讲goto）

[2015-01-26补充] 今天看代码发现种写法，可以分辨模块中某个函数是否有导出(不能是module_info)：`[ Module:foobar() || {Module, _} <- Config, lists:member({foobar, 0}, Module:module_info(exports))]`。使用lists:member/2过滤没有foobar函数的模块，最后用生成的列表调用foobar()初始化。这个写法果然很精妙！

**二是如何确保ETS表不丢失？**Adam提供的方案是由sup进程持有ETS，worker只通过名字来访问它。可能还没看erlang的进程模型/OTP，还是觉得对Erlang的编程思路不太理解。为什么不能是无中心的(抢占式模型)？worker进程发现没有ETS就创建新的，然后再worker死亡时交给下一个worker持有，这样就不会出现因sup这样单点进程挂掉而引起的灾难了。
