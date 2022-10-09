---
layout: post
title: inaka - gold_fever for Erlangers
description: "inaka - gold_fever server for Erlangers"
category: opensource
comments: true
share: true
---

对inaka没什么好感，是因为`inaka_json`里封装的那个难用的json结构。不过最近看到他们新开源的一个GameServer [gold_fever](https://github.com/inaka/gold_fever): `A Treasure Hunt for Erlangers`，正好可以一睹带状态的游戏Server写法。

## 运行gold_fever

像README里写的，运行这个server很简单(如果lager报错记得更新下erlang.mk)：

~~~bash
git clone https://github.com/inaka/gold_fever.git
cd gold_fever/
make erlang-mk    # 更新erlang.mk
make
make shell
~~~

运行后新开一个窗口运行`erl -name zzz@127.0.0.1 -remsh gold_fever@127.0.0.1 -setcookie erlang-dojo-2015`连接到node，就可以看到下面的step1输出了：

~~~
"--------------------------------------------------------------------------------"
"|                       Welcome to the far west, fellow!                       |"
"|              A very well-known thief has robbed us and escaped               |"
"|                        You have to find that treasure                        |"
"|                     Now spawn 'larry' and listen to me.                      |"
"|                         I have something for you...                          |"
"--------------------------------------------------------------------------------"
Eshell V7.0.2  (abort with ^G)
(gold_fever@127.0.0.1)1>
~~~

默认用的是`generic.config`，还可以指定自己的关卡。

貌似有固定的模式，玩法除了猜，就是直接读源码了。
