---
layout: post
title: 今天去Two Woods面试，一番疲劳轰炸下连说的啥都忘了...
description: "Intervirw in Two Woods"
category: opensource
comments: true
share: true
---

约的3:00pm面试结果提前到了，果断坐楼下刷秘密。不过附近的秘密居然都是爆CB离职的，索然无味... 然后顺手Google了下对方的电话，bingo! 邮箱和电话等信息都有了 [wumii.org](http://whois.domaintools.com/wumii.org)

刷到3点多开始面试，三轮下来别说说了些啥，连前两个面试官的长相都快记不清了 @_@ 出来时已经19:30，那个月明星稀啊... 呃，好吧，楼太高看不到月亮；今晚也没有星星 :(

唯一还记得的就是和CEO聊的时候提到的 “**分支预测器（Branch Predictor）**”，回来查了下果然是我记错了。估计是把stackoverflow上那个关于likely/unlikely的帖子搞混了，虽然同样是为了增加效率，但这个并不是分支预测：[How much do __builtin_expect(), likely(), and unlikely() improve performance?](http://blog.man7.org/2012/10/how-much-do-builtinexpect-likely-and.html)

恶补了下相关的知识，就当是今天的收获把。

## [分支预测器](http://zh.wikipedia.org/zh/%E5%88%86%E6%94%AF%E9%A0%90%E6%B8%AC%E5%99%A8)

在现代流水线技术的处理器中，遇到分支指令会因为可能发生的跳转，而导致智能等待分支执行结束，才能进入下一条流水线指令。

为了应付这种情况，继而衍生出以下几种分支预测技术：

### 静态分支预测
类似MISP的单方向静态分支预测，总是认为条件跳转不执行；当然还有优化循环的总是认为向前跳转会执行的，总之准确率只有50%；

### 动态分支预测

一般的有两种逻辑：

1. **BTB(Branch Target Buffer)**: 记录分支指令的目的跳转地址；
2. **BHT(Branch Histroy Table)**: 记录分支是否跳转，一般用2bit饱和计数器表示(如11和10代表跳转，01和00表示不跳转)；

BTB用法比较简单，在取指阶段用PC查BTB，以获取转移历史。BHT则比较有意思，还有个状态机来决定是否跳转：

![branch_prediction_2bit_saturating_counter]({{ site.url }}/images/201412/branch_prediction_2bit_saturating_counter-dia.png)

简单来说就是需要连续2次跳转或不跳转，才会使预测改变。比如连续2次跳转后饱和计数器为11，之后第一次不跳转导致其降低为10，此时还是预测发生跳转的。经测试这种预测器能达到93.5%的准确率。

参考文献：[http://blog.csdn.net/edonlii/article/details/8754724](http://blog.csdn.net/edonlii/article/details/8754724)

## 关于ARM

当然往后还有其他分支预测技术，不过ARM却是独树一帜的去掉了这个东西。

ARM采用固定的32位opcode，减轻分支执行的压力。而为了补强这种简单的设计，又为每条指令头部预留了4bit用于条件执行，这也就是为什么每条ARM都可以加上NE/GE之类前缀，而不用B跳转执行的原因。之前看了段ARM和x86汇编的对比，才明白ARM的这种条件执行前缀有多么省事。
