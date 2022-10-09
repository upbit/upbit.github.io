---
layout: post
title: Google Nest破解与智能设备安全随想
description: "Google Nest Exploiting DFU and the security of intelligent equipments"
category: hack
comments: true
share: true
---

今天有幸在QQ上和看雪坛主聊了几句，感触颇深。学生时代就是在图书馆看他的书启蒙的，也是那年代头一次知道有个论坛叫“看雪”。说来惭愧，个人在安全方面建树不深，之前凭一篇[《GSM Sniffing入门》](http://bbs.pediy.com/showthread.php?t=182574)去论坛混精华的帖子，居然让坛主产生我在硬件方面有所研究的错觉... 不过聊到智能设备，确实也勾起我那爱折腾的心。之前买树莓派折腾各种电路板、到现在折腾路由器，其实这些智能设备离我们并不远。

说到智能硬件，不得不提最近最火爆的Google Nest被黑一事。早在5月底就有人放出了视频[Hacking into the Nest Thermostat (SSL@UCF)](https://www.youtube.com/watch?v=7AnvTgAKa-g)(要翻墙)。随后，Grant Hernandez, Orlando Arias, Daniel Buentello, and Yier Jin在黑帽大会US-14上发布了 [Smart Nest Thermostat: A Smart Spy in Your Home](https://www.blackhat.com/docs/us-14/materials/us-14-Jin-Smart-Nest-Thermostat-A-Smart-Spy-In-Your-Home-WP.pdf) 这篇论文。

论文里提到，在重置Nest时会导致 sys_boot5 高电平(貌似和TI AM3703这个CPU有关)，而这个引脚会直接触发USB启动。从而可以通过USB口，将自定义的x-loader刷入到Nest中，从而获取Shell并进一步获取Nest的控制权。文末还提到，**这个漏洞可以在其他使用类似CPU的设备上应用！**

然而论文都是简洁的，无一例外的需要你大量的Google相关资料。万幸找到GTVHacker的[一篇博文](http://blog.gtvhacker.com/2014/google-nest-exploiting-dfu-for-root/)，里面从获取root的角度提到了这个漏洞的另一些细节：

```
The Bug:

The Nest uses a CPU similar to the OMAP3630 series. This CPU features a Device Firmware Update (DFU) mode that can be accessed by holding down the Nest’s screen while off. This mode is intended for the manufacturer to easily diagnose and repair the device. Unfortunately, in the case of the Nest, this mode also allows us to modify the device without restriction.
```

德州仪器的这块CPU存在一个DFU模式，原本是用于厂商诊断和修复设备用的，但对于Nest则可以用来刷入自定义固件。

这篇文章提到了详细的攻击过程：

1. 通过 omap3_loader 上传自定义固件到DFU模式的Nest里；
2. 先传入stage 1的x-loader，x-loader用于加载stage 2的U-Boot；(这里和黑帽大会里说的方法不太一样？不是直接改x-loader？)
3. 修改U-Boot，在里面放入ssh server: Dropbear；
4. 启动一个脚本，定时检查Nest虚拟磁盘中的host.txt，从中读取配置建立反向的SSH连接；

作者甚至还开源了整个root过程用到的源码：[NestDFUAttack](https://github.com/gtvhacker/NestDFUAttack) 对照[Nest开源的代码](https://nest.com/legal/compliance/)，应该有可能看出是怎么修改U-Boot到获取root的。这回有东西可以研究了:)
