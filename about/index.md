---
layout: page
title: About me
---

# 联系方式

- Email：rmusique@gmail.com

---

# 个人信息

- 硕士/华中科技大学计算机系 信息安全
- 工作年限：3.5年
- GitHub: [https://github.com/upbit](https://github.com/upbit)
- 技术博客：[http://blog.imaou.com](http://blog.imaou.com)

---

# 工作经历

## 深圳卡斯达克科技有限公司(自己创业) （ 2014年6月 ~ 2014年12月 ）
cocos2d-x手游开发主程

和另一个合伙人两人尝试创业，并于3个月内独立完成cocos2d-x战棋类手游的原型编写。不过也是在找天使和风投的过程中才体会到，创业需要的并不仅仅是技术。目前公司已在申请解散的流程中。

## 腾讯科技（深圳）有限公司 （ 2011年4月17日 ~ 2014年5月30日 ）
微博-后台开发工程师

### 项目1：腾讯微博-主索引
主要负责微博首页Timeline的索引Cache层设计、开发以及容灾部署，并参与索引控制层、文件层的设计评审与协议制定。

微博的主要模块包括帐号、关系连、索引、数据四个核心服务，而索引Cache则用于根据关系链实时计算用户的首页Timeline。由于索引一旦故障将导致微博的首页白屏或消息丢失，设计中最优先考虑的是“容灾”。通过内存切片+Binlog同步(类似MySQL的同步机制)，只读副本故障时自动屏蔽等技术，使索引上线近3年来只出现过一次用户可感知故障(IDC网络中断引起)。因此曾与小组其他服务一起，申请并获得过公司级的后台服务稳定奖。

索引Cache设计的另一个难点是计算量。因为采用读扩散架构，在计算首页Timeline时需要并发查询分布在多个机器/进程中的用户发表数据，整个查询过程对CPU/网络流量的开销很大。伦敦奥运会刘翔摔倒时，索引瞬间的读写请求翻了一倍以上，甚至触发了大量进程的防滚雪崩保护。为了应付类似的突发情况，我和另一个同事一起设计了关系连收敛系统。通过预测用户发表提前除去不需要返回的关系链用户，从而减少索引Cache的计算量。该服务上线后，成功降低了索引Cache 40% 的计算开销。

### 项目2：腾讯微博-智慧推广告系统
负责后台侧的项目管理与协调，及广告信令通道的设计与开发，并参与其他模块的评审。

算是接手的第一个正式部门内合作项目。不像以前只用组内几个人讨论、写文档然后代码开发，项目中还包括与前端负责人，产品、商业接入接口人打交道，并跟进其他3个开发的进度。当然因为分类用的“用户画像”数据是从公司数据平台部拿的，其间也体会到一些和外部门合作的方法。也许是兄弟们都很给力，带的第一个项目基本上如期上线。并且在初期的投放测试中，定向广告的点击率达到0.1%以上，比传统不分组的投放方式高出几十倍。

### 项目3：微视-长视频上传通道建设
解决微视长视频上传后，需要在数据平台部和腾讯视频进行2次转码的问题。主要负责项目管理，以及跨部门的沟通合作。

因为微视的开发在北京，大部分时候是异地沟通。整个项目实际上几乎没有开发量，主要解决微视为了快速上线，上传2份给各自部门的转码冗余问题。通过在北京出差了解微视长视频上传的细节后，顺利与其他两个部门接口人确认方案并达成共识。最终通道模块于一周内提前上线。

## 华中科技大学 计算机系 信息安全 （ 2008年9月 ~ 2011年3月 ）
硕士研究生主要研究网络安全方向。主要涉及P2P流媒体检测及特征自动提取，以及实验室的研究方信息隐藏、隐写及检测，Botnet流量检测等。

我在P2P流媒体检测项目中主要负责DAG卡的网络流量采集与流还原，以及基于行为的流量识别模块开发。读研前都是在Windows下写程序，而随着研究的深入，逐渐学会了Linux下编译E1000网卡的零拷贝驱动，给内核打patch，以及通过BPF(Berkeley Packet Filter)分析网络流量。

---

# 开源项目和作品

## 开源项目

- [python-imobiledevice_demo](https://github.com/upbit/python-imobiledevice_demo) : libimobiledevice的Python版演示程序。_包括Afc2Client的Shell，通过InstallationProxyClient安装IPA，以及使用DebugServerClient调试启动程序_
- [IOTrackerOnWeb](https://github.com/upbit/IOTrackerOnWeb) : iOS应用I/O跟踪tweak，通过CocoaHTTPServer提供WebSocket的实时日志查看。_支持常见的网络/文件IO监控，以及NSLog重定向_
- [PixivAPI_iOS](https://github.com/upbit/PixivAPI_iOS) / [pixivpy](https://github.com/upbit/pixivpy) : pixiv.net 的 API (Objective-C/Python)
- [Pixiv-RankingLog](https://github.com/upbit/Pixiv-RankingLog) : 学习Xcode开发时写的Pixiv过去排行扫图应用。

## 技术文章
看雪ID: [木桩](http://bbs.pediy.com/member.php?u=192350) （精华帖: 7）

- [【原创】GSM Sniffing入门（硬件篇1-2楼，软件篇6楼）](http://bbs.pediy.com/showthread.php?t=182574)
- [【原创】SMM Rootkit初步 - 读写SMRAM（带你迈入CPU级Rootkit之门）](http://bbs.pediy.com/showthread.php?t=84835)

## 兴趣爱好
喜欢研究智能设备及安全，之前GSM网络短信嗅探就是在业余时间折腾的，最近则在尝试分析Pangu8的越狱原理。以下是目前掌握的主要技能：

- iOS越狱插件开发: Theos(tweak)/libimobiledevice
- Xcode应用开发: CocoaHTTPServer/SDWebImage/CHTCollectionViewWaterfallLayout
- 反汇编调试：IDA/gdb/lldb
- 后台开发：Linux/MySQL/读扩散/容灾/SSD
- 脚本语言：Python/Bash
- 云平台：GAE/AWS
