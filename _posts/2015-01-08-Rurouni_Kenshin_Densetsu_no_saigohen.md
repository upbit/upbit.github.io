---
layout: post
title: 终于等到了！-《浪客剑心：传说的完结篇》
description: "Rurouni Kenshin Densetsu no saigo-hen"
category: erlang
comments: true
share: true
---

苦等Offer的消息没等来，却意外的盼来了 浪客剑心 伝説の最期編 的下载！感谢[幻之字幕组](http://bt.ktxp.com/html/2015/0108/391871.html)送上的大礼，果断丢离线里晚上等看剑心的新必杀技！

![Rurouni Kenshin]({{ site.url }}/images/201501/rurouni_kenshin.jpg)

---

另外插一个今天发现的小东西 [GitHub/iDict](https://github.com/Pr0x13/iDict)，号称能够绕过iCloud的重试限制去 BruteForce 探测 AppleID 的密码。漏洞是出在 iosbuddy/createDelegateAccounts 这个接口上，不过按脚本里的方法试了下，返回下面这堆东西：

~~~xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>status</key>
    <integer>1</integer>

    <key>status-message</key>
    <string>A server problem is blocking Apple ID sign in. Try signing in later.</string>

  </dict>
</plist>
~~~

看来Apple把该服务给停掉了。想想这哥们也够坏的，在新年第一天发布这个poc，估计忙坏Apple的开发了 :P
