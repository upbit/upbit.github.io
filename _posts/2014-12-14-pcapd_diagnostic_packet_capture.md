---
layout: post
title: 利用 com.apple.pcapd 服务在iOS设备上抓包
description: "pcapd: service to diagnostic packet capture from an iOS device"
category: opensource
comments: true
share: true
---

辛辛苦苦为libimobiledevice写了一天 com.apple.pcapd 服务接口 [libimobiledevice:pcapd](https://github.com/upbit/libimobiledevice/tree/pcapd)，最后却发现原来Apple已经有相关的工具了，合法的从未越狱设备上抓包...

## iOS Packet Tracing

Technical Q&A QA1176 - [Getting a Packet Trace](https://developer.apple.com/library/ios/qa/qa1176/_index.html#//apple_ref/doc/uid/DTS10001707-CH1-SECRVI)：

iOS 5 added a remote virtual interface (RVI) facility that lets you use OS X packet trace programs to capture traces from an iOS device. The basic strategy is:

1. Connect your iOS device to your Mac via USB.
2. Set up an RVI for that device. This creates a virtual network interface on your Mac that represents the iOS device's networking stack.
3. Run your OS X packet trace program, and point it at the RVI created in the previous step.

利用rvictl能够很方便的在MacOS上建立虚拟端口，然后用tcpdump抓取上面的内容。方法如下：

~~~sh
# 查看设备的UDID
$ idevice_id -l
622b53c647548234ddcef0ee3abee616005051ed

# 开启映射
$ rvictl -s 622b53c647548234ddcef0ee3abee616005051ed
Starting device 622b53c647548234ddcef0ee3abee616005051ed [SUCCEEDED] with interface rvi0

# 查看rvi是否建立成功
$ ifconfig -l
lo0 en0 en1 en2 ... rvi0

# 使用tcpdump抓包
$ tcpdump -i rvi0 -ntXs0

# 删除映射
$ rvictl -x 622b53c647548234ddcef0ee3abee616005051ed
Stopping device 622b53c647548234ddcef0ee3abee616005051ed [SUCCEEDED]
~~~
