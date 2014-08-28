---
layout: post
title: 使用dnsmasq+pdnsd在WNDRMACv2(OpenWrt)上，为全家科学上网及Google加速
description: "Speedup Google with dnsmasq and pdnsd on WNDRMACv2(openwrt)"
category: misc
comments: true
share: true
---

[imouto.host](https://plus.google.com/100484131192950935968/about)的Google地址又抽风了，各种打不开。虽然这个更新很及时，但一来没有可以用curl脚本获取的地址(作者提供的是Google Drive和Dropbox)，二来台式机和笔记本都要增量更新这个host(因为里面还有防adobe激活的条目)，于是打算一劳永逸的在路由器里解决问题。

其实早就给家里WNDRMACv2刷了OpenWrt，但自从发现跑go-agent经常莫名退出后(当然现在go-agent也不好用了)，它唯一的用途就是拿来做samba服务器了→_→

今天逛Github无意中看到[clowwindy大大](https://github.com/clowwindy)的ChinaDNS-C，以前就看过ChinaDNS的python代码，里面很巧妙的用黑名单丢弃无效的DNS应答，于是在路由器里搭建个干净的DNS想法又冒了出来。然而折腾到中午才意识到，ChinaDNS-C是配合dnsmasq用的，而且需要定时更新黑名单IP列表。于是改用dnsmasq+pdnsd方案，让pdnsd强制用tcp方式访问远端dns，来得到干净的DNS返回。

然而科学上网并非简单的事，这里记录下整个过程，免得以后再走弯路：

### 1. 安装dnsmasq和pdnsd

```Shell
# opkg update
# opkg install dnsmasq pdnsd
```

配置方面文章 [openwrt 上通过 pdnsd 和 dnsmasq 解决 dns 污染](https://wido.me/sunteya/use-openwrt-resolve-gfw-dns-spoofing) 解释的比较清楚，这里就只列出我的参数配置：

首先为dnsmasq.conf增加conf-dir，并生成个gfw.conf：
```Shell
mkdir /etc/dnsmasq.d/
touch /etc/dnsmasq.d/gfw.conf
echo "conf-dir=/etc/dnsmasq.d" >> /etc/dnsmasq.conf
```

配置pdnsd(/etc/pdnsd.conf)：
```JSON
global {
    server_ip = 192.168.1.1;
    server_port = 5353;             # 因为dnsmasq用了53，这里要换一个
    query_method=tcp_only;
}
server {
    label= "Main";
    ip = 8.8.8.8;                   # 使用Google的DNS，其实OpenDNS的也可以
    root_server = on;
    uptest = none;
}
```

接着配置/etc/dnsmasq.d/gfw.conf将Google转发到路由器(192.168.1.1)的pdnsd(5353)端口上：
```
server=/.google.com/192.168.1.1#5353
server=/.google.com.hk/192.168.1.1#5353
server=/.gmail.com/192.168.1.1#5353
```

重启dnsmasq和pdnsd：
```Shell
/etc/init.d/dnsmasq restart
/etc/init.d/pdnsd restart
```

试着dig下gmail：
```Shell
# dig @192.168.1.1 www.gmail.com

; <<>> DiG 9.9.1-P3 <<>> @192.168.1.1 www.gmail.com
; (1 server found)
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 42881
;; flags: qr rd ra; QUERY: 1, ANSWER: 4, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 1024
;; QUESTION SECTION:
;www.gmail.com.         IN  A

;; ANSWER SECTION:
www.gmail.com.      26832   IN  CNAME   mail.google.com.
mail.google.com.    105726  IN  CNAME   googlemail.l.google.com.
googlemail.l.google.com. 628    IN  A   173.194.33.85
googlemail.l.google.com. 628    IN  A   173.194.33.86

;; Query time: 2 msec
;; SERVER: 192.168.1.1#53(192.168.1.1)
;; WHEN: Thu Aug 28 17:04:00 2014
;; MSG SIZE  rcvd: 127
```

返回是对了，但是怎么ping不通？拿 http://ping.chinaz.com/ 测试了下，发现虽然这个是Google的IP，但国内很多地方都无法访问。换了著名的404域名facebook.com试了下，直接对5353端口dig果然返回了正确地址(假的一般是37.61.54.158)，并且拿https可以访问：
```Shell
# dig @192.168.1.1 -p5353 www.facebook.com

; <<>> DiG 9.9.1-P3 <<>> @192.168.1.1 -p5353 www.facebook.com
; (1 server found)
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 19322
;; flags: qr rd ra; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 1024
;; QUESTION SECTION:
;www.facebook.com.      IN  A

;; ANSWER SECTION:
www.facebook.com.   1436    IN  CNAME   star.c10r.facebook.com.
star.c10r.facebook.com. 900 IN  A   31.13.79.96

;; Query time: 372 msec
;; SERVER: 192.168.1.1#5353(192.168.1.1)
;; WHEN: Thu Aug 28 17:09:16 2014
;; MSG SIZE  rcvd: 85
```

看来最近GFW对Google服务器有特殊照顾，有些地址ping各种丢包，有些则直接100% lost。想了下imouto.host的解决办法————找到最稳定的Google服务器，imouto.host容易失效无非是那个IP被重新封掉而已，只要找到足够多的有效IP(而不是DNS返回的国外可用IP)，就能保证Google的访问变得流畅。

先查询Google的IP地址段：

```Shell
# dig @8.8.8.8 _netblocks.google.com txt

; <<>> DiG 9.9.1-P3 <<>> @8.8.8.8 _netblocks.google.com txt
; (1 server found)
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 30349
;; flags: qr rd ra; QUERY: 1, ANSWER: 1, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 512
;; QUESTION SECTION:
;_netblocks.google.com.     IN  TXT

;; ANSWER SECTION:
_netblocks.google.com.  3599    IN  TXT "v=spf1 ip4:216.239.32.0/19 ip4:64.233.160.0/19 ip4:66.249.80.0/20 ip4:72.14.192.0/18 ip4:209.85.128.0/17 ip4:66.102.0.0/20 ip4:74.125.0.0/16 ip4:64.18.0.0/20 ip4:207.126.144.0/20 ip4:173.194.0.0/16 ~all"

;; Query time: 196 msec
;; SERVER: 8.8.8.8#53(8.8.8.8)
;; WHEN: Thu Aug 28 17:13:19 2014
;; MSG SIZE  rcvd: 265
```

"v=spf1 ip4:216.239.32.0/19 ip4:64.233.160.0/19 ip4:66.249.80.0/20 ip4:72.14.192.0/18 ip4:209.85.128.0/17 ip4:66.102.0.0/20 ip4:74.125.0.0/16 ip4:64.18.0.0/20 ip4:207.126.144.0/20 ip4:173.194.0.0/16 ~all"
这个就是Google的IP分段，参考imouto.host里google.com/google.com.hk的IP，前后连续ping了几个服务器，然后用https访问确认下是不是Google的搜索服务器。

得到IP后直接在/etc/hosts里增加本地host，重启dnsmasq：
```Shell
# Google
64.233.168.103  www.google.com
64.233.168.104  www.google.com
64.233.168.105  www.google.com
64.233.168.106  www.google.com
210.242.125.84  www.google.com.hk
210.242.125.88  www.google.com.hk
```

重启后，Google就被固定在上面几个速度较快的服务器上了。至此成功科学上网。

另外给一个dnsmasq的完整gfw.conf，免得手工编辑：
```
server=/.android.com/192.168.1.1#5353
server=/.appspot.com/192.168.1.1#5353
server=/.blogspot.com/192.168.1.1#5353
server=/.box.com/192.168.1.1#5353
server=/.chrome.com/192.168.1.1#5353
server=/.dropbox.com/192.168.1.1#5353
server=/.dropboxusercontent.com/192.168.1.1#5353
server=/.facebook.com/192.168.1.1#5353
server=/.facebook.net/192.168.1.1#5353
server=/.fbcdn.net/192.168.1.1#5353
server=/.flickr.com/192.168.1.1#5353
server=/.g.cn/192.168.1.1#5353
server=/.g.co/192.168.1.1#5353
server=/.ggpht.com/192.168.1.1#5353
server=/.gmail.com/192.168.1.1#5353
server=/.goo.gl/192.168.1.1#5353
server=/.google-analytics.com/192.168.1.1#5353
server=/.google.cn/192.168.1.1#5353
server=/.google.co.jp/192.168.1.1#5353
server=/.google.com/192.168.1.1#5353
server=/.google.com.hk/192.168.1.1#5353
server=/.google.com.sg/192.168.1.1#5353
server=/.google.com.tw/192.168.1.1#5353
server=/.googleadservices.com/192.168.1.1#5353
server=/.googleapis.com/192.168.1.1#5353
server=/.googlecode.com/192.168.1.1#5353
server=/.googledrive.com/192.168.1.1#5353
server=/.googlehosted.com/192.168.1.1#5353
server=/.googlelabs.com/192.168.1.1#5353
server=/.googlemail.com/192.168.1.1#5353
server=/.googlesource.com/192.168.1.1#5353
server=/.googlesyndication.com/192.168.1.1#5353
server=/.googleusercontent.com/192.168.1.1#5353
server=/.gstatic.cn/192.168.1.1#5353
server=/.gstatic.com/192.168.1.1#5353
server=/.live.com/192.168.1.1#5353
server=/.mediawiki.org/192.168.1.1#5353
server=/.panoramio.com/192.168.1.1#5353
server=/.staticflickr.com/192.168.1.1#5353
server=/.t.co/192.168.1.1#5353
server=/.tfbnw.net/192.168.1.1#5353
server=/.thefacebook.com/192.168.1.1#5353
server=/.tinypic.com/192.168.1.1#5353
server=/.tweetdeck.com/192.168.1.1#5353
server=/.twimg.com/192.168.1.1#5353
server=/.twitpic.com/192.168.1.1#5353
server=/.twitter.com/192.168.1.1#5353
server=/.wikimedia.org/192.168.1.1#5353
server=/.wikinews.org/192.168.1.1#5353
server=/.wikipedia.org/192.168.1.1#5353
server=/.wordpress.com/192.168.1.1#5353
server=/.wordpress.org/192.168.1.1#5353
server=/.wp.com/192.168.1.1#5353
server=/.yimg.com/192.168.1.1#5353
server=/.youtube.com/192.168.1.1#5353
server=/.ytimg.com/192.168.1.1#5353
```
