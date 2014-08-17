---
layout: post
title: 终于把博客首页显示慢的问题解决了，PageSpeed Insights立了大功！
description: "按PageSpeed Insights优化了blog的访问速度，在果然移动端比之前快上许多。以前github在晚上load几个css简直是要人命了"
category: misc
comments: true
share: true
---

按[PageSpeed Insights](https://developers.google.com/speed/pagespeed/insights/)的建议优化了博客的性能，果然神器啊。

最终优化完PageSpeed Insights移动端评分上了90，预载子页面css后也不会导致POST子页面卡顿。扣分的是静态资源缓存时间问题，不过这个是GitHub的设置，暂时不知道怎么处理

![PageSpeed Insights]({{ site.url }}/images/pagespeed_insights.png)

以前GitHub在晚上load几个css简直是要人命了，在微信里点开会白屏好久，现在终于感觉不出来这是GitHub的服务器了:P
