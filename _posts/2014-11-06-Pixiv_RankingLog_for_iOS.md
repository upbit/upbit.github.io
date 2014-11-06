---
layout: post
title: 开源的Pixiv“过去排行”扫图专用App - RankingLog(v1.2)
description: "Pixiv RankingLog v1.2, a Waterfall pixiv ranking_log client"
category: opensource
comments: true
share: true
redirect_from: /opensource/2014/10/31/RankingLog_for_pixiv_v1.html
---

我是个重度的P站扫图爱好者，与其说享受壁纸库和收藏列表里满满的成果，扫图时的快感更让我欲罢不能。我曾经不厌其烦的查看Pixiv的日榜，之后又写了工具去抓取日榜排名(其副产品就是[Pixiv日榜搬运姬(@pixivbot)](https://github.com/upbit/PixivBot))，闲暇之余也会拿起iPad翻翻跟随作者的新作。

如果你也是和我同样的扫图中毒者，你一定也尝试过用下面的方式收集图片(按病症严重程度递增)：

1. 找到收藏作品的作者，去查看ta的作品或者收藏...
2. 不断查看P站的日榜、周榜与月榜，挑选出喜欢的图片右键...
3. 极度图荒时，从收藏作品的Tag、或者收藏该作品的用户里，作为种子继续翻找图片...

有人说了，为什么不用客户端隐藏的“过去的排行榜”功能，查看历史作品？过去的排行榜在哪？打开Pixiv for iOS，点标题栏的“推荐”->“编辑”->选中“过去的排行榜”，之后返回来选榜单类型和时间。

“过去排行”确实是查看漏网作品的利器，只不过iOS客户端上此功能极为反人类，只有2页数据不说每每点回去修改日期，所有设置项就都重置了。于是萌生了自己写通过“过去排行”扫图的App的想法，App开源在[GitHub/Pixiv-RankingLog](https://github.com/upbit/Pixiv-RankingLog)。

<span style="color:#f00;">**IPA下载：**</span>[RankingLog_v1.2.ipa](http://blog.imaou.com/RankingLog/RankingLog_v1.2.ipa) - (通用版，支持iOS6.1~iOS8.1，需越狱)

## 使用说明:

![RankingLog Helper]({{ site.url }}/images/201411/RankingLogHelper.png)

## 主要功能：

1. 无限翻页查看"过去的排行榜"，并支持图片预载与缓存；
2. 支持导出原图到相册或文件(Documents/下)；
3. 支持查看同作者的其他作品；v1.1新增
4. 支持收藏到Pixiv，并可以查看帐号的公开收藏；v1.2新增

更多功能，自己下载体验吧 :P

附iPad2(iOS7.1.1)实际运行截图：

![RankingLog Helper]({{ site.url }}/images/201411/RankingLog_iPad_waterfall.png)

ps: 有任何问题或建议，可以直接在GitHub上提issus，或者给我留言。
