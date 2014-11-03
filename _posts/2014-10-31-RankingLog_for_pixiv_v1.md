---
layout: post
title: 分享自用的扫图工具RankingLog for iOS，瀑布流方式查看Pixiv“过去排行”的作品
description: "Share: RankingLog (v1.0) for iOS, a Waterfall pixiv ranking_log client"
category: opensource
comments: true
share: true
---

一直对P站客户端的“过去的排行榜”功能深恶痛绝，每切一次日期都要重新点进去设置排行类型、日期，积累满满(?)的扫图性趣也被来来回回的繁琐操作折腾得精光...

于是自己写了一个专门用来"扫过去排行榜"的App，并开源在 [GitHub\PixivAPI_iOS](https://github.com/upbit/PixivAPI_iOS/tree/master/examples/RankingLog) (其实我是来宣传Pixiv API的)，有兴趣的可以自行下载编译(Xcode6+iOS8.1下编译通过)。这是iPhone布局的截图，因为是R18日榜数据，部分图片已经在脑内自主规制：

![iPhone R18 Waterfall]({{ site.url }}/images/RankingLog_iPhone5S_waterfall.png)

因为主要用到PixivAPI中的SAPI.ranking_log(过去排行)，所以暂时叫这个扫图工具为RankingLog吧。

### 下载地址：

[RankingLog (v1.0)]({{ site.url }}/RankingLog/RankingLog_v1.0.ipa), 2.0M

支持 iPhone/iPad (iOS6.1~iOS8.1, 推荐iOS7以上)，IPA安装需要**越狱机器**

### RankingLog特点

1. 支持瀑布流与滑动翻页方式，查看包括 "日/周/月/男性热门 (每日R18/每周R18)" 等在内的多种历史排行；
2. 日期自动滚卷。例如查看10.30的日排行达到最大翻页限制，则自动转到10.29日继续翻页；
3. 支持原图导出到相册或文件，并支持只查看省流量的移动端图片；
4. 支持本地图片缓存。P站的过去排行会有重复作品，当这些作品再次出现时，会从本地载入已经下载的图片；

## 使用说明

**第一次打开**App会自动跳转设置页面。如果要查看R18作品，需要使用有R18作品权限的pixiv帐号进行登录，在pixiv ID和password框内填入帐号和密码(建议使用小号)，点返回(Back)保存设置：

![iPad Waterfall]({{ site.url }}/images/RankingLog_iPhone5S_settings.png)

**等待Login**完成后，就会看到默认的“昨日的周排行”数据。当滑动到末尾后，会自动载入下一页的数据，此时标题栏会更新为"榜单:pN/M - [日期]"，N是当前页码，M是最大翻页深度：

![iPad Waterfall]({{ site.url }}/images/RankingLog_iPad_waterfall.png)

**点选任意图片**进入浏览模式，右上角按钮可以导出当前图片到相册(或文件)：

![iPad Waterfall]({{ site.url }}/images/RankingLog_iPad_ImageExport.png)

## RankingLog设置项

![iPad Waterfall]({{ site.url }}/images/RankingLog_iPhone5S_settings.png)

在用户名密码输入框下有三个开关，分别代表：

* **[To Doc/]** 导出图片到程序的Documents/下，之后可以用iTunes等工具下载
* **[To Album]** 导出到系统相册。注意第一次需要在弹出窗里选择允许RankingLog访问相册
* **[Large Image]** 显示原始大图。关闭后会下载移动端用的小图，缺点是导出的作品也会变成小图

![iPad Waterfall]({{ site.url }}/images/RankingLog_iTunes_documents.png)

下面的**pages滑动栏**用于调整最大翻页深度，Pixiv客户端默认6页，如果觉得重复作品太多，可以缩减到每3页日期减1。

屏幕底部的 **"Mode | Date" 切换按钮**，用于选择排行榜模式和日期。往往切换模式后希望从最新日期向前翻，此时点Yesterday按钮可以快速重置到昨天的日期。不过注意，RankingLog查看的是"过去的排行榜"数据，如果设置当天或者未来的日期，可能会取不到或得到错误的结果。

目前**RankingLog**还处于开发阶段，以后会加入收藏按钮和相同作者作品查看功能。如果你发现BUG或者想参与开发，尽管到[https://github.com/upbit/PixivAPI_iOS](https://github.com/upbit/PixivAPI_iOS)给我提 Issues / Pull Requests 吧。
