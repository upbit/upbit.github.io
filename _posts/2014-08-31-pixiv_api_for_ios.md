---
layout: post
title: PixivAPI_iOS初版完成，附带一个日榜图片查看例子
description: "Uploaded Pixiv API for iOS, with a demo project PixivDaily"
category: opensource
comments: true
share: true
---

折腾了几天，终于磕磕碰碰的把 PixivAPI_iOS(Pixiv API for iOS) 发布到[GitHub](https://github.com/upbit/PixivAPI_iOS)了。主要是以前没用过SDWebImage，写PixivDaily这个Demo花了很多时间。不过SDWebImage的Cache和异步下载确实很赞！

### 1. API的用法介绍

API为了尽可能简单，决定用block语法将异步获取和Illust解析封装在里面，于是最后调用方式变成了这样：

```objective-c
#import "PixivFetcher.h"

- (void)getDailyRanking
{
    [PixivFetcher API_getRanking:1 mode:PIXIV_RANKING_MODE_DAY content:PIXIV_RANKING_CONTENT_ALL
                       onSuccess:^(NSArray *illusts, BOOL isIllust) {
                           NSLog(@"%@", illusts);
                       }
                       onFailure:^(NSURLResponse *response, NSInteger responseCode, NSData *data, NSError *connectionError) {
                           NSLog(@"[HTTP %d] %@", responseCode, connectionError);
                       }];
}
```

onSuccess:^(NSArray *illusts, BOOL isIllust) 会在解析成功后调用，NSArray *illusts是一个IllustModel的数组。isIllust透传API的参数，如果是 getUser: 这样获取 Author 信息的API，则 isIllust = NO，此时IllustModel部分属性将没有值

如果responseCode不是200或连接出错时，会调用 onFailure:^(NSURLResponse *response, NSInteger responseCode, NSData *data, NSError *connectionError)，此时可以进行重试或者错误处理

### 2. API的请求流程

对于 API_getRanking: 具体做了些什么，可以概括为以下几个：

1. 调用 URLforRanking: 得到 Pixiv SAPI 的请求URL
2. 如果返回是List调用 asyncFetchIllustList:，单个illust则调用 asyncFetchIllust: 异步获取并解析返回为 IllsutModel
3. 以 asyncFetchIllustList: 为例，调用 asyncURLFetch: 并判断返回是否正常，正常则调用 [PixivFetcher parsePayloadList:payload] 解析返回数据，并传递给 onSuccessHandler()；失败则调用 onFailureHandler()
4. asyncURLFetch: 里先设置 NSMutableURLRequest 的 Referer 和 User-Agent，因为这里调用的是私有的iOS SAPI，必须要稍微伪装下...
5. 接下来就是常规的 [NSURLConnection sendAsynchronousRequest:...] 了，然后淡定的等待返回

### 3. IllustModel说明

大部分提供的属性都在[IllustModel.h](https://github.com/upbit/PixivAPI_iOS/blob/master/PixivFetcher/IllustModel.h)里，对应字段也标注出来了。

需要说明的是 IllustModel.toDataArray 函数，可以将IllustModel反转化为原始的 NSArray 数组，便于存储到数据库中。解析时需要用到 [PixivFetcher parseDataArrayToModel:] 函数，将 NSArray 重新转化为 IllustModel：

```objective-c
+ (IllustModel *)parseDataArrayToModel:(NSArray *)data;
```

## PixivDaily 的截图

目前完成日榜首页拉取，点击查看图片并记录最近访问：

![PixivDaily Screenshot1](https://raw.github.com/upbit/PixivAPI_iOS/master/examples/screenshots/PixivDaily_01.png)

![PixivDaily Screenshot2](https://raw.github.com/upbit/PixivAPI_iOS/master/examples/screenshots/PixivDaily_02.png)

不过PixivDaily这个demo还有单击、双击图片操作，日榜翻页等坑要填，今天太累就先到这里吧 (远目
