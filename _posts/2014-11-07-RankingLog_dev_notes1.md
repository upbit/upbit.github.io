---
layout: post
title: Pixiv RankingLog for iOS 开发手记01 (XCode6/iOS8.1)
description: "Pixiv RankingLog for iOS development notes 01 (XCode6/iOS8.1)"
category: opensource
comments: true
share: true
---

昨天在[开源的Pixiv“过去排行”扫图专用App - RankingLog](http://blog.imaou.com/opensource/2014/11/06/Pixiv_RankingLog_for_iOS.html)发布了完善后的版本，感觉通过写RankingLog很好的熟悉了XCode6的Storyboard和自动布局，这里记录下其中遇到的问题和解决方法，以免以后自己走弯路。

## CocoaPods

先从整个工程说起吧，用到了下面3个pods

```
platform :ios, "6.1"
source 'https://github.com/CocoaPods/Specs.git'

pod 'SDWebImage', '~>3.6'
pod 'CHTCollectionViewWaterfallLayout', :head
pod 'SVProgressHUD', :head
```

[SDWebImage](https://github.com/rs/SDWebImage)就不用多介绍了，API简单功能强大。[CHTCollectionViewWaterfallLayout](https://github.com/chiahsien/CHTCollectionViewWaterfallLayout)是RankingLog用到的瀑布流插件，因为特殊性不像前面的控件那么容易上手，而且自定义Cell还是蛮头疼的；P站的缩略图是128x128的方型无法显示出瀑布流，不过自动调整cell大小这点还是能用到的，后面具体说明。[SVProgressHUD](https://github.com/TransitApp/SVProgressHUD)是个方便的弹出框，等待、成功/错误提示都由这个控件提供。

## CHTCollectionViewWaterfallLayout

首先是瀑布流实现，之前[有篇文章](http://blog.imaou.com/study/2014/10/24/Waterfall_CollectionView_Layout.html)详细介绍了CHTCollectionViewWaterfallLayout的用法。当时使用xib自定义cell，但随之而来的问题是Segue只能通过代码来发起...

苦苦尝试最终找到StoryBoard里自定义Cell的方法：

1. 在StoryBoard的UICollectionView里放一个UICollectionViewCell，Collection Reusable View的Identifier里填入WaterfallCell。这样就不用 collectionView registerClass:forCellWithReuseIdentifier: 了，一切由StoryBoard搞定；
2. 定义CHTCollectionViewCell继承自UICollectionViewCell，用于绑定自定义Cell上面的UIImageView和UILabel；

这样自定义Cell就完成了(相关代码位于[CHTWaterfallLayout](https://github.com/upbit/Pixiv-RankingLog/tree/master/RankingLog/CHTWaterfallLayout)下)，这样自定义Cell的优势在于，能够在StoryBoard里直接拖Segue来实现跳转。

## ScrollImageViewController

图片浏览应用，自然需要个UIScrollView来查看图片了。网上找到个能够使图片自适应窗口，放大后自动居中的片段，于是封装到了ScrollImageViewController里。

首先在UIViewController里放置UIScrollView，里面放上UIImageView。UIScrollView设置距离四周0的平铺，而UIImageView四个对齐绑定到下面四个NSLayoutConstraint上，用于动态居中：

```objective-c
@interface ScrollImageViewController () <UIScrollViewDelegate>

@property (weak, nonatomic) IBOutlet NSLayoutConstraint *constraintLeft;
@property (weak, nonatomic) IBOutlet NSLayoutConstraint *constraintRight;
@property (weak, nonatomic) IBOutlet NSLayoutConstraint *constraintTop;
@property (weak, nonatomic) IBOutlet NSLayoutConstraint *constraintBottom;

@end
```

并在scrollViewDidZoom:时调用updateConstraints:使图片居中显示：

```objective-c
- (void)scrollViewDidZoom:(UIScrollView *)scrollView
{
    [self updateConstraints];
}

- (void)updateConstraints
{
    float imageWidth = self.imageView.image.size.width;
    float imageHeight = self.imageView.image.size.height;

    float viewWidth = self.view.bounds.size.width;
    float viewHeight = self.view.bounds.size.height;

    // center image if it is smaller than screen
    float hPadding = (viewWidth - self.scrollView.zoomScale * imageWidth) / 2;
    if (hPadding < 0) hPadding = 0;

    float vPadding = (viewHeight - self.scrollView.zoomScale * imageHeight) / 2;
    if (vPadding < 0) vPadding = 0;

    self.constraintLeft.constant = hPadding;
    self.constraintRight.constant = hPadding;

    self.constraintTop.constant = vPadding;
    self.constraintBottom.constant = vPadding;

    // Makes zoom out animation smooth and starting from the right point not from (0, 0)
    [self.view layoutIfNeeded];
}
```

这是调整图片大小以适应屏幕(完全显示方式)，其中 fitZoom -= 0.0001 是修复当ZoomScale略大时引起滑动手势失效的BUG：

```objective-c
// Zoom to show as much image as possible unless image is smaller than screen
- (void)updateZoom
{
    float fitZoom = MIN(self.view.bounds.size.width / self.imageView.image.size.width,
                        self.view.bounds.size.height / self.imageView.image.size.height);

    fitZoom -= 0.0001;      // FIX BUG: zoom image small then frame, so disable scrollView's PanGesture

    self.scrollView.minimumZoomScale = (fitZoom < 1.0) ? fitZoom : 1.0;
    //self.scrollView.maximumZoomScale = 3.0;

    // Force scrollViewDidZoom fire if zoom did not change
    if (fitZoom == self.lastZoomScale) fitZoom += 0.000001;

    self.lastZoomScale = self.scrollView.zoomScale = fitZoom;
}
```

## SDWebScrollImageViewController

ScrollImageViewController只完成了基本的图片展示，图片缓存和手势都还没有实现。定义SDWebScrollImageViewController继承于ScrollImageViewController，在viewDidLoad:中加入手势：

```objective-c
#pragma mark - View Controller

- (void)viewDidLoad
{
    [super viewDidLoad];

    // 设置图片下载的默认User-Agent
    [SDWebImageManager.sharedManager.imageDownloader setValue:@"PixivIOSApp/5.1.1" forHTTPHeaderField:@"User-Agent"];
    SDWebImageManager.sharedManager.imageDownloader.executionOrder = SDWebImageDownloaderLIFOExecutionOrder;

    // single/double tap gesture
    UITapGestureRecognizer *singleTapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(singleTap:)];
    singleTapGesture.numberOfTapsRequired = 1;
    [self.scrollView addGestureRecognizer:singleTapGesture];
    UITapGestureRecognizer *doubleTapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(doubleTap:)];
    doubleTapGesture.numberOfTapsRequired = 2;
    [self.scrollView addGestureRecognizer:doubleTapGesture];
    [singleTapGesture requireGestureRecognizerToFail:doubleTapGesture];

    // left/right swipe gesture
    self.scrollView.userInteractionEnabled = YES;
    UISwipeGestureRecognizer *leftSwipeGesture = [[UISwipeGestureRecognizer alloc] initWithTarget:self action:@selector(leftSwipe:)];
    [leftSwipeGesture setDirection:(UISwipeGestureRecognizerDirectionLeft)];
    [self.scrollView addGestureRecognizer:leftSwipeGesture];
    UISwipeGestureRecognizer *rightSwipeGesture = [[UISwipeGestureRecognizer alloc] initWithTarget:self action:@selector(rightSwipe:)];
    [rightSwipeGesture setDirection:(UISwipeGestureRecognizerDirectionRight)];
    [self.scrollView addGestureRecognizer:rightSwipeGesture];
}
```

接着封装Pixiv图片的下载功能。这里会用到[PixivAPI_iOS](https://github.com/upbit/PixivAPI_iOS)的返回类型。因为P站有SAPI和Public-API两种API，返回也存在SAPIIllust和PAPIIllust两种。先定义一个通用的提取illust_id/title信息的函数：

```objective-c
- (NSDictionary *)_safeGetIllustBaseInfo:(NSArray *)illusts index:(NSInteger)index largeSize:(BOOL)largeSize
{
    if ((index < 0) || (index >= illusts.count)) {
        return nil;
    }

    NSInteger illust_id;
    NSString *image_url;
    NSString *title;

    id raw_illust = illusts[index];
    if ([raw_illust isKindOfClass:[NSDictionary class]]) {
        NSDictionary *illust = (NSDictionary *)raw_illust;
        illust_id = [illust[@"illust_id"] integerValue];
        if (largeSize) {
            image_url = illust[@"url_large"];
        } else {
            image_url = illust[@"url_px_480mw"];
        }
        title = illust[@"title"];
    } else if ([raw_illust isKindOfClass:[SAPIIllust class]]) {
        SAPIIllust *illust = (SAPIIllust *)raw_illust;
        illust_id = illust.illustId;
        image_url = illust.mobileURL;
        title = illust.title;
    } else if ([raw_illust isKindOfClass:[PAPIIllust class]]) {
        PAPIIllust *illust = (PAPIIllust *)raw_illust;
        illust_id = illust.illust_id;
        if (largeSize) {
            image_url = illust.true_url_large;
        } else {
            image_url = illust.url_px_480mw;
        }
        title = illust.title;
    } else {
        return nil;
    }

    // 封装成NSDictionary返回
    return @{
        @"index": @(index),
        @"illust_id": @(illust_id),
        @"image_url": image_url,
        @"title": title,
    };
}

- (NSDictionary *)illustRecordWithIndex:(NSInteger)index
{
    return [self _safeGetIllustBaseInfo:self.illusts index:index largeSize:self.showLargeSize];
}
```

接着实现异步下载图片到UIImageView的核心功能reloadImage:

```objective-c
// 图片下载进度通知
- (void)onImageProgress:(NSInteger)receivedSize expectedSize:(NSInteger)expectedSize
{
    //NSLog(@"download progress: %.2f%%", (float)receivedSize/expectedSize);
}

// 图片下载完成的回掉函数，供外部感知下载结束
- (void)onImageDownloaded:(UIImage *)image
{
    self.image = image;
}

- (void)realShowImageWithBaseInfo:(NSDictionary *)illust_record
{
    NSInteger illust_id = [illust_record[@"illust_id"] integerValue];
    NSString *image_url = illust_record[@"image_url"];

    NSLog(@"download(id=%ld): %@", (long)illust_id, image_url);

    [self simulatePixivRefererAndUserAgent:illust_id];

    __weak SDWebScrollImageViewController *weakSelf = self;
    [ApplicationDelegate setNetworkActivityIndicatorVisible:YES];

    [self.imageView sd_setImageWithURL:[NSURL URLWithString:image_url]
                      placeholderImage:nil options:(SDWebImageHighPriority|SDWebImageRetryFailed)
                              progress:^(NSInteger receivedSize, NSInteger expectedSize) {
                                  [weakSelf onImageProgress:receivedSize expectedSize:expectedSize];
                              }
                             completed:^(UIImage *image, NSError *error, SDImageCacheType cacheType, NSURL *imageURL) {
                                 if (error) {
                                     NSLog(@"download(id=%ld) error: %@", (long)illust_id, error);
                                 } else {
                                     NSLog(@"download(id=%ld) completed.", (long)illust_id);
                                 }

                                 dispatch_async(dispatch_get_main_queue(), ^{
                                     [ApplicationDelegate setNetworkActivityIndicatorVisible:NO];
                                     [weakSelf onImageDownloaded:image];
                                 });
                             }];
}

- (void)reloadImage
{
    NSDictionary *illust_record = [self illustRecordWithIndex:self.index];
    if (!illust_record) {
        //NSLog(@"safeGetIllustBaseInfo(%ld) error", (long)self.index);
        return;
    }
    [self realShowImageWithBaseInfo:illust_record];
}
```

1. 先用 illustRecordWithIndex: 获取illust的基本信息；
2. realShowImageWithBaseInfo: 中调用 sd_setImageWithURL:placeholderImage:completed: 异步下载图片；(这个函数没封装好，导致后面增加进度条时，不得不重写整个函数...)
3. completed block 中，在 main queue 上调用 onImageDownloaded: 回掉，更新image；(这里单独拿出来，是为了通知外部下载完成)

其中还有个 simulatePixivRefererAndUserAgent: 因为原图下载需要模拟从网页端的访问，每次下载前调整下Referer和UserAgent：

```objective-c
- (void)simulatePixivRefererAndUserAgent:(NSInteger)illust_id
{
    if (self.showLargeSize) {
        // 模拟Referer来下载原图
        NSString *referer = [NSString stringWithFormat:@"http://www.pixiv.net/member_illust.php?mode=medium&illust_id=%ld", (long)illust_id];
        NSString *user_agent = @"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.4 (KHTML, like Gecko) Ubuntu/12.10 Chromium/22.0.1229.94 Chrome/22.0.1229.94 Safari/537.4";
        [SDWebImageManager.sharedManager.imageDownloader setValue:referer forHTTPHeaderField:@"Referer"];
        [SDWebImageManager.sharedManager.imageDownloader setValue:user_agent forHTTPHeaderField:@"User-Agent"];
    } else {
        [SDWebImageManager.sharedManager.imageDownloader setValue:@"PixivIOSApp/5.1.1" forHTTPHeaderField:@"User-Agent"];
    }
}
```

完成了 reloadImage: 最后是手势响应，单击/左划跳转下一张，右划上一张，双击则是在原图与适合屏幕zoomScale中切换：

```objective-c
#pragma mark - Gesture Recognizer

- (void)singleTap:(UITapGestureRecognizer *)sender
{
    NSLog(@"singleTap");
    self.index = self.index + 1;
    [self reloadImage];
}

- (void)doubleTap:(UITapGestureRecognizer *)sender
{
    if (self.scrollView.zoomScale != 1.0) {
        self.scrollView.zoomScale = 1.0;
    } else {
        self.scrollView.zoomScale = self.lastZoomScale;
    }
}

- (void)leftSwipe:(UITapGestureRecognizer *)sender
{
    NSLog(@"leftSwipe");
    self.index = self.index + 1;
    [self reloadImage];
}

- (void)rightSwipe:(UITapGestureRecognizer *)sender
{
    NSLog(@"rightSwipe");
    self.index = self.index - 1;
    [self reloadImage];
}
```

导出相应的函数供外部重载，至此简易的Pixiv图片下载与显示类完成。(未完待续)
