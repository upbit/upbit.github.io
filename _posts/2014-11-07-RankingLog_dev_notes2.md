---
layout: post
title: Pixiv RankingLog for iOS 开发手记02 (XCode6/iOS8.1)
description: "Pixiv RankingLog for iOS development notes 02 (XCode6/iOS8.1)"
category: opensource
comments: true
share: true
---

继续[Pixiv RankingLog for iOS 开发手记01](http://blog.imaou.com/opensource/2014/11/07/RankingLog_dev_notes1.html)，这篇重点放在StoryBoard中遇到的问题。

![StoryBoard]({{ site.url }}/images/201411/dev_RankingLog_03.png)

这是RankingLog的StoryBoard全貌，首先是从RankingLogWaterfallViewController的主面板，其中一个DatePickerSegue跳转DatePickerViewController。另外两个跳转路径则是左上角的收藏按钮BookmarkSegue，以及点击Cell时的ImageDetail：

```objective-c
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"ImageDetail"]) {
        if ([segue.destinationViewController isKindOfClass:[PixivDetailScrollImageViewController class]]) {
            PixivDetailScrollImageViewController *ivc = (PixivDetailScrollImageViewController *)segue.destinationViewController;
            NSArray *indexPaths = [self.collectionView indexPathsForSelectedItems];
            NSIndexPath *indexPath = [indexPaths objectAtIndex:0];
            ivc.illusts = self.illusts;
            ivc.index = indexPath.row;
        }

    } else if ([segue.identifier isEqualToString:@"DatePickerSegue"]) {
        if ([segue.destinationViewController isKindOfClass:[DatePickerViewController class]]) {
            DatePickerViewController *dpvc = (DatePickerViewController *)segue.destinationViewController;
            // modeArray for RankingLog
            dpvc.modeArray = @[
                @"daily", @"weekly", @"monthly", @"male", @"female", @"rookie",
                @"daily_r18", @"weekly_r18", @"male_r18", @"female_r18", @"r18g",
            ];
        }

    } else if ([segue.identifier isEqualToString:@"BookmarkSegue"]) {
        if ([segue.destinationViewController isKindOfClass:[BookmarksWaterfallViewController class]]) {
            BookmarksWaterfallViewController *bvc = (BookmarksWaterfallViewController *)segue.destinationViewController;
            bvc.user_id = [PixivAPI sharedInstance].user_id;
        }

    }
}
```

## PixivDetailScrollImageViewController

直接从自定义Cell中拖一个Segue到 SDWebScrollImageViewController 控制的视图，这是在StoryBoard里自定义的优势。不过用SDWebScrollImageViewController显示具体图片还略显不足，定义一个PixivDetailScrollImageViewController继承于SDWebScrollImageViewController。这里需要处理的内容有如下几个：

1. 处理SAPIIllust到PAPIIllust的转换（SAPIIllust是获取不到大图地址的，也没有收藏ID等详细信息）；
2. 重载 reloadImage: 增加图片预载；
3. 响应导出按钮的点击，以及实现单击全屏显示图片；
4. 增加一个ContainerView，用于显示作者信息/Tags以及收藏按钮；

### 处理SAPIIllust到PAPIIllust的转换

先覆盖掉父类的 reloadImage: 异步的在下载图片前，检查并尝试获取PAPIIllust内容。定义一个replaceSAPIIllustToPAPIIllustAtIndex:函数，将指定index位置的SAPIIllust，调用 PAPI_works: 后替换到self.illusts的index位置：

```objective-c
- (BOOL)replaceSAPIIllustToPAPIIllustAtIndex:(NSInteger)index
{
    id raw_illust = self.illusts[index];
    if ((self.showLargeSize) && ([raw_illust isKindOfClass:[SAPIIllust class]])) {
        SAPIIllust *SAPI_illust = (SAPIIllust *)raw_illust;
        PAPIIllust *PAPI_illust = [[PixivAPI sharedInstance] PAPI_works:SAPI_illust.illustId];
        if (PAPI_illust) {
            NSMutableArray *new_illusts = [[NSMutableArray alloc] initWithArray:self.illusts];
            [new_illusts replaceObjectAtIndex:index withObject:PAPI_illust];
            self.illusts = new_illusts;
        }
        return YES;
    }
    return NO;
}

- (void)reloadImage
{
    __weak PixivDetailScrollImageViewController *weakSelf = self;
    [[PixivAPI sharedInstance] asyncBlockingQueue:^{
        [weakSelf replaceSAPIIllustToPAPIIllustAtIndex:weakSelf.index];

        [[PixivAPI sharedInstance] onMainQueue:^{
            NSDictionary *illust_record = [weakSelf illustRecordWithIndex:weakSelf.index];
            if (!illust_record) {
                return;
            }
            [weakSelf realShowImageWithBaseInfo:illust_record];
        }];
    }];
}
```

之后就是按常规流程，回到main queue上调用 realShowImageWithBaseInfo: 下载原图。
这里用 asyncBlockingQueue: 将 replaceSAPIIllustToPAPIIllustAtIndex: 异步化，是为了保证获取PAPIIllust和下载图片依次执行，并且不会阻塞主线程。另外在block中调用super而不引起循环引用貌似很复杂，索性干脆重写了reloadImage。

### 重载 reloadImage: 增加图片预载

reloadImage:的事情还没完，因为是图片浏览类应用，提前预载1-2张图片是必备的功能。定义preloadImageWithBaseInfo:index:用来添加指定图片到下载队列，然后在下载当前index图片同时，异步的发起后2张图的预载：

```objective-c
- (void)preloadImageWithBaseInfo:(NSDictionary *)illust_record index:(NSInteger)index
{
    NSInteger illust_id = [illust_record[@"illust_id"] integerValue];
    NSString *image_url = illust_record[@"image_url"];
    NSString *title = illust_record[@"title"];

    NSLog(@" preload(%@, id=%ld): %@", title, (long)illust_id, image_url);

    [self simulatePixivRefererAndUserAgent:illust_id];

    __weak PixivDetailScrollImageViewController *weakSelf = self;
    SDWebImageManager *manager = [SDWebImageManager sharedManager];
    [manager downloadImageWithURL:[NSURL URLWithString:image_url] options:0
                         progress:^(NSInteger receivedSize, NSInteger expectedSize) {
                             NSLog(@" preload id=%ld: %.1f%%", (long)illust_id, (float)receivedSize/expectedSize*100);
                         }
                        completed:^(UIImage *image, NSError *error, SDImageCacheType cacheType, BOOL finished, NSURL *imageURL) {
                            NSLog(@" preload id=%ld: completed", (long)illust_id);
                        }];
}

- (void)reloadImage
{
    __weak PixivDetailScrollImageViewController *weakSelf = self;
    ...

    // preload next 2 illust
    for (NSInteger i = 1; i <= 2; i++) {
        if (self.index+i >= self.illusts.count) {
            // TO-DO: fetch next page here
            continue;
        }

        [[PixivAPI sharedInstance] asyncBlockingQueue:^{
            [weakSelf replaceSAPIIllustToPAPIIllustAtIndex:weakSelf.index+i];

            [[PixivAPI sharedInstance] onMainQueue:^{
                NSDictionary *preload_record = [weakSelf illustRecordWithIndex:weakSelf.index+i];
                if (!preload_record) {
                    NSLog(@"safeGetIllustBaseInfo(%ld) error", (long)(weakSelf.index+i));
                    return;
                }
                [weakSelf preloadImageWithBaseInfo:preload_record index:i];
            }];
        }];
    }
}
```

这里最初是用的UIImageView，这样就可以像Pixiv客户端那样，滑动时有个切入效果。不过这么做UI会变得相当复杂，这里就简单的用了SDWebImageManager。需要注意的是，SDWebImage默认queue中同时只有2个图片在下载，因此下载原图时需要将options设上SDWebImageHighPriority避免翻页后迟迟得不到响应。

这里还留了个坑，在查看图片时如果illusts列表已经翻到尾部，必须退回到Waterfall下才会继续获取。如果最初将MVC思想贯彻得彻底点，将翻页获取写入Model中，就可以在缺少预载内容时，在图片浏览状态下同步发起翻页了。

### 响应导出按钮的点击等UI操作

先说最简单的单击全屏吧，覆盖掉父类的singleTap:即可。隐藏后记得updateZoom:一下，重新使图片适应新的高度：

```objective-c
- (void)singleTap:(UITapGestureRecognizer *)sender
{
    [self.navigationController setNavigationBarHidden:!self.navigationController.isNavigationBarHidden animated:YES];

    if (self.navigationController.isNavigationBarHidden) {
        [self.contantButtomView setHidden:YES];
    } else {
        [self.contantButtomView setHidden:NO];
    }

    [self updateZoom];
}
```

导出则分为两个部分，一个是导出图片到Documents/下，另一个则是导出到相册。

导出图片到文件比较简单，判断图片后缀名然后 UIImagePNGRepresentation(image) writeToFile:atomically: 或 UIImageJPEGRepresentation(image, 清晰度) writeToFile:atomically: 即可。将操作异步到后台执行，最后用SVProgressHUD显示个提示：

```objective-c
- (void)exportImageToDocuments:(UIImage *)image filename:(NSString *)filename ext:(NSString *)ext
{
    NSLog(@"export to: %@", [self documentsPathForFileName:filename]);

    [SVProgressHUD showWithStatus:[NSString stringWithFormat:@"Export '%@' to Documents", filename]];

    dispatch_queue_t exportQueue = dispatch_queue_create("export illust", NULL);
    dispatch_async(exportQueue, ^{
        // export to Documents/
        BOOL success;
        if ([ext isEqualToString:@"png"]) {
            success = [UIImagePNGRepresentation(image) writeToFile:[self documentsPathForFileName:filename] atomically:YES];
        } else {
            success = [UIImageJPEGRepresentation(image, 0.92) writeToFile:[self documentsPathForFileName:filename] atomically:YES];
        }

        dispatch_async(dispatch_get_main_queue(), ^{
            if (!success) {
                [SVProgressHUD showErrorWithStatus:[NSString stringWithFormat:@"Export %@ to Documents/ failed.", filename]];
            } else {
                [SVProgressHUD dismiss];
            }
        });
    });
}
```

导出到相册则需要用到ALAssetsLibrary，首先需要 #import <AssetsLibrary/AssetsLibrary.h>，接着调用 writeImageToSavedPhotosAlbum:orientation:completionBlock: 来异步的导出到相册。不过这个操作在我的iPad2上有点慢，较大的图片SVProgressHUD的圈会转很久

```objective-c
- (void)exportImageToPhotosAlbum:(UIImage *)image filename:(NSString *)filename
{
    [SVProgressHUD showWithStatus:[NSString stringWithFormat:@"Export '%@' to Photos Album", filename]];

    // export to Photos Album
    ALAssetsLibrary *library = [[ALAssetsLibrary alloc] init];
    [library writeImageToSavedPhotosAlbum:[image CGImage]
                              orientation:(ALAssetOrientation)[image imageOrientation]
                          completionBlock:^(NSURL *assetURL, NSError *error) {
                              // on main queue
                              dispatch_async(dispatch_get_main_queue(), ^{
                                  if (error) {
                                      [SVProgressHUD showErrorWithStatus:[NSString stringWithFormat:@"Export error: %@", [error localizedDescription]]];
                                  } else {
                                      [SVProgressHUD dismiss];
                                  }
                              });
                          }];
}
```

### ContainerView的更新与Segue

说实话，头一次接触ContainerView，开始觉得一头雾水，最无法接受的是Embed View内的Segue是Embed View自己跳转，目前我还没找到不用代码让包含ContainerView的父窗口执行Segue的方法。总之，最后是实现了想要的效果：点击作者的头像和姓名，让整个窗口切换到该作者的作品列表页。

Segue名为embedView的就是ContainerView：

```objective-c
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"embedView"]) {
        DetailInfoContainerViewController *dicvc = (DetailInfoContainerViewController *)segue.destinationViewController;
        dicvc.illust = self.illusts[self.index];

    } else if ([segue.identifier isEqualToString:@"UserWorksSegue"]) {
        UserWorksWaterfallViewController *uwvc = (UserWorksWaterfallViewController *)segue.destinationViewController;
        id raw_illust = self.illusts[self.index];
        if ([raw_illust isKindOfClass:[SAPIIllust class]]) {
            SAPIIllust *illust = (SAPIIllust *)raw_illust;
            uwvc.author_id = illust.authorId;
        } else if ([raw_illust isKindOfClass:[PAPIIllust class]]) {
            PAPIIllust *illust = (PAPIIllust *)raw_illust;
            uwvc.author_id =  illust.author_id;
        } else {
            NSLog(@"unknow illust %@ type at index %ld", raw_illust, (long)self.index);
            uwvc.author_id = 0;
        }
    }
}
```

不过ContainerView的数据更新略蛋疼，prepareForSegue:只能在第一次显示时调用，之后需要这样来更新：

```objective-c
- (DetailInfoContainerViewController *)_embedViewController
{
    return (DetailInfoContainerViewController *)[self.childViewControllers firstObject];
}

- (void)reloadImage
{
    __weak PixivDetailScrollImageViewController *weakSelf = self;
    [[PixivAPI sharedInstance] asyncBlockingQueue:^{
        [weakSelf replaceSAPIIllustToPAPIIllustAtIndex:weakSelf.index];

        [[PixivAPI sharedInstance] onMainQueue:^{
            ...

            // update embedView for PAPIIllust
            DetailInfoContainerViewController *dicvc = [weakSelf _embedViewController];
            if (dicvc) {
                dicvc.illust = weakSelf.illusts[weakSelf.index];
                [dicvc updateEmbedView];
            }
        }];
    }];
}
```

先用 self.childViewControllers firstObject 获取第一个ChildView，当然这里写的不严谨，没有检查Class也没有遍历ChildView，不过这样确实能取到ContainerView的实例。

再说上面的Segue处理，既然无法用StoryBoard绑定在ContainerView中，就在父页里埋一个看不见的Segue供ContainerView调用吧。在父页里拖一个UIButton，并命名其上的Segue为UserWorksSegue：

![UserWorksSegue]({{ site.url }}/images/201411/dev_RankingLog_04.png)

注意窗口左下脚的小方框，那个UIButton就藏在ContainerView的底下，并且设置成隐藏状态。父页有了Segue就来考虑怎么触发，在ContainerView的viewDidLoad中：

```objective-c
- (void)viewDidLoad
{
    [super viewDidLoad];
    ...

    // 为带有 WithTapGesture 的View，增加Tap手势
    NSRange range = [self.restorationIdentifier rangeOfString:@"WithTapGesture" options:NSBackwardsSearch];
    if (range.length > 0) {
        self.image.userInteractionEnabled = YES;
        [self.image addGestureRecognizer:[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(singleTap:)]];
        self.label.userInteractionEnabled = YES;
        [self.label addGestureRecognizer:[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(singleTap:)]];
    }
}

#pragma mark - Gesture Recognizer

- (void)singleTap:(UITapGestureRecognizer *)sender
{
    NSLog(@"singleTap");
    [self.parentViewController performSegueWithIdentifier:@"UserWorksSegue" sender:self];
}
```

增加单击手势，其中调用 parentViewController performSegueWithIdentifier:sender: 触发跳转。

### ContainerView中的进度条更新

载入图片总是有个等待时间，可以像Pixiv客户端那样在中间显示进度条，不过我偏好在底部的详细信息区域显示。进度条有2个，一个蓝色的显示当前图片的下载进度，另一个绿色的则显示预载情况，这些都需要在父页的 onImageProgress:expectedSize: 和预载函数中更新。将之前NSLog函数替换为下面这样：

```objective-c
- (void)onImageProgress:(NSInteger)receivedSize expectedSize:(NSInteger)expectedSize
{
    //NSLog(@"download id=%ld: %.1f%%", (long)illust_id, (float)receivedSize/expectedSize*100);
    [[self _embedViewController] updateDownloadProgress:(float)receivedSize/expectedSize];
}

- (void)preloadImageWithBaseInfo:(NSDictionary *)illust_record index:(NSInteger)index
{
    NSInteger illust_id = [illust_record[@"illust_id"] integerValue];
    NSString *image_url = illust_record[@"image_url"];
    NSString *title = illust_record[@"title"];

    NSLog(@" preload(%@, id=%ld): %@", title, (long)illust_id, image_url);

    [self simulatePixivRefererAndUserAgent:illust_id];

    __weak PixivDetailScrollImageViewController *weakSelf = self;
    SDWebImageManager *manager = [SDWebImageManager sharedManager];
    [manager downloadImageWithURL:[NSURL URLWithString:image_url] options:0
                         progress:^(NSInteger receivedSize, NSInteger expectedSize) {
                             //NSLog(@" preload id=%ld: %.1f%%", (long)illust_id, (float)receivedSize/expectedSize*100);
                             [[weakSelf _embedViewController] updatePreloadProgress:(float)receivedSize/expectedSize];
                         }
                        completed:^(UIImage *image, NSError *error, SDImageCacheType cacheType, BOOL finished, NSURL *imageURL) {
                            NSLog(@" preload id=%ld: completed", (long)illust_id);
                            [[weakSelf _embedViewController] updatePreloadProgress:-1.0];
                        }];
}
```

注意，这里没有在main queue中更新进度条，我的理解上UI操作应该都在main queue上进行，才不会导致延迟或异常。不过也许进度条对人的感知不是那么强烈，不在main queue上跑也感知不到有什么不同。另外就是在completed时更新为-1.0，这里hack了下负值代表隐藏进度条。

另外还有个问题实际上每解决，因为预载的是2张图片，各自下载的进度并非相加，而是同时更新绿色那个进度条的。好在SDWebImage同时只有2个工作，而往往也是主线程和预载的其中一张图片在下载，只要不翻的很快是不会出现绿色进度条来回跳变现象的。另外还有个问题是 preloadImageWithBaseInfo: 循环调用的太快，导致有时是间隔而非顺序预载图片的，有功夫再来把它改成顺序加载吧。

剩下的收藏列表与作品列表抓取，和过去排行页面大同小异。至此，RankingLog中解决和未解决的问题都梳理在这了。最后不得不感叹XCode6的 Size Class 和 Auto Layout 的便捷性，以前哪怕Delphi的编辑器，遇上自动布局的事也照样会弄得人一头包，那时还不用考虑不同屏幕高宽的适配。
