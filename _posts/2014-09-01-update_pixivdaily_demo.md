---
layout: post
title: 继续完善PixivDaily，增加iPad布局和图片的单击、双击操作
description: "update demo project PixivDaily"
category: opensource
comments: true
share: true
---

### 为图片浏览增加手势: 双击切换缩放状态；单击隐藏标题

作为一个图片浏览应用，PixivDaily的功能实在太简单了，连最基本的双击改变缩放模式都没有。先尝试在StoryBoard里拖放UITapGestureRecognizer，操作很简单不过当复制ImageView时，绑定的Tap手势失效了。第二个View莫名其妙只能触发双击操作，百思不得其解。

于是手工在代码里添加单击、双击的手势：

```objective-c
- (void)singelTap:(UITapGestureRecognizer *)sender
{
    // 单击隐藏NavigationBar
    [self.navigationController setNavigationBarHidden:!self.navigationController.isNavigationBarHidden animated:YES];
}

- (void)doubleTap:(UITapGestureRecognizer *)sender
{
    // 双击改变缩放比例
    // height -> width -> 1.0
    if (self.scrollView.zoomScale == self.heightZoomScale) {
        self.scrollView.zoomScale = self.widthZoomScale;
    } else if (self.scrollView.zoomScale == self.widthZoomScale) {
        self.scrollView.zoomScale = 1.0;
    } else {
        self.scrollView.zoomScale = self.heightZoomScale;
    }
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self.scrollView addSubview:self.imageView];
    
    // single/double tap gesture
    UITapGestureRecognizer *singleTapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(singelTap:)];
    singleTapGesture.numberOfTapsRequired = 1;
    [self.scrollView addGestureRecognizer:singleTapGesture];

    UITapGestureRecognizer *doubleTapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(doubleTap:)];
    doubleTapGesture.numberOfTapsRequired = 2;
    [self.scrollView addGestureRecognizer:doubleTapGesture];

    [singleTapGesture requireGestureRecognizerToFail:doubleTapGesture];
}
```

这样SDWebImageViewController就能顺利响应双击操作了。不过绑定时需要注意，因为是在scrollView中添加的imageView，需要向scrollView上添加手势。而initWithTarget:要用self，因为是发送的[self singelTap]消息，不然就会报这个经典的运行期错误：

```objective-c
*** Terminating app due to uncaught exception 'NSInvalidArgumentException', reason: '-[UIScrollView singelTap:]: unrecognized selector sent to instance 0x8fc4840'
	2   CoreFoundation                      0x01c23243 -[NSObject(NSObject) doesNotRecognizeSelector:] + 275
```

### 图片宽度/高度自适应

在StoryBoard里拖iPad布局不难，之前因为都有处理 tableView:didSelectRowAtIndexPath:，直接复制就搞定了：

![PixivDaily iPad Screenshot](https://raw.github.com/upbit/PixivAPI_iOS/master/examples/screenshots/PixivDaily_03.png)

唯一没料到的是Zoom，怎么弄都不太对劲。本来想实现尽可能用图片填满屏幕，试了半天发现 maximumZoomScale 会影响缩放比例，不设置的话iPad上又不能填充满ScrollView。最后找到个比较搓的方法：

```objective-c
- (void) initZoom {
    float minZoom = MIN(self.view.bounds.size.width / self.imageView.image.size.width,
                        self.view.bounds.size.height / self.imageView.image.size.height);
    if (minZoom > 1) minZoom = 1.0;
    self.scrollView.minimumZoomScale = minZoom;
    
    self.widthZoomScale = self.view.bounds.size.width*_scrollView.maximumZoomScale / self.imageView.image.size.width;
    self.heightZoomScale = self.view.bounds.size.height*_scrollView.maximumZoomScale / self.imageView.image.size.height;
    
    NSLog(@"bound=%.2f scale=%.1f, image=%.2f", self.view.bounds.size.width, [UIScreen mainScreen].scale, self.imageView.image.size.width);
    NSLog(@"widthZoom=%.2f heightZoom=%.2f, minZoom=%.2f", self.widthZoomScale, self.heightZoomScale, minZoom);

    self.scrollView.zoomScale = self.widthZoomScale;
}
```

widthZoomScale / heightZoomScale 分别是宽度适应和高度适应的比例，计算时bounds.size要乘以maximumZoomScale。这个逻辑太奇怪，不过能跑就先这样把 (远目

### UITableView向下翻页

UITableView的向下翻页功能，一直没找到一个简单的实现方法(网上大多是增加一个Load More的Cell，伴随一堆零碎的插入代码)，只好退而求次不显示"Loading..."和小菊花了：

```objective-c
- (void)addPageRankingIllusts:(NSUInteger)page
{
    __weak DailyRankingViewController *weakSelf = self;
    [PixivFetcher API_getRanking:page mode:PIXIV_RANKING_MODE_DAY content:PIXIV_RANKING_CONTENT_ALL
                       onSuccess:^(NSArray *illusts, BOOL isIllust) {
                           [weakSelf.refreshControl endRefreshing];
                           weakSelf.illusts = [weakSelf.illusts arrayByAddingObjectsFromArray:illusts];
                       }
                       onFailure:^(NSURLResponse *response, NSInteger responseCode, NSData *data, NSError *connectionError) {
                           NSLog(@"[HTTP %d] %@", responseCode, connectionError);
                       }];
}

- (BOOL)loadMoreIllusts
{
    if (self.currentPage < MAX_FETCH_RANKING_PAGE_NUM) {
        self.currentPage++;
        NSLog(@"Load More - page %u", self.currentPage);
        [self addPageRankingIllusts:self.currentPage];
        return YES;
    } else {
        return NO;
    }
}

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (indexPath.row == [self.illusts count]-1) {
        [self loadMoreIllusts];
    }
}
```

首先增加 addPageRankingIllusts:，将获取到的指定页码数据，插入到self.illusts中。接着判断 tableView:willDisplayCell:forRowAtIndexPath: 中indexPath.row == [self.illusts count]-1，如果相等说明已经滑到最后一个cell。此时就可以获取翻页数据了。

### 导出图片到文件

这个比较简单，主要是注意iPad默认detail会显示导出按钮，需要判断illust是否有效。另外在主线程里导出2M的图片担心卡住，于是 dispatch_async() 再写到Documents/；最后 UIAlertView 则要回到main_queue再弹出，不然窗口就冻结了：

```objective-c
- (NSString *)documentsPathForFileName:(NSString *)name
{
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory,NSUserDomainMask, YES);
    NSString *documentsPath = [paths objectAtIndex:0];
    return [documentsPath stringByAppendingPathComponent:name];
}

- (IBAction)exportIllustToDocuments:(UIBarButtonItem *)sender
{
    if ((!self.illust) || (self.illust.illustId == PIXIV_ID_INVALID))
        return;
    
    NSString *illustName = [NSString stringWithFormat:@"illistid_%u.%@", self.illust.illustId, self.illust.ext];
    NSString *illustPath = [self documentsPathForFileName:illustName];
    NSLog(@"export: %@", illustPath);

    __weak SDWebImageViewController *weakSelf = self;
    dispatch_queue_t exportQueue = dispatch_queue_create("export illust", NULL);
    dispatch_async(exportQueue, ^{
        if ([weakSelf.illust.ext isEqualToString:@"png"]) {
            [UIImagePNGRepresentation(weakSelf.image) writeToFile:illustPath atomically:YES];
        } else {
            [UIImageJPEGRepresentation(weakSelf.image, 0.92) writeToFile:illustPath atomically:YES];
        }
        
        dispatch_async(dispatch_get_main_queue(), ^{
            UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:@"Export Success!"
                                                                message:illustName
                                                               delegate:self
                                                      cancelButtonTitle:nil
                                                      otherButtonTitles:@"OK", nil];
            [alertView show];
        });
    });
}
```
