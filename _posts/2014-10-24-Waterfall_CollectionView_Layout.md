---
layout: post
title: 瀑布流Waterfall(CHTCollectionViewWaterfallLayout)笔记
description: "Waterfall - CHTCollectionViewWaterfallLayout"
category: study
comments: true
share: true
---

好久没更新博客了，想想还是把快忘掉的[CHTCollectionViewWaterfallLayout](https://github.com/chiahsien/CHTCollectionViewWaterfallLayout)拿出来备忘下，以便以后使用。

先上最终效果，这是显示的Pixiv的日榜结果(没有使用非等高排列)，配合xib可以方便调整Cell内的样式与布局：

![Waterfall example]({{ site.url }}/images/CHTCollectionViewWaterfallLayout_example.png)

## 瀑布流使用笔记

首先CocosPods安装最新的CHTCollectionViewWaterfallLayout

```
pod 'CHTCollectionViewWaterfallLayout', :head
```

### CHTCollectionViewWaterfallLayout的初始化

之后创建一个WaterfallViewController，在.m中引用头文件以及CHTCollectionViewDelegateWaterfallLayout：

```objective-c
#import <CHTCollectionViewWaterfallLayout/CHTCollectionViewWaterfallLayout.h>

@interface WaterfallViewController () <UICollectionViewDataSource, CHTCollectionViewDelegateWaterfallLayout>

@end
```

在Storyboard里拖放一个UICollectionViewController，因为后面使用xib确定Cell内容，这里先删掉里面的Cell。接着选定WaterfallViewController，开始CHTCollectionViewWaterfallLayout的配置：

先设置

```objective-c
- (void)viewDidLoad
{
    [super viewDidLoad];

    CHTCollectionViewWaterfallLayout *layout = [[CHTCollectionViewWaterfallLayout alloc] init];

    layout.sectionInset = UIEdgeInsetsMake(5, 2, 5, 2);     // top, left, bottom, right边距
    layout.minimumColumnSpacing = 1;                        // Cell之间1px
    layout.minimumInteritemSpacing = 1;
    // 为了自适应横竖屏，layout.columnCount的设置在后面单独说明

    self.collectionView.collectionViewLayout = layout;
    self.collectionView.autoresizingMask = UIViewAutoresizingFlexibleHeight | UIViewAutoresizingFlexibleWidth;
    self.collectionView.dataSource = self;
    self.collectionView.delegate = self;

    // 注册自定义Cell: CHTCollectionViewCell
    [self.collectionView registerClass:[CHTCollectionViewCell class] forCellWithReuseIdentifier:CELL_IDENTIFIER];
}
```

### 自定义CHTCollectionViewCell

接着创建一个UICollectionViewCell的子类CHTCollectionViewCell，勾选生成xib，粘贴下面的初始化代码：

```objective-c
//
//  CHTCollectionViewCell.h
//

#import <UIKit/UIKit.h>

#define CELL_IDENTIFIER @"WaterfallCell"

@interface CHTCollectionViewCell : UICollectionViewCell

@property (weak, nonatomic) IBOutlet UIImageView *image;
@property (weak, nonatomic) IBOutlet UILabel *label;

@end
```

```objective-c
//
//  CHTCollectionViewCell.m
//

#import "CHTCollectionViewCell.h"

@implementation CHTCollectionViewCell

- (instancetype)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        NSString *xibName = @"CHTCollectionViewCell";
        NSArray *arrayOfViews = [[NSBundle mainBundle] loadNibNamed:xibName owner:self options:nil];
        if (([arrayOfViews count] < 1) || (![[arrayOfViews objectAtIndex:0] isKindOfClass:[UICollectionViewCell class]])) {
            return nil;
        }
        self = [arrayOfViews objectAtIndex:0];
    }
    return self;
}

@end
```

接着在xib里拖放UIImageView和UILabel，添加AutoSize的约束如下：

![Waterfall Cell Storyboard]({{ site.url }}/images/CHTCollectionViewWaterfallLayout_cell_storyboard.png)

以后调整Cell就可以直接在Storyboard里修改xib了。需要注意的是，Cell的大小在这里修改是没用的，所以无论UIImageView还是UILabel，都需要自动布局适应外框大小。

### 处理Waterfall的数据源

因为是基于UICollectionView的，在添加 collectionView:cellForItemAtIndexPath: 前，先将数据存储定义好：

```objective-c
@interface WaterfallViewController : UICollectionViewController
@property (strong, nonatomic) NSArray *illusts;
@end
```

当外部设置好illusts后，触发 [self.collectionView reloadData] 来更新数据：

```objective-c
#pragma mark - UICollectionViewDataSource

- (NSInteger)collectionView:(UICollectionView *)collectionView numberOfItemsInSection:(NSInteger)section
{
    return [self.illusts count];
}

- (NSInteger)numberOfSectionsInCollectionView:(UICollectionView *)collectionView
{
    return 1;
}

- (UICollectionViewCell *)collectionView:(UICollectionView *)collectionView cellForItemAtIndexPath:(NSIndexPath *)indexPath
{
    CHTCollectionViewCell *cell = (CHTCollectionViewCell *)[collectionView dequeueReusableCellWithReuseIdentifier:CELL_IDENTIFIER forIndexPath:indexPath];

    ///
    ///  在此设置 cell.image 和 cell.label
    ///
/*
    id raw_illust = self.illusts[indexPath.row];
    NSString *image_url = nil;

    if ([raw_illust isKindOfClass:[PAPIIllust class]]) {
        PAPIIllust *illust = (PAPIIllust *)raw_illust;
        cell.label.text = illust.title;
        image_url = illust.url_px_128x128;

    } else if ([raw_illust isKindOfClass:[SAPIIllust class]]) {
        SAPIIllust *illust = (SAPIIllust *)raw_illust;
        cell.label.text = illust.title;
        image_url = illust.thumbURL;

    } else {
        cell.label.text = @"unhandle class";
    }

    if (image_url) {
        [cell.image sd_setImageWithURL:[NSURL URLWithString:image_url]
                      placeholderImage:[UIImage imageNamed:@"placeholder"] options:indexPath.row == 0 ? SDWebImageRefreshCached : 0];
    }
*/
    return cell;
}
```

除了常规的cell设置外，还有CHTCollectionViewDelegateWaterfallLayout的参数需要调整：

```objective-c
#pragma mark - CHTCollectionViewDelegateWaterfallLayout

// this method asks for the size of cell at indexpath
- (CGSize)collectionView:(UICollectionView *)collectionView layout:(UICollectionViewLayout *)collectionViewLayout sizeForItemAtIndexPath:(NSIndexPath *)indexPath
{
    // 这里返回不一样的长宽比，cell就会根据列数和相对高度自动排列
    CGSize size = CGSizeMake(50, 50);     // 50x50只代表比例，实际Cell的显示大小由columnCount与屏宽决定
    return size;
}

// this method is called when a cell is selected (tapped on)
- (void)collectionView:(UICollectionView *)collectionView didSelectItemAtIndexPath:(NSIndexPath *)indexPath
{
    // 选中某个Cell的处理方法
    NSLog(@"Cell at %ld is selected", (long)[indexPath row]);
}
```

此时运行，就会显示出columnCount=2的瀑布流了。

### 根据屏幕宽度自动调整columnCount

那位说了，我iPhone上和iPad上，看到的怎么都是2列啊，还有横屏时明明可以显示更多，却把图片给拉伸了... 其实还有个处理没做——**根据屏幕宽度自动调整columnCount**

在View显示或者设备发生旋转时

```objective-c
#define __MainScreenFrame   [[UIScreen mainScreen] bounds]
#define __MainScreen_Width  __MainScreenFrame.size.width

// Cell的最小显示大小(决定列数)
#define MIN_CELL_COLUMN_SIZE (96)

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    [self updateLayoutForOrientation:[UIApplication sharedApplication].statusBarOrientation];
}

- (void)willAnimateRotationToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration
{
    [super willAnimateRotationToInterfaceOrientation:toInterfaceOrientation duration:duration];
    [self updateLayoutForOrientation:toInterfaceOrientation];
}

- (void)updateLayoutForOrientation:(UIInterfaceOrientation)orientation
{
    CHTCollectionViewWaterfallLayout *layout = (CHTCollectionViewWaterfallLayout *)self.collectionView.collectionViewLayout;
    layout.columnCount = __MainScreen_Width / MIN_CELL_COLUMN_SIZE;
}
```

关键的就是 updateLayoutForOrientation: 函数，只要必要时根据 __MainScreen_Width 返回的屏幕宽度，以及Cell的最小宽度，就可以知道当前设备、方向下应该显示的瀑布流列数了。

### 关于Header和Footer

为了简便起见(其实是偷懒)，这里省去了相关的设置。定义好Header和Footer Cell后，在viewDidLoad:里注册Cell Class，接着处理 collectionView:viewForSupplementaryElementOfKind:atIndexPath:

```objective-c
- (void)viewDidLoad
{
    // ...

    [self.collectionView registerClass:[CHTCollectionViewWaterfallHeader class]
        forSupplementaryViewOfKind:CHTCollectionElementKindSectionHeader withReuseIdentifier:HEADER_IDENTIFIER];
    [self.collectionView registerClass:[CHTCollectionViewWaterfallFooter class]
        forSupplementaryViewOfKind:CHTCollectionElementKindSectionFooter withReuseIdentifier:FOOTER_IDENTIFIER];
}

// this method will ask for supplementary views - headers and footers - for each section
- (UICollectionReusableView *)collectionView:(UICollectionView *)collectionView
           viewForSupplementaryElementOfKind:(NSString *)kind atIndexPath:(NSIndexPath *)indexPath
{
    if ([kind isEqualToString:CHTCollectionElementKindSectionHeader]) {
        CHTCollectionViewHeader *headerCell = (CHTCollectionViewHeader*)[collectionView
            dequeueReusableSupplementaryViewOfKind:kind withReuseIdentifier:HEADER_IDENTIFIER forIndexPath:indexPath];
        headerCell.label.text = @"example title";
        headerCell.image.image = [UIImage imageNamed:@"example"];
        return headerCell;
    } else if ([kind isEqualToString:CHTCollectionElementKindSectionFooter]) {
        // ...
    }
    return nil;
}
```

CHTCollectionViewWaterfallLayout就这么多东西了，虽然上手起来有点不易，不过效果还是很不错的。未来做其他资讯类的app时应该也能用上。
