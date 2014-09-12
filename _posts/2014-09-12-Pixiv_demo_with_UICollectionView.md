---
layout: post
title: 为PivivAPI的example，增加三列显示的UICollectionView及自动翻页
description: "PixivDaily demo with UICollectionView"
category: opensource
comments: true
share: true
---

昨晚更新iPad到7.1.2后，终于在iPad上试了下[PixivDaily](https://github.com/upbit/PixivAPI_iOS/tree/master/examples/PixivDaily)。说实话，UITableView用来显示图片确实太小，在iPad2上50x50像素简直看不清内容。于是花了一中午把UICollectionView加了进去。

和UITableView一样，先定义一个UICollectionViewController的父类，方便显示IllustModel：

```objective-c
@interface PixivIllustCollectionViewController : UICollectionViewController
@property (strong, nonatomic) NSArray *illusts;     // of IllustModel
@end
```

里面类似主要实现如下几个方法：
```objective-c
#pragma mark - UICollectionViewDataSource

- (NSInteger)collectionView:(UICollectionView *)collectionView numberOfItemsInSection:(NSInteger)section
{
    return [self.illusts count];
}

- (UICollectionViewCell *)collectionView:(UICollectionView *)collectionView cellForItemAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Image ColCell";
    UICollectionViewCell *cell = [collectionView dequeueReusableCellWithReuseIdentifier:CellIdentifier forIndexPath:indexPath];
    
    if ([self.illusts count] > 0)
    {
        IllustModel *illust = [self.illusts objectAtIndex:indexPath.row];
        
        UIImageView *imageView = [[UIImageView alloc] init];
        cell.backgroundView = imageView;
        
        // download illusts.thumbURL for cell image
        [imageView sd_setImageWithURL:[NSURL URLWithString:illust.thumbURL]
                     placeholderImage:[UIImage imageNamed:@"placeholder"] options:indexPath.row == 0 ? SDWebImageRefreshCached : 0];
    }
    
    return cell;
}
```

注意，因为只显示一张缩略图，collectionView:cellForItemAtIndexPath: 里并没有设置cell的大小，直接将 cell.backgroundView 设置为目标UIImageView。此时图片显示大小会根据 UICollectionView 的配置而显示。

```objective-c
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
    if ([sender isKindOfClass:[UICollectionViewCell class]]) {
        NSIndexPath *indexPath = [self.collectionView indexPathForCell:sender];
        if (indexPath) {
            if (([segue.identifier isEqualToString:@"Show Image"]) && ([segue.destinationViewController isKindOfClass:[PixivImageViewController class]])) {
                [segue.destinationViewController setHidesBottomBarWhenPushed:YES];
                [self prepareImageViewController:segue.destinationViewController toDisplayPhoto:self.illusts[indexPath.row] mobileSize:NO];
            }
        }
    }
}
```

至于 prepareImageViewController: 则和之前一样，用于准备SDWebImageViewController

再创建一个 DailyRankingCollectionViewController 继承于 PixivIllustCollectionViewController()，用于控制View显示日榜内容。唯一不同的是UICollectionView没有 willDisplayCell: 方法，需要自己在 collectionView:cellForItemAtIndexPath: 里进行判断。因此重载父类的该方法，在执行前先检查是否达到末尾：

```objective-c
- (UICollectionViewCell *)collectionView:(UICollectionView *)collectionView cellForItemAtIndexPath:(NSIndexPath *)indexPath
{
    if (indexPath.row == [self.illusts count]-1) {
        [self loadMoreIllusts];
    }
    
    return [super collectionView:collectionView cellForItemAtIndexPath:indexPath];
}
```

如果此时运行，会发现 UICollectionViewCell 只有默认的 50x50 像素，而且两两之间存在很粗的黑边。此时只需要在StoryBoard里配置 UICollectionView 的 Collection View Size，即可完成UICollectionView布局的定制：

![For フルーツバスケット]({{ site.url }}/images/collection_view_size_for_PixivDaily.png)

完成后的iPad布局如下：

![PixivDaily iPad Screenshot2](https://raw.github.com/upbit/PixivAPI_iOS/master/examples/screenshots/PixivDaily_04.png)

当然，如果是需要UICollectionView全屏显示或者让Cell自适应屏幕宽度，就需要用到 [UICollectionViewDelegateFlowLayout](https://developer.apple.com/library/ios/documentation/uikit/reference/UICollectionViewDelegateFlowLayout_protocol/Reference/Reference.html) 里的 collectionView:layout:sizeForItemAtIndexPath: 方法了，返回一个经过计算的 CGSize 来调整 UICollectionViewCell 的显示大小。

