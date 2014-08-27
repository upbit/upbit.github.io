---
layout: post
title: CS193p作业4完成基本功能，备忘下UITableView的Section的用法
description: "CS193p: assignment 4"
category: study
comments: true
share: true
---

作业4和第11讲演示的Shutterbug很类似，做起来较为简单。主要遇到下面两个难点：

1. UITableView增加索引: [flickr.places.getTopPlacesList](https://www.flickr.com/services/api/flickr.places.getTopPlacesList.html)返回的分组问题
2. RecentPhotos的存储: 要求是存储到NSUserDefaults，不过学着在AppDelegate里定义了全局变量，用来保存访问过的数据

先放图，再依次记录解决办法。

![Assignment 4 iPhone](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot4a.png)

![Assignment 4 iPad](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot4d.png)

----------------------

## UITableView增加索引

关于对UITableView数据增加字母顺序的索引，网上搜了不少教程但处理的都不是很好。有的是生成一个A-Z的固定NSDictionary(无法对应特殊字符)，有的则讲的云里雾里一运行就报错... 最后找到个[Implementing UITableView Sections from an NSArray of NSDictionary Objects](http://www.icodeblog.com/2010/12/10/implementing-uitableview-sections-from-an-nsarray-of-nsdictionary-objects/)，说的比较清晰。

这里对allKeys排序优化了下，增加首字母索引方法如下：

### 1. 当 [setPlaces()](https://github.com/upbit/CS193p_Homework/blob/e5652a0d5ef5458055ba67d5e567513de97a23ce/TopPlaces/TopPlaces/FlickrPlacesTVC.m#L60) 时，生成排序过的 NSDictionary *sectionPlaces

```objective-c
@interface FlickrPlacesTVC ()
@property (strong, nonatomic) NSArray *sortedSectionTitles;
@property (strong, nonatomic) NSDictionary *sectionPlaces;
@end
```

```objective-c
- (void)makeSectionDictionaryWithPlacesArray:(NSArray *)places
{
    NSMutableDictionary *sectionPlaces = [[NSMutableDictionary alloc] init];
    for (NSDictionary *place in places) {
        NSString *sectionTitle = [[place valueForKey:FLICKR_PLACE_NAME] substringToIndex:1];
        
        NSMutableArray *sectionArray = [sectionPlaces objectForKey:sectionTitle];
        if (!sectionArray) {
            [sectionPlaces setValue:[[NSMutableArray alloc] init] forKey:sectionTitle];
            sectionArray = [sectionPlaces objectForKey:sectionTitle];
        }
        
        [sectionArray addObject:place];
    }
    
    // Sort each section array
    for (NSString *key in sectionPlaces.allKeys) {
        NSMutableArray *sortedArray = [[[sectionPlaces objectForKey:key] sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            NSString *title1 = [obj1 valueForKey:FLICKR_PLACE_NAME];
            NSString *title2 = [obj2 valueForKey:FLICKR_PLACE_NAME];
            return [title1 compare:title2];
        }] mutableCopy];
        [sectionPlaces setValue:sortedArray forKey:key];
    }

    self.sectionPlaces = sectionPlaces;
}

- (void)setPlaces:(NSArray *)places
{
    _places = places;
    [self makeSectionDictionaryWithPlacesArray:places];
    [self.tableView reloadData];
}
```

关于makeSectionDictionaryWithPlacesArray()，这里先将每个place的首字母取出，检查NSDictionary中是否已有这个Key，没有则 [sectionPlaces setValue:[[NSMutableArray alloc] init] forKey:sectionTitle]。之后将place插入这个NSMutableArray的尾部。最后对每个key下的NSMutableArray排序，存储到self.sectionPlaces里。

### 2. 处理获取到的 Section Titles

其实简单的直接调用 self.sectionPlaces.allKeys 就能得到 sectionIndexTitlesForTableView() 用的NSArray了，不过这样做这些Titles就没有按字母大小排序。参考[教程里的方法](http://www.icodeblog.com/2010/12/10/implementing-uitableview-sections-from-an-nsarray-of-nsdictionary-objects/)，[self.sectionPlaces.allKeys sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)] 就是想要的结果。只不过每次这样排序性能上不知如何，想想还是存到NSArray里的好。

```objective-c
- (NSArray *)sortedSectionTitles
{
    if (!_sortedSectionTitles) _sortedSectionTitles = [[NSArray alloc] init];
    return _sortedSectionTitles;
}

- (void)setSectionPlaces:(NSDictionary *)sectionPlaces
{
    _sectionPlaces = sectionPlaces;
    self.sortedSectionTitles = [self.sectionPlaces.allKeys sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
}
```

当每次self.sectionPlaces更新后，将排序过的结果存储到self.sortedSectionTitles里。但因为初始化时sectionPlaces可能还没有内容，需要lazy init sortedSectionTitles为空的NSArray

### 3. 实现UITableView需要的几个函数

因为已经准备好了self.sortedSectionTitles，下面几个函数就好返回了：

```objective-c
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    // Return the number of sections.
    return [self.sortedSectionTitles count];
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section
{
    return [self.sortedSectionTitles objectAtIndex:section];
}

- (NSArray *)sectionIndexTitlesForTableView:(UITableView *)tableView
{
    return self.sortedSectionTitles;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    // Return the number of rows in the section.
    return [[self.sectionPlaces valueForKey:[self.sortedSectionTitles objectAtIndex:section]] count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Flickr Place Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
    
    NSDictionary *place = [self.sectionPlaces[self.sortedSectionTitles[indexPath.section]] objectAtIndex:indexPath.row];
    NSArray *locality = [[place valueForKey:FLICKR_PLACE_NAME] componentsSeparatedByString:@", "];
    cell.textLabel.text = [locality firstObject];
    cell.detailTextLabel.text = [locality componentsJoinedByString:@", "];
    
    return cell;
}
```

tableView: numberOfRowsInSection这个的处理比较巧妙，另外注意从indexPath里获取的内容，都需要改成这样：

```objective-c
NSDictionary *place = [self.sectionPlaces[self.sortedSectionTitles[indexPath.section]] objectAtIndex:indexPath.row];
```

**完整代码见 [TopPlaces/FlickrPlacesTVC.m](https://github.com/upbit/CS193p_Homework/blob/e5652a0d5ef5458055ba67d5e567513de97a23ce/TopPlaces/TopPlaces/FlickrPlacesTVC.m)**

----------------------

## RecentPhotos的存储

作业要求是存储到NSUserDefaults里，没看到结果用全局变量实现了... 先说思路吧，后面再来改。

因为需要存储访问过的照片，最先想到的是在 FlickrPhotosTVC.m 的 prepareImageViewController() 里，增加记录函数。但 RecentsViewController 却是 FlickrPhotosTVC 的子类，而且还有其他UITableViewController是继承于FlickrPhotosTVC的，在这里维护一个NSArray显然不是好办法。于是研究了下AppDelegate的全局变量访问方法。

```objective-c
@interface TopPlacesAppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (strong, nonatomic) NSArray *recentPhotos;

@end
```

在TopPlacesAppDelegate.h里定义recentPhotos，这种全局变量访问方法比较简单。

读：

```objective-c
#import "TopPlacesAppDelegate.h"

- (IBAction)fetchPhotos
{
    self.photos = ((TopPlacesAppDelegate *)[[UIApplication sharedApplication] delegate]).recentPhotos;
}
```

写：

```objective-c
static const int MAX_RECENT_PHOTO_NUM = 20;

- (void)addPhotoToRecentPhotosArray:(NSDictionary *)photo
{
    if (self.tableView.tag == TVC_TAG_IGNORE_VIEW_HISTORY)
        return;
    
    TopPlacesAppDelegate *appDelegate = [[UIApplication sharedApplication] delegate];
    NSMutableArray *recentPhotos = [[NSMutableArray alloc] initWithArray:appDelegate.recentPhotos];
    
    if (![recentPhotos containsObject:photo]) {
        NSLog(@"Add recent: '%@'", [photo valueForKeyPath:FLICKR_PHOTO_TITLE]);
        
        [recentPhotos insertObject:photo atIndex:0];
        appDelegate.recentPhotos = [recentPhotos subarrayWithRange:NSMakeRange(0, MIN([recentPhotos count], MAX_RECENT_PHOTO_NUM))];
    }
}
```

因为 RecentsViewController 里点击图片，也会调用 addPhotoToRecentPhotosArray()，只好用比较搓的办法 self.tableView.tag = TVC_TAG_IGNORE_VIEW_HISTORY，然后判断自己tableView.tag是否为TVC_TAG_IGNORE_VIEW_HISTORY来跳过。


## [Update SourceCode](https://github.com/upbit/CS193p_Homework/tree/bff7c92109567e811a331b954282d4f907001941/TopPlaces/TopPlaces)

修复Recents保存到NSUserDefaults的问题。UIImageView的自动缩小倒是很容易，暂时没做双击放大。

```objective-c
// Zoom to show as much image as possible
// http://stackoverflow.com/questions/14471298/zooming-uiimageview-inside-uiscrollview-with-autolayout
- (void)initZoom
{
    float minZoom = MIN(self.view.bounds.size.width / self.imageView.image.size.width,
                        self.view.bounds.size.height / self.imageView.image.size.height);
    if (minZoom > 1) return;
    
    self.scrollView.minimumZoomScale = minZoom;
    self.scrollView.zoomScale = minZoom;
}

- (void)setImage:(UIImage *)image
{
    ...

    [self initZoom];
}

- (void)didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation
{
    [self initZoom];
}
```

至此算是把作业4搞定了，另外吐槽下GFW，在国内访问Flickr真心不容易。而且因为cn.edit.yahoo.com无法访问，还要挂VPN注册Flickr帐号...
