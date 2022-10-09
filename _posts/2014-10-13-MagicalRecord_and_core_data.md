---
layout: post
title: MagicalRecord与CoreData - MagicalRecord使用笔记
description: "MagicalRecord and CoreData"
category: opensource
comments: true
share: true
---

因为需要本地存储Pixiv的返回内容，于是搜了下CoreData的写法，自然而然也就接触到了[MagicalRecord](https://github.com/magicalpanda/MagicalRecord)。MagicalRecord确实极大简化了CoreData的各种操作，但正因为基于CoreData，到最后还是没搞定按插入顺序返回查询结果的功能...

已经打算切换到 [SQLite + FMDB](https://github.com/ccgus/fmdb)，不过还是先记录下MagicalRecord的用法吧。

## MagicalRecord的配置

1. 首先是用CocoaPods安装MagicalRecord：**pod 'MagicalRecord', '~> 2.2'**
2. 引入 **CoreData+MagicalRecord.h**
3. 在 application:didFinishLaunchingWithOptions: 里加入初始化语句

```objective-c
#import <MagicalRecord/CoreData+MagicalRecord.h>

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Override point for customization after application launch.
    [MagicalRecord setupCoreDataStackWithAutoMigratingSqliteStoreNamed:@"PixixWalker.sqlite"];

    return YES;
}
```

## MagicalRecord的使用

### 插入数据

```objective-c
  // saveUsingCurrentThreadContextWithBlockAndWait: 是使用后台保存避免阻塞主线程
  [MagicalRecord saveUsingCurrentThreadContextWithBlockAndWait:^(NSManagedObjectContext *localContext) {

    // 先按 illust_id 查询 IllustBase 是否存在，不存在则用 MR_createInContext: 创建
    IllustBase *illust = [IllustBase MR_findFirstByAttribute:@"illust_id"
                                                   withValue:@(illustBaseInfo.illustId)];
    if (illust == nil) {
        NSLog(@"new illust=%ld: %@", illustBaseInfo.illustId, illustBaseInfo.title);
        illust = [IllustBase MR_createInContext:content];
    }

    // illust.score = illustBaseInfo.score;

  }];
```

### 查询与筛选

过滤出评分次数大于500，且平均分高于8.0分的作品：

```objective-c
    NSPredicate *filter = [NSPredicate predicateWithFormat:@"scored_count > %d AND score/scored_count > %f", 500, 8.0];
    NSArray *illustArray = [IllustBase MR_findAllSortedBy:@"score" ascending:NO withPredicate:filter];
    weakSelf.illusts = illustArray;
```

### 清空所有数据

```objective-c
    [MagicalRecord saveUsingCurrentThreadContextWithBlockAndWait:^(NSManagedObjectContext *localContext) {
        [IllustBase MR_truncateAllInContext:localContext];
    }];
```

### 计算存储空间

```objective-c
    NSError *error;
    NSString *yourSqlLitePath = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) lastObject] stringByAppendingPathComponent: @"PixixWalker/PixixWalker.sqlite"];
    NSDictionary *storeAttributesOfItemAtPath = [[NSFileManager defaultManager] attributesOfItemAtPath:yourSqlLitePath error:&error];
    NSNumber *entities = [IllustBase MR_numberOfEntities];

    NSLog(@">>> SQLite size %.2f MB of %@ entities.", [[storeAttributesOfItemAtPath valueForKey:@"NSFileSize"] floatValue]/1024/1024, entities);
```

也许是对CoreData不熟，或者是CoreData本身就不适合复杂查询，写起来总觉得怪怪的，想想要是加上黑名单和联合查询，估计头都大了... 想想还是重新学习FMDB算了，虽然又要折腾一阵，不过应该比CoreData用的更加顺手。
