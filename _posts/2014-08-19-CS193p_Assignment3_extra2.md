---
layout: post
title: 继续完善CS193p作业3的Extra Credit，发现多排序的Ranking处理起来还真不容易
description: "CS193p: assignment 3 Extra Credit completed"
category: study
comments: true
share: true
---

今天接着做作业3的Extra Credit部分，要求是实现一个Ranking页和Setting页。

Ranking可以存在NSUserDefaults里，但因为插入结果时需要排序（按最高分/最短时间/最新游戏），则必须实现一个GameResult对象。最先想到的问题是三个排行榜的存储方式，存成一个NSArray肯定是不行的，等游戏记录一多别说排序了，存在NSUserDefaults里的数据都会不断膨胀。

所以在调用storageGameResultToRanking()时，分别对三个排行榜插入本轮的数据，并截断为Top10写入NSUserDefaults里。一运行发现根本就跑不起来，查了下原因发现NSUserDefaults不能存储自定义类。最终在[stackoverflow](http://stackoverflow.com/questions/3000220/best-way-to-save-to-nsuserdefaults-for-custom-class)上找到个较好的处理办法，利用NSKeyedArchiver将自定义对象序列化为NSData存储。排行榜更新部分的代码如下：

```objective-c
+ (NSArray *)sortRankingByKeyAndOrder:(NSArray *)ranking rankingName:(NSString *)rankingName
{
    NSString *keySorted = @"";
    BOOL ascending = NO;
    
    if ([rankingName isEqualToString:RANK_LAST_PLAYED]) {
        keySorted = @"date";
    } else if ([rankingName isEqualToString:RANK_GAME_SCORE]) {
        keySorted = @"score";
    } else if ([rankingName isEqualToString:RANK_GAME_DURATION]) {
        keySorted = @"duration";
        ascending = YES;
    }
    
    NSSortDescriptor *sortDescriptor = [[NSSortDescriptor alloc] initWithKey:keySorted ascending:ascending];
    NSArray *sortDescriptors = [NSArray arrayWithObject:sortDescriptor];
    return [ranking sortedArrayUsingDescriptors:sortDescriptors];
}

+ (void)storageOneGameResult:(GameResult *)gameResult rankingName:(NSString *)rankingName
{
    // get GameRanking from NSUserDefaults
    NSString *keyRanking = [NSString stringWithFormat:@"%@_%@", gameResult.gameType, rankingName];
    NSArray *rawRanking = [[NSUserDefaults standardUserDefaults] objectForKey:keyRanking];
    
    // decode Ranking
    NSMutableArray *ranking = [[NSMutableArray alloc] init];
    if (rawRanking) {
        for (NSData *rawObject in rawRanking) {
            GameResult *record = (GameResult *)[NSKeyedUnarchiver unarchiveObjectWithData:rawObject];
            [ranking addObject:record];
        }
    }

    [ranking addObject:gameResult];
    
    // sort by key
    NSArray *sortedArray = [GameRanking sortRankingByKeyAndOrder:ranking rankingName:rankingName];
    NSArray *resultArray = [sortedArray subarrayWithRange:NSMakeRange(0, MIN([sortedArray count], RANKING_TOP_SIZE))];

    NSMutableArray *encodeArray = [[NSMutableArray alloc] init];
    for (GameResult *record in resultArray) {
        NSData *encodeObject = [NSKeyedArchiver archivedDataWithRootObject:record];
        [encodeArray addObject:encodeObject];
    }

    [[NSUserDefaults standardUserDefaults] setObject:encodeArray forKey:keyRanking];
}

+ (void)storageGameResultToRanking:(GameResult *)gameResult
{
    [GameRanking storageOneGameResult:gameResult rankingName:RANK_LAST_PLAYED];
    [GameRanking storageOneGameResult:gameResult rankingName:RANK_GAME_SCORE];
    [GameRanking storageOneGameResult:gameResult rankingName:RANK_GAME_DURATION];
    [[NSUserDefaults standardUserDefaults] synchronize];
}
```

另外，sortRankingByKeyAndOrder()需要导出用于排行榜中最高分和最短时间的加亮显示。不得不说NSAttributedString用的非常不顺手，折腾排行榜加亮又花了快2个小时，不过学到[NSRange rangeOfString:]的用法，以后对付上色的事情应该会轻松不少：

```objective-c
    NSString *rankingString = [[GameRanking getGlobalRankingByName:gameType rankingName:rankingName] componentsJoinedByString:@"\n"];
    self.rankingTextView.text = rankingString;
    
    // try highlight shortest game / highest score in array
    NSArray *rawRankingArray = [GameRanking getGlobalRawRankingByName:gameType rankingName:rankingName];
    if ([rawRankingArray count]) {
        NSMutableAttributedString *textViewAttributedText = [[NSMutableAttributedString alloc] initWithAttributedString:self.rankingTextView.textStorage];
        
        NSString *shortestGameLine = [[[GameRanking sortRankingByKeyAndOrder:rawRankingArray rankingName:RANK_GAME_DURATION] firstObject] description];
        [textViewAttributedText addAttributes:@{ NSStrokeWidthAttributeName : @-3,
                                                 NSStrokeColorAttributeName : [UIColor orangeColor] }
                                        range:[rankingString rangeOfString:shortestGameLine]];
        
        NSString *highestScoreLine = [[[GameRanking sortRankingByKeyAndOrder:rawRankingArray rankingName:RANK_GAME_SCORE] firstObject] description];
        [textViewAttributedText addAttributes:@{ NSForegroundColorAttributeName : [UIColor redColor] }
                                        range:[rankingString rangeOfString:highestScoreLine]];
        
        [self.rankingTextView setAttributedText:textViewAttributedText];
    }
```

先把rankingTextView的内容取到NSMutableAttributedString *textViewAttributedText，之后排序得到最高分和最短时间的两个记录的NSString，用rangeOfString搜索NSRange并标记之。最后加亮完，整个Ranking页是这个样子：

![Assignment 3 screenshut](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot3c.png)

至此，终于把Assignment3给全部搞定了:)
