---
layout: post
title: CS193p作业2完成，另外自己打造了个带同花顺匹配的版本，代码见文末
description: "CS193p: complete assignment 2"
category: study
comments: false
share: true
---

第二个作业难度确实陡增，整整花了一下午才搞定3张牌的匹配问题：

![Assignment 2 Screenshot](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot2.png)

大部分实现在[CardMatchingGame.m](https://github.com/upbit/CS193p_Homework/blob/fb53c021c455cf53d85341ce61f9fb20ae3c3b7f/Matchismo/Matchismo/Model/CardMatchingGame.m)里，直接[clone这个分支](https://github.com/upbit/CS193p_Homework/tree/fb53c021c455cf53d85341ce61f9fb20ae3c3b7f)查看吧

---

### 关于[Joker分支](https://github.com/upbit/CS193p_Homework/tree/joker)

玩了会觉得游戏略显单调，于是加了顺子和王的匹配。调整后的规则如下：

0. 随机分布 8,9,10,J,Q,K,A 以及2张Joker，一共30张牌
1. 三张为顺子（如 A,K,Q 或 8,9,10）且同花色，得64分；不同花色得24分；
2. 三张中rank相同，得32分；
3. 三张的花色相同，得8分；
4. Joker可以代替任意一张牌，且按最大分值组合。

![Screenshot](https://raw.github.com/upbit/CS193p_Homework/joker/screenshot/joker.png)


主要的[match函数](https://github.com/upbit/CS193p_Homework/blob/joker/Matchismo/Matchismo/Model/PlayingCard.m)实现。貌似还可以简化，不过由他去了→_→

```objective-c
- (NSInteger)match:(NSArray *)otherCards
{
    int score = 0;
    
    if ([otherCards count] == 1) {          // 2x model
        PlayingCard *otherCard = [otherCards firstObject];
        
        if ((self.rank > 0) && (otherCard.rank > 0)) {
            if (self.rank == otherCard.rank) {
                score += 4;
            } else if ([self.suit isEqualToString:otherCard.suit]) {
                score += 1;
            }
        } else {
            // joker match any cards
            score += 4;
        }
        
    } else if ([otherCards count] == 2) {     // 3x model
        // sort cards by rank
        NSSortDescriptor *sortDescriptor;
        sortDescriptor = [[NSSortDescriptor alloc] initWithKey:@"rank" ascending:YES];
        NSArray *sortDescriptors = [NSArray arrayWithObject:sortDescriptor];
        NSArray *sortedCards = [@[self, [otherCards firstObject], [otherCards lastObject]] sortedArrayUsingDescriptors:sortDescriptors];


        BOOL isStraight = NO;
        
        // 1. 检查排序后数组中joker的数量0-2
        NSUInteger jokerCount = 0;
        for (PlayingCard *card in sortedCards) {
            if (card.rank == 0) {
                jokerCount++;
                continue;
            }
        }

        PlayingCard *lowCard = sortedCards[jokerCount];
        PlayingCard *midCard = sortedCards[(jokerCount+2)/2];
        PlayingCard *highCard = sortedCards[2];
        NSUInteger rankDiff = highCard.rank - lowCard.rank;
        
        if (jokerCount == 0) {
            // 2. 如果joker为0，检查high-low的差值，不是2则肯定不为顺子
            if (rankDiff == 2) {
                // 为2时，检查牌的两两差值是否为1
                if (((midCard.rank-lowCard.rank) == 1) && ((highCard.rank-midCard.rank) == 1))
                    isStraight = YES;
            }
        } else if (jokerCount == 1) {
            // 3. 如果Joker为1，检查high-low差值是否为1或2，不是则不为顺子
            if ((rankDiff == 1) || (rankDiff == 2))
                isStraight = YES;
        } else if (jokerCount == 2) {
            // 4. 如果Joker为2，此时一定组成顺子
            isStraight = YES;
        }
        
        if (isStraight) {
            if (([lowCard.suit isEqualToString:midCard.suit]) && ([lowCard.suit isEqualToString:highCard.suit])) {
                score += 16;    // straight flush
            } else {
                score += 6;     // straight
            }
        } else if ((lowCard.rank == midCard.rank) && (lowCard.rank == highCard.rank)) {
            score += 8;         // same rank
        } else if (([lowCard.suit isEqualToString:midCard.suit]) && ([lowCard.suit isEqualToString:highCard.suit])) {
            score += 2;         // same suit
        }
    }
    
    return score;
}
```

**下载**：Github的[工程源码](https://github.com/upbit/CS193p_Homework/archive/joker.zip)

ps: 如果在PlayingCardDeck.m里将牌扩展到全部54张，难度就会大幅增加。
