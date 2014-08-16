---
layout: post
title: 痛苦不堪的实现了CS193p作业3的基本要求，但是各种坑
description: "CS193p: upload assignment 3"
category: study
comments: false
share: true
---

陆陆续续花了2天，才功能上完成了作业3的要求。。。

题目要求是完成一个叫Set的游戏，根据[wiki](http://en.wikipedia.org/wiki/Set_(game))得知只有满足以下全部4个规则，才能组成Set：

1. They all have the same number, or they have three different numbers.
2. They all have the same symbol, or they have three different symbols.
3. They all have the same shading, or they have three different shadings.
4. They all have the same color, or they have three different colors.

从描述中可以知道，Set的Card有 number, symbol, shading, color 四组属性，每个属性共有3种类型，一共81张牌。

所以match()改成了这个样子：

```objective-c
- (BOOL)matchSetCardsWithKey:(SetPlayingCard *)card1 card2:(SetPlayingCard *)card2 card3:(SetPlayingCard *)card3 key:(NSString *)key isString:(BOOL)isString
{
    BOOL match = NO;
    
    id value1 = [card1 valueForKey:key];
    id value2 = [card2 valueForKey:key];
    id value3 = [card3 valueForKey:key];
    
    if (!isString) {
        if ((value1 == value2) && (value2 == value3)) {
            match = YES;
            NSLog(@"3 card has same '%@'", key);
        } else if ((value1 != value2) && (value2 != value3) && (value3 != value1)) {
            match = YES;
            NSLog(@"3 card has different '%@'", key);
        }
    } else {
        if (([value1 isEqualToString:value2]) && ([value2 isEqualToString:value3])) {
            match = YES;
            NSLog(@"3 card has same '%@'", key);
        } else if ((![value1 isEqualToString:value2]) && (![value2 isEqualToString:value3]) && (![value3 isEqualToString:value1])) {
            match = YES;
            NSLog(@"3 card has different '%@'", key);
        }
    }
    
    return match;
}

// override
- (NSInteger)match:(NSArray *)otherCards
{
    int score = 0;
    if ([otherCards count] == 2) {     // 3x model
        id card1 = [otherCards firstObject];
        id card2 = [otherCards lastObject];
        if (([card1 isKindOfClass:[SetPlayingCard class]]) && ([card2 isKindOfClass:[SetPlayingCard class]])) {
            SetPlayingCard *otherCard1 = (SetPlayingCard *)card1;
            SetPlayingCard *otherCard2 = (SetPlayingCard *)card2;
            
            // They all have the same number, or they have three different numbers
            if ([self matchSetCardsWithKey:self card2:otherCard1 card3:otherCard2 key:@"rank" isString:NO]) {
                // They all have the same symbol, or they have three different symbols
                if ([self matchSetCardsWithKey:self card2:otherCard1 card3:otherCard2 key:@"symbol" isString:YES]) {
                    // They all have the same shading, or they have three different shadings
                    if ([self matchSetCardsWithKey:self card2:otherCard1 card3:otherCard2 key:@"shading" isString:YES]) {
                        // They all have the same color, or they have three different colors
                        if ([self matchSetCardsWithKey:self card2:otherCard1 card3:otherCard2 key:@"color" isString:NO]) {
                            score = 16;
                        }
                    }
                }
            }
            
        }
    }
    return score;
}
```

主要是实现matchSetCardsWithKey()这个函数，比较某条规则是否匹配。完成的作业截图如下：

![Assignment 3 screenshut](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot3a.png)

History功能比较简单，因为已经显示了每次匹配的结果，不断将NSAttributedString附加到一个matchHistory变量即可：

![Assignment 3 history page](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot3b.png)

[代码见GitHub](https://github.com/upbit/CS193p_Homework/tree/975beb12f587d5e9a93ead443fe61f633150fc43/Matchismo/Matchismo)

------------------

做完后看Hint，才悲剧的发现这样实现存在2个严重的问题。一个是第7点，模型应该是UI无关的，而我把NSAttributedStrings引入到了card.contents的内容显示上

```
 7. Your Model is UI independent, so it cannot have NSAttributedStrings with UI 
    attributes anywhere in its interface or implementation. Any attribute defined in UIKit 
    is a UI attribute (obvious ones are those whose values are, for example, a UIColor or a 
    UIFont). All the attributes discussed in lecture were UI attributes. While it would 
    theoretically be legal to have an NSAttributedString without UI attributes in your 
    Model, it is recommended you not do that for this assignment. Use 
    NSAttributedString only in your Controller camp, not your Model camp
```

但去掉后又不知改怎么渲染NSAttributedStrings，难道要导出NSDictionary在外面插回去？

另一个是我的CardGameViewController/SetGameViewController并非继承于同一个自定义类，因为设置父类GameViewController时，无法将StoryBoard里不同数量的按钮在子类中绑定，又能在父类中获取到cardButtons的IBOutletCollection。也许需要动态创建按钮吧，这里着实想不通。

看来只能找机会去看看别人的实现方法了。。。
