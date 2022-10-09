---
layout: post
title: 终于填完了CS193p作业3的坑，不过话说还有Extra Credit没写...
description: "CS193p: assignment 3 update"
category: study
comments: true
share: true
---

网上找了个[作业3的例子](http://cs193p.m2m.at/tag/matchismo+2013-14/)，看完代码才对UIViewController的继承有所体会

```objective-c
@property (strong, nonatomic) IBOutletCollection(UIButton) NSArray *cardButtons;
```

IBOutletCollection在父类GameViewController绑定后，在StoryBoard里切换View的Controller为子类(比如SetGameViewController)，此时View中按钮就不再绑定到父类的这个IBOutletCollection中了。在子类头部声明变量后，查看绑定正常。

------------------

另一个是对Set这个游戏理解不够，这个游戏的UI应该是这样的：

![Assignment 3 screenshut](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot3a.png)

也就说不是翻转卡片，需要在子类中重写卡片绘制函数：

```objective-c
- (NSAttributedString *)attributedTitleForCard:(Card *)card
{
    NSString *symbol = @"?";
    NSMutableDictionary *attributes = [[NSMutableDictionary alloc] init];
    
    if ([card isKindOfClass:[SetPlayingCard class]]) {
        SetPlayingCard *setCard = (SetPlayingCard *)card;
        
        if ([setCard.symbol isEqualToString:@"oval"]) symbol = @"●";
        if ([setCard.symbol isEqualToString:@"squiggle"]) symbol = @"▲";
        if ([setCard.symbol isEqualToString:@"diamond"]) symbol = @"■";
        
        symbol = [symbol stringByPaddingToLength:setCard.number withString:symbol startingAtIndex:0];

        if ([setCard.color isEqualToString:@"red"]) {
            [attributes setObject:[UIColor redColor] forKey:NSForegroundColorAttributeName];
        } else if ([setCard.color isEqualToString:@"green"]) {
            [attributes setObject:[UIColor greenColor] forKey:NSForegroundColorAttributeName];
        } else if ([setCard.color isEqualToString:@"purple"]) {
            [attributes setObject:[UIColor purpleColor] forKey:NSForegroundColorAttributeName];
        }
        
        if ([setCard.shading isEqualToString:@"open"]) {
            [attributes setObject:@8 forKey:NSStrokeWidthAttributeName];
        } else if ([setCard.shading isEqualToString:@"striped"]) {
            [attributes addEntriesFromDictionary:@{ NSStrokeWidthAttributeName : @-8,
                                                    NSStrokeColorAttributeName : [UIColor blackColor],
                                                    NSForegroundColorAttributeName : [attributes[NSForegroundColorAttributeName] colorWithAlphaComponent:0.6]}];
        } else { // solid
            // pass
        }
    }
    
    return [[NSMutableAttributedString alloc] initWithString:symbol attributes:attributes];
}
```

不像CardGame里需要判断是否选中，这里需要总是返回卡片的图案。另外学到个stringByPaddingToLength，用在这里确实比for循环简洁。

最后是History功能，觉得他里面那样实现过于曲折，于是稍稍改动按钮的响应动作，每次匹配后把updateUI()后的attributedText附加到History中：

```objective-c
- (IBAction)cardTouchButton:(UIButton *)sender {
    int chosenButtonIndex = [self.cardButtons indexOfObject:sender];
    BOOL matched = [self.game chooseCardAtIndex:chosenButtonIndex matchCount:self.chosenCardCount];
    [self updateUI];
    
    if (matched) {
        [self.matchHistory appendAttributedString:self.infoLabel.attributedText];
        [self.matchHistory appendAttributedString:[[NSAttributedString alloc] initWithString:@"\n"]];
    }
}
```

重点是让chooseCardAtIndex()增加个BOOL的返回值，以便判断是否发生过matchCard()。如果match过，则等待updateUI()更新了Label后，把Label的attributedText拿过来。整个过程都是父类内部的操作，子类只用关注于按自己需求更新Label就好。

![Assignment 3 history page](https://raw.github.com/upbit/CS193p_Homework/master/screenshot/screenshot3b.png)

[GitHub源码地址](https://github.com/upbit/CS193p_Homework/tree/a96f260a7b3b24b3f0484655d58094284f023f38/Matchismo/Matchismo)

至于Extra Credit说的高分存储和排行榜功能，就留到以后完善把...
