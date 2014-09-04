---
layout: post
title: Ingress-iOS ports代码阅读1
description: "Ingress-iOS code reading 01"
category: opensource
comments: true
share: true
---

昨晚装上VPN兴冲冲出去把Ingress升到了Lv2，觉得这种实境"世界"实在是让宅男出门的最佳办法，何况它还打着拯救/奴役世界的幌子。想想不用实现的这么复杂，只要有个简单的户外游戏，就能够持续给予我外出的动力：能够根据行走的距离累计游戏内的点数(或金钱)，完成一些特殊地点才能取得的成就，乃至通过自己的努力改变虚拟世界的状态(例如经商模拟类游戏，通过物品的交易提升某个地域的贸易等级，从而慢慢从小镇发展为城邦甚至王国)

于是在GitHub上找到了曾经有人ports的 [Ingress for iOS](https://github.com/marstone/ingress-ios)，打算下载下代码自己研究下，于是有了这篇文章。

先研究主界面，虽然没有现在Ingress那么绚丽，但也足够满足最初的需求了。以下是运行截图：

![Ingress for iOS](/images/ingress-ios_screenshot.png)

从StoryBoard里可以看到，最初显示的由LoadingViewController控制，出口一个是Enbed Segue: GLKView，另一个则是Model Segue: ScannerViewController，也就是上面的正题 MapKit 界面。根据 Model Seque 的Identifier "LoadingCompletedSegue"可以找到如下代码：

```objective-c
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    if ([segue.identifier isEqualToString:@"LoadingCompletedSegue"]) {
        [[AppDelegate instance] setScannerViewController:(ScannerViewController *)segue.destinationViewController];

        if ([[NSUserDefaults standardUserDefaults] boolForKey:DeviceSoundToggleBackground]) {
            [[SoundManager sharedManager] playMusic:@"Sound/sfx_ambient_scanner_base.aif" looping:YES];
        }
    }
}
```

在loading动画结束后，切换ScannerViewController，并播放背景音效"Sound/sfx_ambient_scanner_base.aif"

转到ScannerViewController，从它的两个连接都是处理地图点击的，暂时不用关心：

1. Model Seque<PortalDetailSegue>: PortalDetailViewController
2. Model Seque<FactionChooseSegue>: MissionViewController

回到ScannerViewController的 viewDidLoad:，先初始化几个单例类，然后将自身的_mapView导出到AppDelegate里的mapView变量里，并检查app是否有定位权限：

```objective-c
- (void)viewDidLoad {

    [[AppDelegate instance] setMapView:_mapView];

    // 检查app是否有定位服务的权限
    [self validateLocationServicesAuthorization];
    ...
}

- (void)validateLocationServicesAuthorization {
    if ([CLLocationManager authorizationStatus] != kCLAuthorizationStatusAuthorized) {
        // 没有定位权限，显示 "Please allow location services" 的提示
        if (!locationAllowHUD) {
            _mapView.hidden = YES;
            playerArrowImage.hidden = YES;
            // MBProgressHUD: Displays a simple HUD window containing a progress indicator and two optional labels for short messages
            locationAllowHUD = [[MBProgressHUD alloc] initWithView:self.view];
            locationAllowHUD.userInteractionEnabled = NO;
            locationAllowHUD.mode = MBProgressHUDModeCustomView;
            locationAllowHUD.customView = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"warning.png"]];
            locationAllowHUD.labelText = @"Please allow location services";
            locationAllowHUD.labelFont = [UIFont fontWithName:[[[UILabel appearance] font] fontName] size:16];
            locationAllowHUD.removeFromSuperViewOnHide = YES;
            __weak __block typeof(locationAllowHUD) weakLocationAllowHUD = locationAllowHUD;
            locationAllowHUD.completionBlock = ^{
                weakLocationAllowHUD = nil;
            };
            [self.view addSubview:locationAllowHUD];
            [locationAllowHUD show:YES];
        }
    } else {
        if (locationAllowHUD) {
            [locationAllowHUD hide:YES];
            _mapView.hidden = NO;
            playerArrowImage.hidden = NO;
        }
    }
    
    [self updateRangeCircleView];
}
```

接着从StoryBoard里读取CommViewController模板，添加到SubView中：

```objective-c
- (void)viewDidLoad {

    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"MainStoryboard_iPhone" bundle:nil];
    commVC = [storyboard instantiateViewControllerWithIdentifier:@"CommViewController"];
    [self.view addSubview:commVC.view];

    [self updateCommViewControllerFrame];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(statusBarFrameDidChange:) name:UIApplicationDidChangeStatusBarFrameNotification object:nil];
    [self addChildViewController:commVC];

}
```

根据StoryBoard布局，这个层是对话框界面，暂时pass继续向后看。接着初始化LocationManager和手势操作：

```objective-c
- (void)viewDidLoad {

    [[LocationManager sharedInstance] addDelegate:self];

    UIPinchGestureRecognizer *recognizer = [[UIPinchGestureRecognizer alloc] initWithTarget:self action:@selector(handlePinch:)];
    [_mapView addGestureRecognizer:recognizer];

    UILongPressGestureRecognizer *mapViewLognPressGestureRecognizer = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(mapLongPress:)];
    [_mapView addGestureRecognizer:mapViewLognPressGestureRecognizer];

    #if __IPHONE_OS_VERSION_MAX_ALLOWED >= 70000

    if ([_mapView respondsToSelector:@selector(isPitchEnabled)]) {
        _mapView.pitchEnabled = NO;
    }

    if ([_mapView respondsToSelector:@selector(isRotateEnabled)]) {
        _mapView.rotateEnabled = NO;
    }

    #endif

}
```

先设置 LocationManager <CLLocationManagerDelegate> 为self，然后绑定Pinch手势到handlePinch:，LongPress手势到mapLongPress:。iOS7.0以上SDK时，还设置MapKit本身无法Pitch和Rotate。

LocationManager是一个自定义对CLLocationManager的单例封装，里面包含了对CLLocationManager的初始化操作：

```objective-c
- (id)init {
    self = [super init];
    if (self) {
        
        _delegates = [NSMutableArray array];
        
        _locationManager = [[CLLocationManager alloc] init];
        _locationManager.desiredAccuracy = kCLLocationAccuracyBestForNavigation;        // 设置为最佳精度(不怕过于耗电?)
        if ([_locationManager respondsToSelector:@selector(activityType)]) {
            _locationManager.activityType = CLActivityTypeFitness;                      // 当位置改变时才进行跟踪
        }
        _locationManager.delegate = self;
        
        [_locationManager startUpdatingLocation];
        [_locationManager startUpdatingHeading];
        
    }
    return self;
}
```

XMOverlay是一个MKOverlay协议的对象，貌似是用于绘制什么内容到Map上的。暂时看不懂，不过也有[How do I create an image overlay and add to MKMapView?](http://stackoverflow.com/questions/5283741/how-do-i-create-an-image-overlay-and-add-to-mkmapview)这样的例子可以参考，暂时也不用管它：

```objective-c
- (void)viewDidLoad {

    _xmOverlay = [XMOverlay new];
    [_mapView addOverlay:_xmOverlay];

}
```

后面的quickActionsMenu初始化和NSNotificationCenter部分，无关地图绘制的部分就先pass了。
