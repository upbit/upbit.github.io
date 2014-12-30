---
layout: post
title: 关于iOS第三方SDK的反跟踪与欺骗 - MobClick/MTA
description: "Disable 3rd SDK (MobClick/MTA) tracking"
category: theos
comments: true
share: true
---

用Fiddler4追踪某App请求时，发现有个发给 pingma.qq.com 的内容无法解析，看了下居然是rc4加密的。这种不知道别人上报了你什么内容的请求让人很反感，索性把App上带的SDK上报都看了下。无奈的发现不光腾讯这个MTA，MobClick甚至事无巨细到连App是否是破解的都有上报。于是写了个禁止上报的Cydia插件，[源码放在GitHub](https://github.com/upbit/iOS_3rdTrackingBlocker)。

## MTA

因为好奇 pingma.qq.com 的上报内容，先分析了这个SDK。要想知道SDK的上报内容，直接启用SDK自己的Log就好了，MTAConfig中有个debugEnable就是控制这个的：

~~~objective-c
@interface MTAConfig : NSObject {}
@property(assign) BOOL debugEnable;
@end
~~~

让这个 debugEnable 永远返回YES就能输出NSLog：

~~~logos
%hook MTAConfig
- (id)debugEnable { return (id)YES; }
%end
~~~

另外，用cycript调用 [MTAHelper getEnv] 和 [MTAHelper getUser] 可以很轻松的获取MTAEnv和MTAUser的实例，他们的初始化则在 [MTAHelper init] 中。还原初始化的伪代码如下：

~~~objective-c
- (id)init {
  self = [super init];
  if (self) {
    env = [[MTAEnv alloc] init];
    env.platform = [[UIDevice currentDevice] systemName];
    sys_version = [[UIDevice currentDevice] systemVersion];
    if ([sys_version compare:@"6.0" options:0x40] != 0xffffffff) {
      env.ifv = [[UIDevice currentDevice] identifierForVendor];
    }
    uname(device_name);
    env.devicename = [NSString stringWithCString:device_name encoding:0x4];
    env.modulename = [[UIDevice currentDevice] model];
    env.os_version = [[UIDevice currentDevice] systemVersion];
    # env.jailbroken checks
    # [[NSFileManager defaultManager] fileExistsAtPath:@"/bin/bash"]
    # [[NSFileManager defaultManager] fileExistsAtPath:@"/Applications/Cydia.app"]
    # [[NSFileManager defaultManager] fileExistsAtPath:@"/private/var/lib/apt"]
    env.timezone = [[NSTimeZone systemTimeZone] name];
    # env.resolution = [[UIScreen mainScreen] bounds] or [[UIScreen mainScreen] scale]
    env.deviceid = [MTAOpenUDID value];
    env.language = [[[NSUserDefaults standardUserDefaults] objectForKey:@"AppleLanguages"] objectAtIndex:0];
    tni = [[CTTelephonyNetworkInfo alloc] init];
    mcc = [[tni subscriberCellularProvider] mobileCountryCode];
    mnc = [[tni subscriberCellularProvider] mobileNetworkCode];
    env.mccmnc = [mcc stringByAppendingString:mnc];
    env.app_version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleShortVersionString"];
    env.sdk_version = @"1.4.1";
  }
  return self;
}
~~~

然后在MTAHelper中还会收集当前Wifi的名字和MAC地址，估计是用于判断用户上网的场景。

## MobClick

友盟的这个SDK算是最常见的，国内绝大多数App都有这个。上报的也比MTA"完善"多了，甚至还提供 [MobClick isPirated] 来判断app的签名是否正常...

里面封装的可真深，不过也很容易查到MobClickApp的logEnabled就是控制NSLog输出的。同样hook掉：

~~~logos
%hook MobClickApp
- (BOOL)logEnabled { return YES; }
%end
~~~

相对禁用API，这个就没有MTA那样方便了。最后只好干掉传入的appkey，让SDK初始化失败来禁用：

~~~logos
%hook MobClickSession
+ (void)startWithAppkey:(id)appkey reportPolicy:(int)policy channelId:(id)anId { %orig((id)@"", policy, anId); }
%end
~~~

`[MobClickSession startWithAppkey:reportPolicy:channelId:]` 是最内层的调用，所以替换掉这里的 appkey 就可以达到目的了。

代码已经上传到GitHub: [TrackingBlocker.xm](https://github.com/upbit/iOS_3rdTrackingBlocker/blob/master/TrackingBlocker.xm)，或者直接下载deb包进行安装：[com.zzz.3rdtrackingblocker_1.0_iphoneos-arm.deb](http://blog.imaou.com/uploads/com.zzz.3rdtrackingblocker_1.0_iphoneos-arm.deb)
