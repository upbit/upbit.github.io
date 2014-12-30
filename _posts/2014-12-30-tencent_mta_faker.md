---
layout: post
title: 腾讯 MTA SDK 上报内容分析与伪造
description: "Tencent MTA SDK report faker"
category: theos
comments: true
share: true
---

用Fiddler4追踪某App请求时，发现有个发给 pingma.qq.com 的内容无法解析，看了下居然是rc4加密的。这种不知道别人跟踪你什么内容的请求让人很反感，于是索性从app侧看了下上报的内容。

幸运的从classdump结果中发现，MTA自己带有debugEnable属性，这样只要hook MTAConfig让debugEnable总是返回YES就好了：

~~~objective-c
@interface MTAConfig : NSObject {
  BOOL _debugEnable;
}
@property(assign) BOOL debugEnable;
@end
~~~

Theos写个简单的tweak，注入到app就可以看到上报内容了：

~~~logos
%hook MTAConfig
- (id)debugEnable {	return (id)YES; }
%end
~~~

syslog里的上报内容：

~~~
[1419923960] [INFO] Report event :{"et":1000,"idx":6,"mid":"none","mc":"02:00:00:00:00:00","ky":"I8E2UHE1PB2A","ifv":"56B2AA59-8212-6636-71EB-C16A815E29C3","ui":"050b68ba97eeac622529f8e61342fe914cbf0878","sv":"1.4.1","ch":"appstore","ei":"feed_change","kv":{"type":"latest"},"si":1142517312,"ts":1419923960,"ut":0,"av":"1.1.0"}
[1419923960] [INFO] Send stat events to access server.
[1419923960] [INFO] Report response content is {"ret":0} with length:9
[1419923960] [INFO] Send stat request success
~~~

看上去加密前数据是段json，成功接受后返回 {"ret":0}：

~~~json
{
  "et": 1000,
  "idx": 6,
  "mid": "none",
  "mc": "02:00:00:00:00:00",
  "ky": "I8E2UHE1PB2A",
  "ifv": "56B2AA59-8212-6636-71EB-C16A815E29C3",
  "ui": "050b68ba97eeac622529f8e61342fe914cbf0878",
  "sv": "1.4.1",
  "ch": "appstore",
  "ei": "feed_change",
  "kv": {
    "type": "latest"
  },
  "si": 1142517312,
  "ts": 1419923960,
  "ut": 0,
  "av": "1.1.0"
}
~~~

其中ky/ifv/ui显然和机器有关。跟踪了会代码，发现上报请求是 [MTADispatcher nsRequest:responseData:] 发送的，其中相关的机器数据由 MTAHelper 类提供：

~~~objective-c
@interface MTAEnv : NSObject {
  BOOL _jailbroken;
  NSString* _platform;
  NSString* _os_version;
  NSString* _language;
  NSString* _resolution;
  NSString* _deviceid;
  NSString* _mccmnc;
  NSString* _timezone;
  NSString* _app_version;
  NSString* _sdk_version;
  NSString* _devicename;
  NSString* _modulename;
  NSUUID* _ifa;
  NSUUID* _ifv;
  NSString* _wf;
}
@property(retain, nonatomic) NSString* wf;
@property(assign) BOOL jailbroken;
@property(retain, nonatomic) NSUUID* ifv;
@property(retain, nonatomic) NSUUID* ifa;
@property(retain, nonatomic) NSString* modulename;
@property(retain, nonatomic) NSString* devicename;
@property(retain, nonatomic) NSString* sdk_version;
@property(retain, nonatomic) NSString* app_version;
@property(retain, nonatomic) NSString* timezone;
@property(retain, nonatomic) NSString* mccmnc;
@property(retain, nonatomic) NSString* deviceid;
@property(retain, nonatomic) NSString* resolution;
@property(retain, nonatomic) NSString* language;
@property(retain, nonatomic) NSString* os_version;
@property(retain, nonatomic) NSString* platform;
-(void)dealloc;
@end

@interface MTAUser : NSObject {
  NSString* _uid;
  int _userType;
  NSString* _appVer;
  unsigned _tagTime;
}
@property(assign) unsigned tagTime;
@property(retain, nonatomic) NSString* appVer;
@property(assign) int userType;
@property(retain, nonatomic) NSString* uid;
-(void)dealloc;
@end

@interface MTAHelper : NSObject {
  MTAEnv* env;
  MTAUser* user;
  BOOL _MTAEnableFlag;
}
@property(assign) BOOL MTAEnableFlag;
+(id)getInstance;
+(BOOL)isWiFiAvailable;
+(id)getMacAddress;
+(int)GUnzip:(id)unzip Out:(id*)anOut;
+(int)GZip:(id)zip Out:(id*)anOut;
+(void)ChiperRC4:(id)a4;
-(id)getJson:(id)json;
-(void)saveMid:(id)mid;
-(id)getMid;
-(BOOL)checkMTAEnable;
-(id)fetchSSIDInfo;
-(void)dealloc;
-(id)init;
-(id)getUser;
-(id)getEnv;
@end
~~~

用cycript调用 [MTAHelper getEnv] 和 [MTAHelper getUser] 可以很轻松的获取MTAEnv和MTAUser的实例，他们的初始化则在 [MTAHelper init] 中。还原初始化的伪代码如下：

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

从init中可以看到，MTAEnv里收集的关键信息为 ifv、devicename、deviceid 和 mccmnc，另外MTAUser中还有个uid需要留意。如果真要防止被跟踪，可以hook MTAEnv/MTAUser里的对应getter函数，返回个随机值什么的糊弄下MTA :P
