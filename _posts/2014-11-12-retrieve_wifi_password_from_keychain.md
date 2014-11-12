---
layout: post
title: 自己动手从 iOS Keychain 中恢复保存的Wifi密码
description: "Retrieve Wifi password form Keychain by code"
category: theos
comments: true
share: true
---

最近在学用Theos编写插件和小工具，本来打算自己动手写个类似[Wifi Passwords](http://modmyi.com/cydia/com.malcolmhall.wifipasswords)的工具，用于查看保存在iOS设备中的Wifi密码的。不过搜了下居然没找到具体的实现方法。

本来以为iOS和Android一样保存在某个plist里(Android的Wifi密码明文保存在/data/misc/wifi/wpa_supplicant.conf)，不过Google了才发现iOS虽然在 /private/var/preferences/SystemConfiguration/com.apple.wifi.plist 里保存了Wifi信息，但密码却是存储在Keychain中的，而且网上没有给出具体的读取办法。

对Keychain Services不熟，只好用 Hopper Disassembler 反汇编WiFiPasswords自己看了。WiFiPasswords代码不多，大部分都是和UITableView相关的内容，排除掉这些很快发现 -[RootViewController refresh] 就是要找的函数：

![StoryBoard]({{ site.url }}/images/201411/wifi_password_01.png)

这里读取了 /private/var/preferences/SystemConfiguration/com.apple.wifi.plist 的List of known networks中的信息(意义不明，实际上直接用SecItemCopyMatching也是能获取到Wifi名的，而且它后面也这样做了)。到机器上用plutil查看该plist文件，会看到类似下面的内容：

```
    "List of known networks" =     (
                {
            "80211D_IE" =             {
                "IE_KEY_80211D_COUNTRY_CODE" = JP;
            };
            "80211W_ENABLED" = 0;
            AGE = 41;
            "AP_MODE" = 2;
            "ASSOC_FLAGS" = 1;
            "BEACON_INT" = 20;
            BSSID = "00:2e:1f:54:1d:82";
            CAPABILITIES = 1041;
            CHANNEL = 12;
		...
```

这个plist里存储的是Wifi的属性等信息，继续往后看就能发现关键数据：

![StoryBoard]({{ site.url }}/images/201411/wifi_password_02.png)

先构造一个NSMutableArray，内容为 [kSecClass, kSecAttrService, kSecReturnAttributes, kSecMatchLimit]

![StoryBoard]({{ site.url }}/images/201411/wifi_password_03.png)

接着构造另一个NSMutableArray，内容为 [kSecClassGenericPassword, @"AirPort", kCFBooleanTrue, kSecMatchLimitAll]。根据接着调用的 SecItemCopyMatching 可以得知，这个数据是传递给 SecItemCopyMatching 参数query的CFDictionaryRef，其作用是查询 Keychain 中kSecClass=kSecClassGenericPassword，且kSecAttrService为AirPort的属性。

参考[StackOverflow中SecItemCopyMatching的用法例子](http://stackoverflow.com/questions/10966969/enumerate-all-keychain-items-in-my-ios-application)，很容易还原出上面的代码。用Theos的nic.pl创建一个tools项目，输入验证用代码：

```objective-c
#import <Security/Security.h>

int main(int argc, char **argv, char **envp)
{
	NSMutableDictionary *query = [NSMutableDictionary dictionary];

	[query setObject:(__bridge id)kSecClassGenericPassword forKey:(__bridge id)kSecClass];
	[query setObject:(__bridge id)kSecMatchLimitAll forKey:(__bridge id)kSecMatchLimit];
	[query setObject:(__bridge id)@"AirPort" forKey:(__bridge id)kSecAttrService];
	[query setObject:(__bridge id)kCFBooleanTrue forKey:(__bridge id)kSecReturnAttributes];

	CFTypeRef result = NULL;
	OSStatus status = SecItemCopyMatching((__bridge CFDictionaryRef)query, &result);
	if (status != errSecSuccess) {
		printf("[ERROR] SecItemCopyMatching() failed! error = %d\n", (int)status);
		return;
	}

	NSArray *wifi_list = (NSArray *)result;
	for (int i = 0; i < wifi_list.count; i++) {
		NSDictionary *wifi = (NSDictionary*)wifi_list[i];

		NSString *output = [NSString stringWithFormat:@"%@", wifi];
		printf("%s\n", [output cStringUsingEncoding:NSUTF8StringEncoding]);
	}

	if (result != NULL) {
		CFRelease(result);
	}

	return 0;
}
```

make后传到iOS里运行，然后顺利的失败了。提示-34018：

```
[ERROR] SecItemCopyMatching() failed! error = -34018
```

Google发现SecItemCopyMatching返回-34018(errSecMissingEntitlement)是权限问题，
用 ldid -e WiFiPasswords 查看entitlement，发现它比常规程序多了 keychain-access-groups 权限。于是编辑一个 ent.xml 如下：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>application-identifier</key>
	<string>com.zzz.keychain_enum</string>
	<key>get-task-allow</key>
	<true/>
	<key>keychain-access-groups</key>
	<array>
		<string>apple</string>
	</array>
</dict>
</plist>
```

用 ldid -Sent.xml <app> 签上keychain-access-groups后运行，成功输出保存的Wifi信息：

```
{
    accc = "<SecAccessControlRef: 0x1566a4c0>";
    acct = Magdalene;
    agrp = apple;
    cdat = "2014-11-02 04:56:39 +0000";
    mdat = "2014-11-02 04:56:39 +0000";
    pdmn = ck;
    svce = AirPort;
    sync = 0;
    tomb = 0;
}
```

注意，ldid签名如果失败，一般是codesign_allocate没有用对导致的。在MacOS上/usr/bin/codesign_allocate并非iOS用的版本，需要手工export一下(注意替换为你的Xcode安装目录)：

```
export CODESIGN_ALLOCATE=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/codesign_allocate
ldid -Sent.xml obj/wifi_passwords
```

已经在获取到的 NSDictionary 中找到Wifi的名称(acct)，就可以开始查询对应的密码了。获取密码的参数为：

![StoryBoard]({{ site.url }}/images/201411/wifi_password_04.png)

和之前获取Wifi名类似，将kSecReturnAttributes换成了kSecReturnData，另外有kSecAttrAccount(acct)也不需要搜索了；kSecReturnData的返回为NSData，里面就是对应Wifi的密码。

完成后的恢复Wifi密码的函数如下：

```objective-c
void keychain_wifi_passwords()
{
	NSMutableArray *acct_name = [NSMutableArray array];

	// form KeyChain get AirPort.acct (Wifi Name)
	{
		NSMutableDictionary *query = [NSMutableDictionary dictionary];

		[query setObject:(__bridge id)kSecClassGenericPassword forKey:(__bridge id)kSecClass];
		[query setObject:(__bridge id)kSecMatchLimitAll forKey:(__bridge id)kSecMatchLimit];
		[query setObject:(__bridge id)@KEYCHAIN_SVCE_AIRPORT forKey:(__bridge id)kSecAttrService];
		[query setObject:(__bridge id)kCFBooleanTrue forKey:(__bridge id)kSecReturnAttributes];

		CFTypeRef result = NULL;
		OSStatus status = SecItemCopyMatching((__bridge CFDictionaryRef)query, &result);
		if (status != errSecSuccess) {
			printf("[ERROR] SecItemCopyMatching() failed! error = %d\n", (int)status);
			return;
		}

		NSArray *wifi_list = (NSArray *)result;
		for (int i = 0; i < wifi_list.count; i++) {
			NSDictionary *wifi = (NSDictionary*)wifi_list[i];
			// get wifi name
			[acct_name addObject:wifi[@KEYCHAIN_ACCT_NAME]];
		}

		if (result != NULL) {
			CFRelease(result);
		}
	}

	// get password for each AirPort.acct
	{
		for (NSString *acct in acct_name) {
			NSMutableDictionary *query = [NSMutableDictionary dictionary];

			[query setObject:(__bridge id)kSecClassGenericPassword forKey:(__bridge id)kSecClass];
			[query setObject:(__bridge id)@KEYCHAIN_SVCE_AIRPORT forKey:(__bridge id)kSecAttrService];
			[query setObject:acct forKey:(__bridge id)kSecAttrAccount];
			[query setObject:(__bridge id)kCFBooleanTrue forKey:(__bridge id)kSecReturnData];

			CFTypeRef result = NULL;
			OSStatus status = SecItemCopyMatching((__bridge CFDictionaryRef)query, &result);
			if (status != errSecSuccess) {
				printf("[ERROR] SecItemCopyMatching() failed! error = %d\n", (int)status);
				return;
			}

			NSData *password = (NSData *)result;
			NSString *output = [[NSString alloc] initWithData:password encoding:NSASCIIStringEncoding];
			printf("%s: %s\n", [acct cStringUsingEncoding:NSUTF8StringEncoding], [output cStringUsingEncoding:NSUTF8StringEncoding]);

			if (result != NULL) {
				CFRelease(result);
			}
		}
	}
}
```

make编译并make ldid签上ent.xml后，传入iOS会输出Keychain中保存的Wifi名和密码：

```
root# ./wifi_passwords
Magdalene: Retrieve Wifi password.
iPhone: 123456
```

剩下就是给控制台程序加个界面了。完整Theos工程代码见：[https://github.com/upbit/My-iDevice-Tools/blob/master/wifi_passwords.mm](https://github.com/upbit/My-iDevice-Tools/blob/master/wifi_passwords.mm)
