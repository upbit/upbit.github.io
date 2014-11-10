---
layout: post
title: Tweak开发 - 为 SSL Kill Switch 增加AppList
description: "Tweak Dev - Added support for SSL Kill Switch"
category: theos
comments: true
share: true
---

调试HTTPS请求的都知道，大部分App的HTTPS流量一经Fiddler decode就会失效，这是iOS的证书检查在作怪。于是有了[iOS SSL Kill Switch](https://github.com/iSECPartners/ios-ssl-kill-switch)，可以干掉这个检查并让你顺利追踪HTTPS请求。

最近手痒升级了iOS8，一则是不确定 SSL Kill Switch 是否能作用于iOS8，另外为了解决Hook全局这个问题，于是自己动手给 SSL Kill Switch 加了个AppList，用于手动选择要Hook的目标程序（比如用户App或天气等系统应用）:

<span style="color:#f00;">**deb下载：**</span>[com.isecpartners.nabla.sslkillswitch_v0.61-iOS_8.1.deb](http://blog.imaou.com/SSLKillSwitch/com.isecpartners.nabla.sslkillswitch_v0.61-iOS_8.1.deb) - (兼容iOS8.1，需要**AppList**)

![SSL Kill Switch]({{ site.url }}/SSLKillSwitch/screenshot.png)

## Tweak修改过程

开发Cydia插件和应用，目前主流的好像都是Theos了。Mac下[配置环境](http://iphonedevwiki.net/index.php/Theos/Setup)极为简单，就不重复Theos的安装方法了。

### SSL Kill Switch

这个插件在v0.5版后，采用Hook **SSLCreateContext** 增加 **SSLSetSessionOption**: [kSSLSessionOptionBreakOnServerAuth](https://developer.apple.com/library/IOs/documentation/Security/Reference/secureTransportRef/index.html#//apple_ref/c/econst/kSSLSessionOptionBreakOnServerAuth)选项来实现关闭本地证书检查，关于这个选项的解释如下：

~~~
kSSLSessionOptionBreakOnServerAuth

Enables returning from SSLHandshake (with a result of errSSLServerAuthCompleted) when the server authentication portion of the handshake is complete to allow your application to perform its own certificate verification.

Note that in iOS (all versions) and OS X 10.8 and later, setting this option disables Secure Transport's automatic verification of server certificates.

If you set this option, your application should perform its own certificate verification when errSSLServerAuthCompleted is returned before continuing with the handshake.

Available in iOS 5.0 and later.
~~~

Secure Transport Reference里提到这个是用于自行实现证书校验的，设置后会关闭Secure Transport的自动server端证书校验。

理解这一点后，[SSL Kill Switch的代码](https://github.com/iSECPartners/ios-ssl-kill-switch/blob/master/Tweak.xm)就变得清晰易懂了。

1. **%ctor**里Hook **SSLHandshake/SSLSetSessionOption** 和 **SSLCreateContext**；
2. Hook **SSLCreateContext**是为了增加 **SSLSetSessionOption**(sslContext, kSSLSessionOptionBreakOnServerAuth, true) 来关闭自带的证书检查；
3. Hook **SSLSetSessionOption**则是为了防止外部修改 kSSLSessionOptionBreakOnServerAuth；
4. Hook **SSLHandshake**是因为设置 kSSLSessionOptionBreakOnServerAuth 后，需要处理errSSLServerAuthCompleted (其实此时只要调用原来的SSLHandshake继续处理就行)；

### AppList

[AppList](https://github.com/rpetrich/AppList)是个辅助获取已安装程序列表的插件，自带sample里演示了如何利用PreferenceLoader在设置中增加一个App列表，并可以供用户设置。用在这里，刚好适合SSL Kill Switch选取需要Hook的程序。

首先复制sample下的application.m，这个是AppList显示Preference时需要的cell等相关代码，因为不需要修改Cell的外观，这里保持不变。修改Makefile增加如下内容：

~~~makefile
# application
APPLICATION_NAME = SSLKillSwitchSettings
SSLKillSwitchSettings_FILES = application.m

SSLKillSwitchSettings_FRAMEWORKS = UIKit CoreGraphics
SSLKillSwitchSettings_PRIVATE_FRAMEWORKS = Preferences
SSLKillSwitchSettings_LIBRARIES = applist

include $(THEOS_MAKE_PATH)/application.mk
~~~

这样打包时就会将这个app和tweak打到同一个deb中了。这个app用于显示设置里的AppList，并将设置存储于指定的plist中。修改 layout/Library/PreferenceLoader/Preferences/SSLKillSwitch_prefs.plist 文件，适配AppList的内容：

~~~json
{
	entry = {
		bundle = AppList;
		cell = PSLinkCell;
		icon = "/Library/PreferenceLoader/Preferences/SSLKillSwitch.png";
		isController = 1;
		label = "SSL Kill Switch";
		ALSettingsPath = "/var/mobile/Library/Preferences/com.isecpartners.nabla.SSLKillSwitchSettings.plist";
		ALSettingsKeyPrefix = "Settings-";
		ALChangeNotification = "com.rpetrich.applist.sample.notification";
		ALAllowsSelection = 1;
		ALSectionDescriptors = (
			{
				items = (
					{
						text = "Disable Certificate Validation";
						image = "/Library/PreferenceLoader/Preferences/SSLKillSwitch.png";
					},
				);
				"footer-title" = "SSL Kill Switch v0.61 - iSEC Partners";
			},
			{
				title = "User Applications";
				predicate = "isSystemApplication = FALSE";
				"cell-class-name" = ALSwitchCell;
				"icon-size" = 29;
				"suppress-hidden-apps" = 1;
			},
			{
				title = "System Applications";
				predicate = "isSystemApplication = TRUE";
				"cell-class-name" = ALSwitchCell;
				"icon-size" = 29;
				"suppress-hidden-apps" = 1;
				"footer-title" = "2012-2014 nabla-c0d3";
			},
		);
	};
}
~~~

此文件可以直接用json格式，也可以用plutil转为XML。主要是设置 **ALSettingsPath** 为保存设置的plist位置，并指定 **ALSettingsKeyPrefix** 这个设置前缀，其他照常处理。这里可以先写个简单的plist，然后放到机器上的 /Library/PreferenceLoader/Preferences 里慢慢调，其他可用的Cell类型和属性名，可以参考 [Preferences specifier plist](http://iphonedevwiki.net/index.php/Preferences_specifier_plist) 这个wiki。

另外 **ALSectionDescriptors** 里注意不能出现 PSSwitchCell，也许是因为ALSwitchCell是继承于PSSwitchCell，加上会导致异常。本来想自定义一个单独的UISwitch，不过加上后就变成没有图标的程序列表了...

最后在Tweak.m里加上bundleIdentifier获取与plist查询功能，就实现了针对单个App增加Hook的SSL Kill Switch。代码fork在[GitHub](https://github.com/upbit/ios-ssl-kill-switch)，这是编译好的deb：

<span style="color:#f00;">**deb下载：**</span>[com.isecpartners.nabla.sslkillswitch_v0.61-iOS_8.1.deb](http://blog.imaou.com/SSLKillSwitch/com.isecpartners.nabla.sslkillswitch_v0.61-iOS_8.1.deb) - (兼容iOS8.1，需要**AppList**)
