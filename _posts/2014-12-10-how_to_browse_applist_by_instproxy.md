---
layout: post
title: python-imobiledevice教程03 - 通过InstallationProxyClient获取iOS中安装的应用信息
description: "How to browse applist by InstallationProxyClient"
category: opensource
comments: true
share: true
---

之前看InstallationProxyClient时一直不知道 instproxy_browse 怎么使用，今天看Pangu8里面得到个详细的方法：

![Pangu8 _instproxy_browse]({{ site.url }}/images/201412/pangu8_instproxy_browse_applist.png)

写成Python验证如下：

~~~python
import plist
from imobiledevice import *

def lockdown_get_service_client(service_class):
  ld = LockdownClient(iDevice())
  return ld.get_service_client(service_class)

def list_installed_app(app_type="Any"):
  instproxy = lockdown_get_service_client(InstallationProxyClient)

  client_options = plist.Dict({
    "ApplicationType": app_type,
    "ReturnAttributes": plist.Array([
      "CFBundleIdentifier",
      "CFBundleExecutable",
      "Container",
    ]),
  })

  result_list = instproxy.browse(client_options)
  for data in result_list:
    print data
~~~

ApplicationType的输入可以是 System(系统应用), User(用户App), Any(全部)，然后 ReturnAttributes 可以指定需要的内容。之前没带ReturnAttributes导致返回数据过多，然后core在plist的输出上了。这里参考 Pangu8 的返回 CFBundleIdentifier/CFBundleExecutable/Container 三个选项。前两个就不用介绍了，Container则是iOS8中的Container目录，也就是存储数据的文件夹，返回的例子如下：

~~~python
{'CFBundleIdentifier': u'com.apple.PhotosViewService', 'CFBundleExecutable': u'PhotosViewService'}
{'CFBundleIdentifier': u'com.saurik.Cydia', 'CFBundleExecutable': u'Cydia'}
{'CFBundleIdentifier': u'com.apple.mobilesafari', 'Container': u'/private/var/mobile/Containers/Data/Application/156B33D2-3D28-4830-A78A-87A4957DECB4', 'CFBundleExecutable': u'MobileSafari'}
...
~~~

其他可选字段还有：

~~~
ApplicationDSID
ApplicationType
BuildMachineOSBuild
CFBundleDevelopmentRegion
CFBundleDisplayName
CFBundleDocumentTypes
CFBundleExecutable
CFBundleIcons
CFBundleIdentifier
CFBundleInfoDictionaryVersion
CFBundleName
CFBundleNumericVersion
CFBundlePackageType
CFBundleShortVersionString
CFBundleSignature
CFBundleSupportedPlatforms
CFBundleURLTypes
CFBundleVersion
Container
DTCompiler
DTPlatformBuild
DTPlatformName
DTPlatformVersion
DTSDKBuild
DTSDKName
DTXcode
DTXcodeBuild
Entitlements
EnvironmentVariables
Fabric
IsUpgradeable
LSRequiresIPhoneOS
MinimumOSVersion
NSPhotoLibraryUsageDescription
Path
SequenceNumber
SignerIdentity
UIDeviceFamily
UILaunchImages
UIMainStoryboardFile
UIPrerenderedIcon
UIStatusBarHidden
UIStatusBarStyle
UIStatusBarTintParameters
UISupportedInterfaceOrientations
UTExportedTypeDeclarations
UTImportedTypeDeclarations
~~~

比如 CFBundleName(应用名) 以及 CFBundleVersion(版本号)。

<span style="color:#f00;">**完整源码**</span>可以在GitHub找到：[`instproxy_browse_installed_app.py`](https://github.com/upbit/python-imobiledevice_demo/blob/master/instproxy_browse_installed_app.py)
