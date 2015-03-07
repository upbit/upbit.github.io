---
layout: post
title: 使用最新FLEX代码编译 FLEX injected 并打包
description: "Make FLEX injected with latest FLEX"
category: erlang
comments: true
share: true
---

昨天听朋友提到[Flipboard/FLEX](https://github.com/Flipboard/FLEX)这个项目，还有个用theos写的Tweak：[FLEX_injected](https://github.com/dtrukr/FLEX_injected)。不过用作者自带的deb安装无效，于是自己重新了个。

<span style="color:#F00">**下载地址**</span>：[`com.daapps.FLEXInjected_0.0.1-FLEX_e3612e3.deb`]({{ site.url }}/assets/download/com.daapps.FLEXInjected_0.0.1-FLEX_e3612e3.deb)

注入微信的效果(iPhone4S/iOS8.1下测试通过):

![index hello]({{ site.url }}/images/201502/FLEX_injected_wechat.png)

## 编译方法

首先尝试按作者的方法编译，make package时因为FLEX版本不对无法继续。

于是自己看了下这个tweak，思路是将FLEX代码中的.m/.h复制到temp目录里，然后Makefile里编译并和Tweak.xm打包在一起。Tweak里核心就一句，在AppList里选中的App里调用`[[FLEXManager sharedManager] showExplorer]`启动FLEX。

修改Makefile如下，就可以对最新的FLEX[e3612e31d7b42744805586c8804cb34e89c8a2d5](https://github.com/Flipboard/FLEX/commit/e3612e31d7b42744805586c8804cb34e89c8a2d5)进行编译了：

~~~makefile
TARGET = iphone::8.1
ARCHS = armv7 arm64
include theos/makefiles/common.mk

ADDITIONAL_CFLAGS = -fobjc-arc -Os -Qunused-arguments -Wno-deprecated-declarations -Itemp -Wno-c++11-extensions -Xclang -fobjc-runtime-has-weak

SDKVERSION = 8.1
INCLUDE_SDKVERSION = 8.1
TARGET_IPHONEOS_DEPLOYMENT_VERSION = 8.1
TARGET_CC = xcrun -sdk iphoneos clang
TARGET_CXX = xcrun -sdk iphoneos clang++
TARGET_LD = xcrun -sdk iphoneos clang++
SHARED_CFLAGS = -fobjc-arc

TWEAK_NAME = FLEXInjected

FLEXInjected_FILES = Tweak.xm temp/FLEXArgumentInputColorView.m temp/FLEXArgumentInputFontsPickerView.m temp/FLEXArgumentInputFontView.m temp/FLEXArgumentInputJSONObjectView.m temp/FLEXArgumentInputNotSupportedView.m temp/FLEXArgumentInputNumberView.m temp/FLEXArgumentInputStringView.m temp/FLEXArgumentInputStructView.m temp/FLEXArgumentInputSwitchView.m temp/FLEXArgumentInputTextView.m temp/FLEXArgumentInputView.m temp/FLEXArgumentInputViewFactory.m temp/FLEXDefaultEditorViewController.m temp/FLEXFieldEditorView.m temp/FLEXFieldEditorViewController.m temp/FLEXIvarEditorViewController.m temp/FLEXMethodCallingViewController.m temp/FLEXPropertyEditorViewController.m temp/FLEXExplorerToolbar.m temp/FLEXExplorerViewController.m temp/FLEXManager.m temp/FLEXToolbarItem.m temp/FLEXWindow.m temp/FLEXClassesTableViewController.m temp/FLEXFileBrowserSearchOperation.m temp/FLEXFileBrowserTableViewController.m temp/FLEXGlobalsTableViewController.m temp/FLEXInstancesTableViewController.m temp/FLEXLibrariesTableViewController.m temp/FLEXLiveObjectsTableViewController.m temp/FLEXWebViewController.m temp/FLEXSystemLogMessage.m temp/FLEXSystemLogTableViewCell.m temp/FLEXSystemLogTableViewController.m temp/FLEXArrayExplorerViewController.m temp/FLEXClassExplorerViewController.m temp/FLEXDefaultsExplorerViewController.m temp/FLEXDictionaryExplorerViewController.m temp/FLEXGlobalsTableViewControllerEntry.m temp/FLEXImageExplorerViewController.m temp/FLEXLayerExplorerViewController.m temp/FLEXObjectExplorerFactory.m temp/FLEXObjectExplorerViewController.m temp/FLEXSetExplorerViewController.m temp/FLEXViewControllerExplorerViewController.m temp/FLEXViewExplorerViewController.m temp/FLEXHeapEnumerator.m temp/FLEXResources.m temp/FLEXRuntimeUtility.m temp/FLEXUtility.m temp/FLEXHierarchyTableViewCell.m temp/FLEXHierarchyTableViewController.m temp/FLEXImagePreviewViewController.m temp/FLEXNetworkTransactionDetailTableViewController.m temp/FLEXNetworkTransactionTableViewCell.m temp/FLEXNetworkTransaction.m temp/FLEXNetworkSettingsTableViewController.m temp/FLEXNetworkRecorder.m temp/FLEXArgumentInputDateView.m temp/FLEXFileBrowserFileOperationController.m temp/FLEXMultilineTableViewCell.m temp/FLEXNetworkHistoryTableViewController.m temp/FLEXNetworkObserver.m
FLEXInjected_LDFLAGS = -lz
FLEXInjected_FRAMEWORKS = UIKit CoreGraphics QuartzCore ImageIO

BUNDLE_NAME = FLEXInjectedBundle
FLEXInjectedBundle_INSTALL_PATH = /Library/MobileSubstrate/DynamicLibraries
include $(THEOS)/makefiles/bundle.mk

include $(THEOS_MAKE_PATH)/tweak.mk

after-install::
	install.exec "killall -9 SpringBoard"
~~~

除了增加了几个 FLEXNetwork 相关的文件，还有个ImageIO需要引用。将上面内容替换原来的Makefile，再./make.sh就可以生成deb了。
