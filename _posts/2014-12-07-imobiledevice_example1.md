---
layout: post
title: python-imobiledevice教程01 - 使用AfcClient和InstallationProxyClient后台安装应用
description: "python-imobiledevice example 01"
category: opensource
comments: true
share: true
---

为了加深对libimobiledevice的理解，这两天用Python重写了几个核心服务的调用例子，放在了[GitHub/python-imobiledevice_demo](https://github.com/upbit/python-imobiledevice_demo)。这是教程的第一篇，讲解如何通过AfcClient和InstallationProxyClient实现上传IPA文件和后台安装App。

python调用imobiledevice依赖于 python-imobiledevice，可以获取[libimobiledevice](https://github.com/libimobiledevice/libimobiledevice)的Cython binding (包名为python-imobiledevice)；MacOS也可以参考之前的[搞定MacOS上的python-imobiledevice，两套Python真心害死人](http://blog.imaou.com/opensource/2014/12/04/compile_python-imobiledevice_on_macos.html)一文来从源码安装。

## imobiledevice基础

使用python-imobiledevice需要先引入imobiledevice：

~~~python
from imobiledevice import *
~~~

imobiledevice.iDevice() 会自动寻找已连接的iOS设备，或者可以指定UDID来等待某个设备的接入，设备的UDID可以通过libimobiledevice自带的`idevice_id`命令来查看。

设备上的服务，大多是通过LockdownClient来启动的，定义函数来从lockdown获取service_client：

~~~python
def lockdown_get_service_client(service_class):
	ld = LockdownClient(iDevice())
	return ld.get_service_client(service_class)
~~~

之后就可以通过服务的Client名来获取对应的客户端了：

~~~python
  afc = lockdown_get_service_client(AfcClient)
  instproxy = lockdown_get_service_client(InstallationProxyClient)
~~~

其中LockdownClient().get_service_client()实际上是[内部封装了start_service](https://github.com/libimobiledevice/libimobiledevice/blob/master/cython/lockdown.pxi#L210)，`afc = lockdown_get_service_client(AfcClient)`等价于：

~~~python
	dev = iDevice()
	ld = LockdownClient(dev)
	svrport = ld.start_service(AfcClient)
	afc = AfcClient(dev, svrport)
~~~

其中`ld.start_service(AfcClient)`虽然传递的是Class，实际获取的是[cython/afc.pxi](https://github.com/libimobiledevice/libimobiledevice/blob/master/cython/afc.pxi#L171)中的 AfcClient.__service_name__，即 com.apple.afc。此时就可以AfcClient与AFC服务进行交互了。

## 使用AfcClient上传IPA文件

首先写一个上传文件的函数，将IPA的payload写入到iDevice中：

~~~python
def afc_upload_file(filename, local_stream):
	afc = lockdown_get_service_client(AfcClient)

	# 使用afc服务打开filename文件，并写入local_stream中的所有内容
	testipa = afc.open(filename, mode="w+")
	testipa.write(local_stream.read())
	testipa.close()
~~~

这样，调用 `afc_upload_file("test.ipa", open("payload/pangunew.ipa"))` 就会将本地 payload/pangunew.ipa 的文件，写入到AFC根目录(默认为/private/var/mobile/Media)下的 test.ipa 文件中了。

相应的，afc还提供了其他目录文件操作，例如创建目录`afc.make_directory(path)`或删除文件/目录`afc.remove_path(path)`。不过这些操作都被限制在AFC的根目录/private/var/mobile/Media中。

另外还有两个很有用的函数：

~~~python
	# 显示 / 下的所有文件和目录
	print afc.read_directory("/")
	# 获取test.ipa的信息
	print afc.get_file_info("test.ipa")
~~~

## 使用InstallationProxyClient安装上传的IPA

com.apple.mobile.installation_proxy 是一个用来查看/安装/升级/卸载/管理用户App的服务，这里只用到其中的upgrade()函数。更多可以参考libimobiledevice作者的另一个作品[ideviceinstaller](https://github.com/libimobiledevice/ideviceinstaller)，其中使用了InstallationProxyClient的几乎全部功能。

这里用到的是upgrade而不是install，是因为upgrade不会理会App是否已经存在。另外参数部分第一次用到libplist，直接传一个空Dict即可：

~~~python
import plist
def instproxy_install_file(filename):
	instproxy = lockdown_get_service_client(InstallationProxyClient)
	instproxy.upgrade(filename, plist.Dict({}))
~~~

另外演示InstallationProxyClient的一个功能，通过browse()查看已安装应用的信息：

~~~python
	# dump application info
	client_options = plist.Dict({
		"ApplicationType": "User",		# Any, System, User
	})
	for app in instproxy.browse(client_options):
		print "[CFBundleIdentifier] %s" % app["CFBundleIdentifier"]
		print "[EnvironmentVariables] %s" % app["EnvironmentVariables"]
~~~

不过注意，目前libplist对中文支持好像有问题，导出System列表时居然引起Python core dump。反正不是很常用，不管了...

<span style="color:#f00;">**完整源码**</span>可以在GitHub找到：[afc_and_instproxy_upgrade_ipa.py](https://github.com/upbit/python-imobiledevice_demo/blob/master/afc_and_instproxy_upgrade_ipa.py)
