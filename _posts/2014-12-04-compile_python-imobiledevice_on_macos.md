---
layout: post
title: 搞定MacOS上的python-imobiledevice，两套Python真心害死人
description: "Compile python-imobiledevice on MacOS"
category: opensource
comments: true
share: true
---

最近需要研究libimobiledevice，总写c的代码测试不方便吧，于是翻到GitHub有个[cython binding](https://github.com/libimobiledevice/libimobiledevice/tree/master/cython)。几番周折下终于把这个 python-imobiledevice 搞定了，上图：

![python-imobiledevice]({{ site.url }}/images/201412/python-imobiledevice1.png)

下面是折腾的过程，希望对其他想要在MacOS上安装python-imobiledevice的同学有所帮助。

## libimobiledevice Cython binding

libimobiledevice有个Cython binding，不过找遍网络也没有说明文档，而且python-imobiledevice也不能通过pip安装。无奈之下尝试用Homebrew安装`brew install libimobiledevice --with-python`，但进入Python就发现问题了：

~~~python
>>> import imobiledevice
ImportError: No module named imobiledevice
~~~

压根就没有安装python的package嘛。于是`git clone https://github.com/libimobiledevice/libimobiledevice.git`了一份最新源码，打算从源码安装。`./configure`后发现Python binding没有启用，检查了下是libplist依赖问题，之前Homebrew安装的版本没有cython支持。

~~~sh
$ brew install libplist --with-python
~~~

重新安装有python支持的libplist后，进入Python试了下：

~~~python
>>> import plist
Fatal Python error: PyThreadState_Get: no current thread
~~~

然后噩梦开始了。看GitHub上说是多格Python版本冲突引起的，关于这个错误[stackoverflow上有个更详细的解释](http://stackoverflow.com/questions/15678153/homebrew-python-on-mac-os-x-10-8-fatal-python-error-pythreadstate-get-no-cu)，按我的理解是libplist引用了系统版本的Python库，但我是用Homebrew安装的Python2.7.8跑的。`otool -L <dyld>`查看确实存在问题，Homebrew安装的libplist指向了/System/Library/Frameworks/Python.framework/下的Python2.7，也就是系统的Python。

折腾了一阵后想到办法，既然libplist先找系统的Python.framework，那我让它找不到就好了。于是给/System/Library/Frameworks/Python.framework/改了名，用libplist的源码重新编译：

~~~sh
$ git clone https://github.com/libimobiledevice/libplist.git
$ cd libplist/
$ ./autogen.sh
~~~

这里不明白为什么默认Python binding没有启用，于是我直接patch了configure文件强制让build_cython=true。接着再运行提示：

~~~makefile
configure: error:
	Could not link test program to Python. Maybe the main Python library has been
	installed in some non-standard library path. If so, pass it to configure,
	via the LDFLAGS environment variable.
	Example: ./configure LDFLAGS="-L/usr/non-standard-path/python/lib"
	============================================================================
	ERROR!
	You probably have to install the development version of the Python package
	for your distribution.  The exact name of this package varies among them.
	============================================================================
~~~

也就是已经找不到Python的lib目录了，于是手动给个。先用`python-config --prefix`找到当前Python的安装位置，加/lib/传给LDFLAGS：

~~~sh
$ python-config --prefix
/usr/local/Cellar/python/2.7.8_2/Frameworks/Python.framework/Versions/2.7
$ ./autogen.sh LDFLAGS="-L/usr/local/Cellar/python/2.7.8_2/Frameworks/Python.framework/Versions/2.7/lib/"
~~~

然后顺利`make && make install`到/usr/local/include/plist/和/usr/local/lib/

这里还有个小问题，libplist的cython支持没有正确复制，需要手动copy过去：

~~~sh
$ cd cython/
$ mkdir /usr/local/include/plist/cython
$ cp plist.p* /usr/local/include/plist/cython/
~~~

再用同样方法源码编译libimobiledevice，顺利找到Python binding：

~~~sh
$ cd ../../
$ cd libimobiledevice/
$ ./configure LDFLAGS="-L/usr/local/Cellar/python/2.7.8_2/Frameworks/Python.framework/Versions/2.7/lib/"
~~~

`make && make install`后，python-imobiledevice就可用了。
