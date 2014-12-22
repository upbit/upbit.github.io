---
layout: post
title: 用dynamic_pager为 iOS7.1.2~iOS8.1 手动增加虚拟内存
description: "dynamic_pager for iOS 7.1.2+"
category: misc
comments: true
share: true
---

最近的游戏越来越消耗内存了，在我那服役多年的iPad2上，别说3d游戏常常连2d游戏都会闪退。LaunchDaemons已经精简再精简，可惜iOS7.1.2光系统后台就吃了一半(近128MB)的内存。于是想到给老机器增加虚拟内存。不过搜了个vm装上后，iPad陷入无限重启循环。。。
不过好在afc2有那么几秒能连上，在写了个死循环等待设备启动后删除LaunchDaemons下dynamic_pager的启动项后，又再次回到了系统。

[dynamic_pager](http://www.opensource.apple.com/source/system_cmds/system_cmds-597.90.1/dynamic_pager.tproj/dynamic_pager.c)是Apple自己的虚拟内存管理工具，Cydia里绝大多数虚拟内存都是依赖这个工具来建立虚拟内存的。手动运行发现，iOS7.1之后一旦以root权限运行就会导致kernel panic然后重启，这个就是之前装vm导致无限重启的原因了。

寻觅很久，终于发现dynamic_pager的正确用法：

[iOS7.1.2の脱獄環境で仮想メモリーを作る。〜手動編〜まとめ](http://infinitedarkblue.blog.jp/archives/41271874.html)

在iOS7.1.2和iOS8.1上测试，<span style="color:#F00">**必须使用mobile用户启动dynamic_pager才能正确创建swapfile文件**</span>。

**方法如下：**

1. 用mobile用户登如iOS设备: `ssh mobile@<IP>`；
2. su到root创建swapfile0，并chown为mobile: `cd /var/vm; touch swapfile0; chown mobile swapfile0`
3. 退回到mobile，以后台方式运行dynamic_pager: `dynamic_pager -S 536870912 -H 1024 -L 536872000 -P 1`；

dynamic_pager参数说明：

~~~
dynamic_pager [-F filename] [-L low water alert trigger] [-H high water alert trigger] [-S file size] [-P priority]

-F filename: 指定swapfile的路径，默认为 /private/var/vm/swapfile
-L low water alert trigger: 设置swapfile总剩余空间多于多少字节时删除空闲的交换文件
-H high water alert trigger: 设置当swapfile的总剩余空间低于多少字节时候创建新的交换文件
-S file size: 指定swapfile的文件大小
-P priority: 优先级
~~~

所以 `dynamic_pager -S 536870912 -H 1024 -L 536872000 -P 1` 就是几乎无条件的(-H 1KB)创建一个512MB的swapfile。当然如果512MB都用完了，可能需要再创建个swapfile1来保证dynamic_pager的正常运行。
