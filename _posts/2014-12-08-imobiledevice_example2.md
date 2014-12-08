---
layout: post
title: python-imobiledevice教程02 - 通过DebugServerClient服务远程运行和调试程序
description: "python-imobiledevice example 02"
category: opensource
comments: true
share: true
---

为了加深对libimobiledevice的理解，这两天用Python重写了几个核心服务的调用例子，放在了[GitHub/python-imobiledevice_demo](https://github.com/upbit/python-imobiledevice_demo)。这是教程的第二篇，通过DebugServerClient从命令行运行和调试程序。

## 环境准备:cython-dev

libimobiledevice目前的Cython binding没有[debugserver.pxi](https://github.com/upbit/libimobiledevice/blob/cython-dev/cython/debugserver.pxi)，可以使用我修改的[libimobiledevice/cython-dev这个分支](https://github.com/upbit/libimobiledevice/tree/cython-dev)来重新编译cython binding，增加DebugServerClient：

~~~sh
$ git clone https://github.com/upbit/libimobiledevice.git
$ cd libimobiledevice/
$ git fetch
$ git checkout cython-dev
$ ./autogen.sh
$ make
$ cd cython/
$ make install
~~~

这样安装的cython-binding就带有 DebugServerClient 了。

## 通过DebugServerClient运行APP

参考[idevicedebug.c的代码](https://github.com/libimobiledevice/libimobiledevice/blob/master/tools/idevicedebug.c#L346)，可以很容易实现与com.apple.debugserver的交互。
DebugServerClient的命令都是以DebugServerCommand来封装的，用法如下：

~~~python
  debugserver = lockdown_get_service_client(DebugServerClient)

  # SetLogging: bitmask=Log等级
  with DebugServerCommand("QSetLogging:bitmask=LOG_ALL|LOG_RNB_REMOTE|LOG_RNB_PACKETS") as cmd:
    print "Setting logging bitmask: %s" % debugserver.send_command(cmd)
  # SetMaxPacketSize: 1024
  with DebugServerCommand("QSetMaxPacketSize:", 1, ["1024"]) as cmd:
    print "Setting maximum packet size: %s" % debugserver.send_command(cmd)
  # SetWorkingDir: <app_root>
  with DebugServerCommand("QSetWorkingDir:", 1, [app_root]) as cmd:
    print "Setting working directory: %s" % debugserver.send_command(cmd)
~~~

com.apple.debugserver的输入与输出都是以一个大写字母打头的，表示后面数据的类型，QSetWorkingDir就是设置SetWorkingDir的内容，也就是APP的运行目录。

接着传递参数来启动应用：

~~~python
  response = debugserver.set_argv(1, [app_bin_path])
  print "Setting argv: %s" % response

  # If return "Efailed to get the task for process XXX",
  # 	add "get-task-allow = True" in entitlements.plist
  with DebugServerCommand("qLaunchSuccess") as cmd:
    print "Checking if launch succeeded: %s" % debugserver.send_command(cmd)
~~~

debugserver.set_argv() 就是传递启动参数了，远程调试端口也可以在这里添加。因为这个不是命令，不需要debugserver.send_command()，之后查询是否启动成功。

如果应用闪退并返回 "Efailed to get the task for process XXX" 则说明App没有 get-task-allow，需要用 ldid -Sent.plist <app_bin> 签上允许调试的 get-task-allow = True

如果程序运行，就可以用c(continue)继续运行目标程序了：

~~~python
  # Setting on Thread 0
  with DebugServerCommand("Hc0") as cmd:
    print "Setting thread: %s" % debugserver.send_command(cmd)

  # c: continue
  cmd_continue = DebugServerCommand("c")
  loop_response = debugserver.send_command(cmd_continue)

  # waiting Ctrl+C to exit
  while True:
    try:
      if (loop_response == None):
        break

      # 获取返回并发送reply:OK，继续等待DebugServer的回应
      result, loop_response = debugserver.handle_response(loop_response, reply=True)
      if result:
        sys.stdout.write(result)

      time.sleep(100/1000.0)

    except (KeyboardInterrupt, SystemExit):
      print "Exiting..."
      break
~~~

## 通过InstallationProxyClient获取应用的PATH

cython-dev分支里，还加上了 instproxy_client_get_path_for_bundle_identifier() 函数，用于从bundle_id获取应用的path。用法如下：

~~~python
  APP_BUNDLE_ID = "com.malcolmhall.WiFiPasswords"
  instproxy = lockdown_get_service_client(InstallationProxyClient)
  print instproxy.get_path_for_bundle_identifier(APP_BUNDLE_ID)
~~~

<span style="color:#f00;">**完整源码**</span>可以在GitHub找到：[`debugserver_app_runner.py`](https://github.com/upbit/python-imobiledevice_demo/blob/master/debugserver_app_runner.py)
