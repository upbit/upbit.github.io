---
layout: post
title: 使用Capstone增强lldb的 ARM/Thumb 代码反汇编能力
description: "Import Capstone in lldb for ARM/Thumb disassemble."
category: opensource
comments: true
share: true
---

用lldb调试armv7的代码后，总是为 dis -A thumb 部分代码显示不正确而烦恼。而手边又没有arm64的设备，常常是一边对着IDA一边s。最近接触到[Capstone](https://github.com/aquynh/capstone)，一个开源的反汇编框架，并且目前拥有python、java、go、node.js等众多bindings，于是想借用capstone来增强lldb的反汇编能力。

**实现的功能如下：**

1. 根据CPSR的Thumb标志判断arm指令的类型，并支持选择处理器类型 arm 或 arm64；
2. <span style="color:#f00;">调用capstone反汇编PC位置代码，或指定的代码段；</span>

先上效果图，完整的脚本可以到这里下载：[https://github.com/upbit/lldb-capstone-arm](https://github.com/upbit/lldb-capstone-arm)

![Screenshot](https://raw.github.com/upbit/lldb-capstone-arm/master/screenshot.png)

可以看到，dis -A thumb 无法识别的指令，均被正确的显示出来了。

## 安装方法

在MacOS上，先安装capstone和Python binding：

~~~sh
brew install capstone
sudo pip install capstone
~~~

接着复制 dis_capstone.py 到 ~/.lldb/ 目录，并将`command script import ~/.lldb/dis_capstone.py`加入~/.lldbinit文件中（不存在的话可以直接创建）

这样lldb启动时，就会显示如下提示，dis_capstone 或缩写 discs 就可以使用了：

~~~
The "discs (dis_capstone)" command has been installed
~~~

当然也可以在lldb里输入`command script import ~/.lldb/dis_capstone.py`来手动加载脚本

## 原理说明

核心函数如下，主要完成如下几个操作：

1. 从debugger中获取当前frame；
2. 调用 process.ReadMemory() 读取指定地址；
3. 使用指定的arch和mode初始化capstone.CS `md = Cs(disasm_arch, disasm_mode)`
4. 调用 md.disasm() 反汇编，并输出结果；

~~~python
def real_disassemble(debugger, start_addr, disasm_length, disasm_arch, disasm_mode):
	""" Disassemble code with target arch/mode """

	target = debugger.GetSelectedTarget()
	process = target.GetProcess()
	thread = process.GetSelectedThread()
	frame = thread.GetSelectedFrame()

	# read bytes
	error = lldb.SBError()
	bytes = process.ReadMemory(start_addr, disasm_length, error)

	if error.Success():
		# decode with capstone
		md = Cs(disasm_arch, disasm_mode)
		isFirstLine = True
		for insn in md.disasm(bytes, start_addr):
			if (isFirstLine):
				print("-> 0x%x:  %-16s %-8s %s" % (insn.address, bytes_to_hex(insn.bytes), insn.mnemonic, insn.op_str))
				isFirstLine = False
				continue

				print("   0x%x:  %-16s %-8s %s" % (insn.address, bytes_to_hex(insn.bytes), insn.mnemonic, insn.op_str))

			else:
				print "[ERROR] ReadMemory(0x%x): %s" % (start_addr, error)
~~~

## 小插曲

Capstone作者相当友善，在反馈了[Homebrew中capstone 3.0的问题](https://github.com/aquynh/capstone/issues/74#issuecomment-65342454)后，作者10小时不到就回复了：

~~~
confirmed this issue & working on a fix for Brew formula now, thanks.
~~~

~~~
this should be fixed now in Brew formula. can you update Brew (with brew update), then try to reinstall Capstone?

thanks.
~~~

另外作者更新的也相当勤快，赞一个！
