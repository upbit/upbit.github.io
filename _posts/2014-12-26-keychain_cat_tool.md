---
layout: post
title: keychain_cat - 查看/修改keychain2数据的工具
description: "keychain_cat tool"
category: theos
comments: true
share: true
---

最近需要修改iOS keychain中kSecClassGenericPassword的数据，不过没有找到方便的工具，于是抽时间自己写了一个。工具可以批量查询/删除，或者更新kSecClassGenericPassword中的单条v_Data记录，方便进行调试。[keychain_cat的源码](https://github.com/upbit/My-iDevice-Tools/blob/master/keychain_cat.mm) 放在GitHub上。

---------------

## 使用方法

~~~
$ ./keychain_cat -h
Usage: ./keychain_cat [options]
-d --dump                       Dump Keychain AccessGroups
-U --update                     UPDATE v_Data with specified value <-g> <-s> <-a> <-v>
-D --delete                     DELETE keychain with <-g> (-s) (-a)
-g --group <AccessGroup>        kSecAttrAccessGroup
-s --service <Service>          kSecAttrService
-a --account <Account>          kSecAttrAccount
-v --value <v_Data>             (UPDATE only) kSecValueData

<SecClass selector>
  -G --generic-password         kSecClassGenericPassword
  -N --internet-password        kSecClassInternetPassword
  -I --identity                 kSecClassIdentity
  -C --certificate              kSecClassCertificate
  -K --classKey                 kSecClassKey

-h --help                       Show this help
~~~

### [1] 查看 AccessGroup

因为修改需要知道 keychain-access-groups。这里参考 [Keychain-Dumper](https://github.com/upbit/Keychain-Dumper/blob/master/main.m#L56) 里的 dumpKeychainEntitlements()，实现列举机器上所有 AccessGroup功能：

~~~
$ ./keychain_cat -d
>> keychain-access-groups:
  6WX5RKLG95.com.supercell.reef
  88L2Q4487U.com.tencent.mttlite
  apple
  com.apple.ProtectedCloudStorage
  com.apple.PublicCloudStorage
  com.apple.apsd
  com.apple.assistant
  com.apple.cloudd
  com.apple.ind
  com.apple.security.sos
~~~

### [2] 查看某个 AccessGroup 下的所有数据

知道 AccessGroup 的名称，就可以使用 --group 来指定查询目标：

~~~
./keychain_cat -g 6WX5RKLG95.com.supercell.reef
<AccessGroup:6WX5RKLG95.com.supercell.reef, Service:com.supercell, Account:appRated>
{
  accc = "<SecAccessControlRef: 0x15563b70>";
  acct = appRated;
  agrp = "6WX5RKLG95.com.supercell.reef";
  cdat = "2014-11-08 23:33:28 +0000";
  class = genp;
  invi = 1;
  labl = Supercell;
  mdat = "2014-11-08 23:33:28 +0000";
  pdmn = ak;
  svce = "com.supercell";
  sync = 0;
  tomb = 0;
  "v_Data" = TRUE;
}
...
<AccessGroup:6WX5RKLG95.com.supercell.reef, Service:com.supercell, Account:THLevel>
{
  accc = "<SecAccessControlRef: 0x15563b80>";
  acct = THLevel;
  agrp = "6WX5RKLG95.com.supercell.reef";
  cdat = "2014-12-07 12:03:07 +0000";
  class = genp;
  invi = 1;
  labl = Supercell;
  mdat = "2014-12-07 12:03:07 +0000";
  pdmn = ak;
  svce = "com.supercell";
  sync = 0;
  tomb = 0;
  "v_Data" = 13;
}
~~~

### [3] 查看某个 AccessGroup,Service,Account 的数据

当然还可以用 --service 和 --account 来进一步筛选结果。特别的，如果可能v_Data会自动解析成NSString:

~~~
$ ./keychain_cat -g 6WX5RKLG95.com.supercell.reef -s com.supercell -a THLevel
<AccessGroup:6WX5RKLG95.com.supercell.reef, Service:com.supercell, Account:THLevel>
{
  accc = "<SecAccessControlRef: 0x16665dc0>";
  acct = THLevel;
  agrp = "6WX5RKLG95.com.supercell.reef";
  cdat = "2014-12-07 12:03:07 +0000";
  class = genp;
  invi = 1;
  labl = Supercell;
  mdat = "2014-12-07 12:03:07 +0000";
  pdmn = ak;
  svce = "com.supercell";
  sync = 0;
  tomb = 0;
  "v_Data" = 13;
}
~~~

### [4] 修改某个 com.supercell,THLevel 的数据

当指定 --group,--service,--account 后，会定位到单独的一条记录，此时可以使用 --update 来更新数据。使用 --value 指定要修改的v_Data内容（字符串）。

比如修改 com.supercell,THLevel 的v_Data为 99：

~~~
./keychain_cat -g 6WX5RKLG95.com.supercell.reef -s com.supercell -a THLevel -v 99 -U
Origin: {
  accc = "<SecAccessControlRef: 0x146798d0>";
  acct = THLevel;
  agrp = "6WX5RKLG95.com.supercell.reef";
  cdat = "2014-12-07 12:03:07 +0000";
  invi = 1;
  labl = Supercell;
  mdat = "2014-12-07 12:03:07 +0000";
  pdmn = ak;
  svce = "com.supercell";
  sync = 0;
  tomb = 0;
  "v_Data" = <3133>;
}
>> Update v_Data to: <3939>
~~~

可以看到，写入的NSData实际是 <3939>，此时去掉 --update,--value 查看，会看到THLevel已经被改成字符串99了。

## 局限性

目前只支持修改 kSecClassGenericPassword 的v_Data，并且只支持字符串输入。

如果有其它需求，可以在GitHub上给我提issue：[https://github.com/upbit/My-iDevice-Tools/issues](https://github.com/upbit/My-iDevice-Tools/issues)
