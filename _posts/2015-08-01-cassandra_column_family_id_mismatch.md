---
layout: post
title: 解决Cassandra Schema不一致导致的 Column family ID mismatch
description: "Fix Cassandra schema inconsistency when Column family ID mismatch"
category: opensource
comments: true
share: true
---

说到Cassandra，一般会想到为了支持分页不得不将数据存成多份，每当这个时候都会无比怀念MySQL里想怎么撸就怎么撸的便利性。而实际使用中，最讨厌的还要属"Column family ID mismatch"。从Cassandra的issues列表就能看到，无论在哪个版本都存在CF ID不一致导致的问题：[CASSANDRA-8387](https://issues.apache.org/jira/browse/CASSANDRA-8387) / [CASSANDRA-6038](https://issues.apache.org/jira/browse/CASSANDRA-6038) / [CASSANDRA-5202](https://issues.apache.org/jira/browse/CASSANDRA-5202)，而实际上就算线上更新到Cassandra 2.1.5，不小心还是会遇到多节点同时建表导致的同步异常。各种血的教训下终于明白，不要重复修改Schema！不要重复修改Schema！不要重复修改Schema！（因为很重要所以说三遍

## Column family ID mismatch 的原因

众所周知，Cassandra是一个无中心的分布式数据库，其CREATE/DROP/ALTER等对Schema的修改操作，都是在一个节点上完成后，再同步给集群其它节点的。当一个以上节点同时执行修改Schema操作（例如创建新的Column family，哪怕加了IF NOT EXISTS），此时多个节点彼此都认为没有这张表，于是为这张表各自生成了一个Column family ID。于是等他们各自完成本地的操作开始同步时，灾难就降临了：

~~~
ERROR [Thrift:15] 2015-07-31 11:24:54,781 CustomTThreadPoolServer.java:224 - Error occurred during processing of message.
java.lang.RuntimeException: java.util.concurrent.ExecutionException: java.lang.RuntimeException: org.apache.cassandra.exceptions.ConfigurationException: Column family ID mismatch (found 1660db50-3717-11e5-bb49-4d1f4d3a1785; expected 16490d90-3717-11e5-baae-43ee31cfdaef)
~~~

可以看到该节点接收到一个`1660db50-3717-11e5-bb49-4d1f4d3a1785`的CFID，但本机的内容为`16490d90-3717-11e5-baae-43ee31cfdaef`。遇上这种情况如果这个表有多个replicas(copys)，可能会因为无法同步导致写入阻塞或超时。

也许你也曾经和我一样，天真的以为作者会在新版里修复。按Aleksey Yeschenko在CASSANDRA-8387里的回复，CASSANDRA-6038(大概3.x)里将应用一个并没有实际改变的新协议，尝试修复这个问题...

~~~
Your issue is a consequence of CASSANDRA-5202, that made table uuids non-deterministic.
I don't see a good way to fix this in 2.1. I will try to handle this scenario in CASSANDRA-6038, with the new schema change protocol, but even then, I don't see an immediate solution - yet.
~~~

曾经因为两次ALTER TABLE了同一张表，而把整个keyspace弄得卡死无法写入，最后无意中rolling重启所有节点后问题解决。今天则无意中看到[Cassandra keyspace does not propagate to newly added node](http://stackoverflow.com/a/27629161)的回答，也是提及如何解决Column family ID mismatch：

~~~
1. stop the Cassandra service/process, typically by running: nodetool drain
2. Remove the Schema* and Migration* sstables inside of your system keyspace
3. After starting Cassandra again, this node will notice the missing information and pull in the correct schema from one of the other nodes.
~~~

其实2做不做无所谓，2.x的Cassandra重启后会找其他节点拉取正确的schema，让整个集群重新趋于一致。首先摘掉当前节点：

~~~
nodetool disablegossip
nodetool disablethrift
nodetool drain
nodetool stopdaemon
~~~

具体解释可以参看[Safe Cassandra shutdown and restart](http://devblog.michalski.im/2012/11/25/safe-cassandra-shutdown-and-restart/)，先禁止gossip和thrift协议，然后用drain禁写并flush所有MemTables到SSTables，这样就可以安全的stopdaemon了。

停止Cassandra后，往往不用去data下删除对应keyspace的数据和commitlog，直接启动Cassandra后它会自己load SSTable并且replaying commitlog：

~~~
daemon --user cassandra "cassandra" -p $pidfile
~~~

重启节点后可能出现`UnknownColumnFamilyException: Couldn't find cfId=16490d90-3717-11e5-baae-43ee31cfdaef`错误，这是因为其他机器上有错误的schema，Handshake后发给重启后节点找不到对应的meta信息：

~~~
WARN  [Thread-21] 2015-07-31 12:08:15,879 IncomingTcpConnection.java:94 - UnknownColumnFamilyException reading from socket; closing
org.apache.cassandra.db.UnknownColumnFamilyException: Couldn't find cfId=16490d90-3717-11e5-baae-43ee31cfdaef
~~~

这个错误只要轮流将其他节点依次重启后，schema就会达到一致而停止报错。

不过还是没弄明白，为什么cqlsh里在写频繁的keyspace里建表，也会遇到`Column family ID mismatch`问题。常常是执行命令后提示`TSocket read 0 bytes`然后log里出现mismatch错误，目前只能推测cqlsh在遇到错误时有重试操作。

总之在Cassandra动态CREATE/DROP/ALTER Schema时，要注意一致性问题，不要在分布式的代码、cqlsh里对这样的操作进行重试。因为如果刚好这个操作落在两个不同节点上，就只能这样rolling重启Cassandra来解决mismatch问题了...
