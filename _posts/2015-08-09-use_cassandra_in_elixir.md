---
layout: post
title: 在Elixir中通过cqerl操作Cassandra的方法
description: "Use cqerl for Cassandra client in Elixir"
category: elixir
comments: true
share: true
---

项目中需要操作Cassandra，目前ecto不支持于是用了erlang的第三方库。这里记录下遇到的麻烦，算是为Elixir中使用Cassandra增加点资料吧。

## 配置[cqerl](https://github.com/matehat/cqerl)

首先cqerl是个erlang库，需要用源地址clone下来。在mix.exs的deps里加入`{:cqerl, git: "https://github.com/matehat/cqerl.git", tag: "v0.8.0"},`

然后是配置，参考官方erlang的写法转换是不行的，`cassandra_nodes`里配置并不会被cqerl读取：

~~~erlang
[
  {cqerl, [
            {cassandra_nodes, [ { "127.0.0.1", 9042 } ]},
            {ssl, [ {cacertfile, "cassandra.pem"} ]},
            {auth, {cqerl_auth_plain_handler, [ {"test", "aaa"} ]}}
          ]},
]
~~~

于是自己写个读配置的函数，从config.exs里拿：

~~~elixir
use Mix.Config

config :zserver, :cqerl,
  cassandra_node: {'127.0.0.1', 9042},
  keyspace: "test",
  consistency: 1
~~~

然后定义一个config函数用于读取Application的指定配置（注意修改zserver和config里的app名一致）：

~~~elixir
  # get config from config.exs
  def config(key) do
    Application.get_env(:zserver, key)
  end
  def config(key, sub_key) do
    config(key)[sub_key]
  end
~~~

之后就可以这样取`cassandra_node`里的配置内容了：`{:ok, client} = :cqerl.new_client(ZServer.config(:cqerl, :cassandra_node))`

不过这样没法像默认那样在多个节点间随机选择，暂时没研究出怎么让cqerl自己读elixir的配置。

## cql_query

用cqerl自然是要用到`cql_query/cql_query_batch`的，不过hrl是erlang的定义，要先转成elixir的Record，定义一个如下的module：

~~~elixir
defmodule cqerl.utils do
  require Record

  Record.defrecord :cql_query, Record.extract(:cql_query, from: "deps/cqerl/include/cqerl.hrl")
  Record.defrecord :cql_query_batch, Record.extract(:cql_query_batch, from: "deps/cqerl/include/cqerl.hrl")
end
~~~

在要使用的地方加上`import cqerl.utils`，之后就可以这样用了：

~~~elixir
  {:ok, resp} = :cqerl.run_query(client, cql_query(statement: "SELECT cluster_name FROM system.local ;"))
  # output
  IO.inspect :proplists.get_value(:cluster_name, :cqerl.head(resp))
~~~

## cql_query_batch

Batch在官方的例子里有，不过`CQERL_BATCH_UNLOGGED = 1`和`CQERL_BATCH_COUNTER = 2`没有宏，需要自己指定。比如UPDATE计数：

~~~elixir
  defp cql_add_matches_counters(client, matches) do
    query1 = cql_query(statement: "UPDATE demo_matches SET count = count + 1 WHERE uid = ? AND matched_uid = ? AND date = ?;")
    query2 = cql_query(statement: "UPDATE demo_matches_total SET count = count + 1 WHERE uid = ? AND matched_uid = ?;")

    # CQERL_BATCH_COUNTER = 2
    batch_query = cql_query_batch(mode: 2, queries: Enum.map(matches, fn({uid,matched_uid,ts}) ->
      [
        cql_query(query1, values: [{:uid, uid}, {:matched_uid, matched_uid},
                                   {:date, ts |> DateTimeUtils.ts_to_beginning_of_day |> DateTimeUtils.ts_to_milliseconds}]),
        cql_query(query2, values: [{:uid, uid}, {:matched_uid, matched_uid}])
      ]
    end) |> List.flatten)
    |> cql_query_batch(consistency: ZServer.config(:cqerl, :consistency))

    {:ok, :void} = :cqerl.run_query(client, batch_query)
    :ok
  end
~~~

指定mode=2`CQERL_BATCH_COUNTER`来批量更新counter字段，queries里可以带多张表的update操作。注意最后还有个设置`consistency`的动作，实际发现虽然在测试环境跑的一切正常，但线上却会报timeout，查了好久发现是没有设置consistency。

## 关于IN等复杂查询

试了半天，发现cqerl的values写法并不能像test里那样正确的获取in查询参数，最后只好直接拼查询并放在statement里。而TRUNCATE这样的语句，是无法在run_query里执行的，好在也就debug时用用。

elixir还是有些不顺手的地方，比如打release包后没有正确的监听服务器IP，以及[调整process_limit](https://groups.google.com/forum/#!topic/elixir-lang-talk/No1Qq0huj_E)。这个只能慢慢摸索了
