---
layout: post
title: Elixir - Ecto的建模与查询教程
description: "Elixir - Use Ecto to query MySQL"
category: elixir
comments: true
share: true
---

最近两个月来用Elixir写了2个独立的项目，其中都涉及到Ecto查询MySQL。因为Ecto的models和query例子是分开的，一些特别的写法只有看源码才知道如何处理，这里记录下以便以后查阅。

## 自定义primary_key

migration和schema如下：

~~~elixir
defmodule Server.Repo.Migrations.CreatePosts do
  use Ecto.Migration

  def change do
    create table(:posts, primary_key: false) do
      add :id, :string, size: 40, null: false, primary_key: true
      add :user_id, :string, size: 40, null: false
      add :data, :text
      timestamps
    end
  end
end

defmodule Server.Post do
  use Ecto.Model

  @primary_key {:id, :string, autogenerate: false}
  schema "posts" do
    belongs_to :user, Server.User, type: :string
    field :text, :string
    field :data, :map, default: %{}
    timestamps
  end
end
~~~

posts使用自定义的`id varchar(40)`作为主键，于是用`@primary_key`宏声明。

## belongs_to的查询

如上面schema所示，用belongs_to声明与user表的从属关系，`belongs_to :user, Server.User`会使用posts.user_id字段，与User.id进行查询，这样声明后可以使用join或者preload加载内容。

~~~elixir
import Ecto.Query

from(p in Post)
|> where([p], p.id == ^post_id)
|> preload([p], ^:user)
|> Repo.one
~~~

执行上面语句可以看到，实际是先查询Post，然后用post.user_id查询对应的User记录，最后merge到Post.user中。如果不需要user对象，则可以用如下语句代替：

~~~elixir
Repo.get(Post, post_id)
~~~

当然，如果既想获得user里的信息，又想只用一个查询，灵活的Ecto也提供了inner join的方式：

~~~elixir
from(p in Post, join: assoc(p, :user))
|> where([p, u], p.id == ^post_id)
|> select([p, u], {p.id, p.text, p.data, u.id, u.name})
|> Repo.one
~~~

这样就可以把user里的name等字段提取出来了。join的写法比较灵活，具体可以参考Ecto的query_test里的例子。

## 使用fragment指定SQL片段

有时候查询需要区分大小写(比如邀请码)，而默认是不区分的。此时可以用`fragment("binary code=?", ^code)`来指定查询片段：

~~~elixir
from(i in Server.InviteCode)
|> where([i], fragment("binary code=?", ^code))
|> update([i], inc: [count: -1])
|> Repo.update_all([])
~~~

## 计数更新

如果是set操作，直接用`changeset(model, %{count: 99})`即可。不过如果是+1或者-1这种操作，则需要用到update/3。如上面query语句，在where后使用update指定inc或者dec的字段和value，接着调用Repo.update更新。

## `order_by`执行算式

比如按某几个字段的和逆序，可以这样写：`query |> order_by([f], fragment("(count1 + count2 + count3) DESC"))`

## 新增或者返回已存在记录

经常需要写根据open_id查询User对象，如果不存在则创建并继续。这种判断可以利用elixir nil的语法特性，写成inline语句：

~~~elixir
user = Repo.get_by(User, open_id: open_id) || %User{open_id: open_id, last_login_at: Ecto.DateTime.utc} |> Repo.insert!
~~~

`Repo.get_by`是查询非`:id`键的结果，如果不存在则执行后面的`%User{} |> Repo.insert!`，并返回新增的User对象

## 两个实用的库

* [EctoEnum](https://github.com/gjaldon/ecto_enum) 用于生成枚举类型，例如`defenum Type, text: 0, photo: 1`，这样就可以将Ecto的:integer转换成atom的:text/:photo，方便代码阅读。
* [Maru.Entity](https://github.com/teodor-pripoae/maru_entity) 一个仿照grape-entity的库，如果你用maru的话，这个能很方便的将models转换成各种场景的输出结果。
