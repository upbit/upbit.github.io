---
layout: post
title: Elixir - 升级 maru v0.3.1 与maru_swagger的配置
description: "Elixir - upgrade maru to v0.3.1, and how to setup maru seagger"
category: erlang
comments: true
share: true
---

这几个月过得真有点七荤八素，连blog的更新都搁置了。包括之前升级[PixivPy](https://github.com/upbit/pixivpy)遇到的问题，以及学习Elixir时遇到的难点。说实话，Elixir写代码比Erlang要简单，用上[Plug](https://github.com/elixir-lang/plug)这样神奇的库后，处理请求再也不像之前那样令人头疼。

## SwaggerUI

其实Erlang里就在寻找，想有个能像SwaggerUI一样方便的API文档生成工具，于是就发现了[maru](https://github.com/falood/maru)和[maru_swagger](https://github.com/falood/maru_swagger)。先看效果，定义一个GET /user的接口如下：

~~~elixir
  namespace :user do
    desc "get user info by id"
    params do
      requires :id, type: String, desc: "user id"
      optional :age, type: Integer, values: 18..65, desc: "age [18-65]"
      optional :sex, type: Atom, values: [:male, :female], default: :male, desc: "male, female"
    end
    get do
      %{ uid: params[:id], age: params[:age], sex: params[:sex] }
    end
  end
~~~

使用 maru_swagger 自动生成的API文档如下：

![swagger_output]({{ site.url }}/images/201507/swagger_output.png)

之后就可以用 [SwaggerUI](http://petstore.swagger.io/) 来查看文档：

![swagger_ui]({{ site.url }}/images/201507/swagger_ui.png)

## maru

maru是`Elixir copy of grape`，基于plug但多了些参数检查等功能。不过当前版本tag还是v0.3.0，不说和网上v0.2.x的例子不同，有些地方master(v0.3.1-dev)的写法都不一样。所以要会用maru，最快的办法是看test和源码。。。

首先rescue_from是master新增的内容，如果你和我一样像Getting Started Guide里的deps.get了v0.3.0的版本，那肯定是报错的。另外相比v0.3.0，返回值默认为json，而要返回html或者text则需要自己指定ContentType：

~~~elixir
defmodule ZServer.Router.Homepage do
  use Maru.Router

  resources do
    get do
      content_type "text/html"
      "<h1>It Works!</h1>"
    end
  end
end
~~~

这样 GET / 就会返回text/html而不是json。

而像上一节介绍的API参数获取方法，get里就是直接返回maps的结构，最终到这里进行编码：

~~~elixir
# https://github.com/falood/maru/blob/master/lib/maru/response.ex
defimpl Maru.Response, for: Any do
  def content_type(_) do
    "application/json"
  end

  def resp_body(resp) do
    resp |> Poison.encode!
  end
end
~~~

从test里也可以看出，如果是text也会自己判断，或者自定义response：

~~~elixir
  test "string response" do
    resp = "ok"
    assert "text/plain" == Maru.Response.content_type(resp)
    assert "ok" == Maru.Response.resp_body(resp)
  end

  test "any response" do
    resp = :atom
    assert "application/json" == Maru.Response.content_type(resp)
    assert ~s["atom"] == Maru.Response.resp_body(resp)
  end

  test "custom response" do
    defmodule User do
      defstruct name: nil, age: nil, password: nil
      def hehe, do: "hehe"
    end

    defimpl Maru.Response, for: User do
      def content_type(_) do
        "application/json"
      end

      def resp_body(user) do
        %{name: user.name} |> Poison.encode!
      end
    end

    resp = struct User, %{name: "falood", age: 25, password: "123456"}
    assert "application/json" == Maru.Response.content_type(resp)
    assert ~s[{"name":"falood"}] == Maru.Response.resp_body(resp)
  end
~~~

最后就是middleware的用法，比如为每个请求增加个跨站防御的Header：

~~~elixir
defmodule XSS.Protection do
  # https://www.owasp.org/index.php/List_of_useful_HTTP_headers
  use Maru.Middleware
  import Plug.Conn

  def call(conn, _opts) do
    conn
    |> put_resp_header("X-Frame-Options", "deny")
    |> put_resp_header("X-XSS-Protection", "1; mode=block")
    |> put_resp_header("X-Content-Type-Options", "nosniff")
  end
end
~~~

这样只要在API的入口处加上`plug XSS.Protection`，就会把这三个header加入到返回的conn中。

## maru_swagger

配置 maru_swagger 并不复杂，首先在mix.exs加上deps：

`{ :maru_swagger, git: "https://github.com/upbit/maru_swagger.git", branch: "master" }`

接着在router里加上`plug MaruSwagger, at: "/swagger"`就可以工作了。

需要注意的是，因为v0.1.0的maru_swagger还指向v0.3.0的maru，获取deps时会冲突，于是我自己fork了并改了下依赖，将git换成了自己的repo。

---------------------------

ps: 上面的完整代码，可以在这里找到[GitHub:ZServer](https://github.com/upbit/zserver/tree/8c4253309ebaec61f3585696d16f850d9a9ccf73)
