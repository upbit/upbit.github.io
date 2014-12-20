---
layout: post
title: 从 foldl/foldr 实现到递归的矩阵行列转换
description: "From foldl/foldr to Matrix Rows to Columns"
category: misc
comments: true
share: true
redirect_from: /misc/2014/12/17/from_fold_to_matrix%20_row_to_column.html
---

晚上面试遇到个很nice的面试官，聊的很尽兴。其中要求用类似 erlang 的语法，实现无循环的fold操作。当时可是想了好久才写出来的，看看还是Wiki的解释清晰明了。

### [left fold](http://zvon.org/other/haskell/Outputprelude/foldl_f.html)
![left fold]({{ site.url }}/images/201412/left_fold_transformation.png)

### [right fold](http://zvon.org/other/haskell/Outputprelude/foldr_f.html)
![right fold]({{ site.url }}/images/201412/right_fold_transformation.png)

fold对应Python的写法为：

~~~python
foldl: reduce(func, list, initval)
foldr: reduce(lambda x,y: func(y,x), reversed(list), initval)
~~~

另外临走还留了个问题：矩阵的行列转换

~~~python
输入：
[
  [1, 5, 7, 9],
  [2, 6],
  [3, 8],
  [4]
]
输出：
[
  [1, 2, 3, 4],
  [5, 6, 8],
  [7],
  [9]
]
~~~

要求同样是不能用循环，不能有随机内存访问。回来想了好久，算是有个基本思路了，将整个过程分为两个步骤：

1. 内函数将输入的数据拆分为 head,tail，例如 (1,[5,7,9])；
2. 外函数将拆好的head,tail依次加入返回数组中，迭代直到tail的返回数组为空；

上面矩阵的转置过程如下：

~~~python
# 第一次迭代，得到两个部分的输出
([1], [[5, 7, 9]])
([1, 2], [[5, 7, 9], [6]])
([1, 2, 3], [[5, 7, 9], [6], [8]])
([1, 2, 3, 4], [[5, 7, 9], [6], [8]])
# 输出中的 [[5, 7, 9], [6], [8]] 作为第二次的输入递归
([5], [[7, 9]])
([5, 6], [[7, 9]])
([5, 6, 8], [[7, 9]])
# 后面2轮
([7], [[9]])
([9])
~~~

大致流程就是这样，等有空了写出来验证下。

补充Python版本的代码：

~~~python
result = []
head = []
tail = []

def _split(init, ele):
  global head
  global tail

  # 将ele内容分割到 head,tail 内
  head.append(ele[0])
  if len(ele) > 1:
    tail.append(ele[1:])

def wrap_trans(data):
  if len(data) <= 0:
    return

  global head
  global tail

  # 分割data的每个元素
  head = []
  tail = []
  reduce(_split, data, 0)

  # head结果加入返回数组，tail继续递归处理
  result.append(head)
  wrap(tail)

def main():
  data = [
    [1, 5, 7, 9],
    [2, 6],
    [3, 8],
    [4]
  ]

  wrap_trans(data)
  print result        # [[1, 2, 3, 4], [5, 6, 8], [7], [9]]
~~~

写得有点绕，而且两层递归嵌套好别扭的感觉...
