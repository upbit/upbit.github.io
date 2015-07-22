---
layout: post
title: 用函数式语言计算区间的 merge 和 subtract - Elixir实现
description: "Use functional language writen merge/subtract intervals - Elixir Implement"
category: elixir
comments: true
share: true
---

最近遇到个计算两个人是否在某个位置相遇的问题，推演了几遍发现最终问题被归为两个：一是查询某个时间段是否与其他用户的时间段又交集；二是求两个人在相同geohash下的几个时间段相减的结果(subtract interval)，并且记录用户在该区域下的活动时长(merge interval)。

# 判断区间相交

第一个问题问是否相交，可以描述成：A用户在线的时间区间为(a1,a2)，有一组其他用户的在线时间[(t1,t2),...]，求问与A存在交集的用户有哪些。

用枚举法可以很容易得出，需要满足`(t1 < a1 < t2) or (t1 < a2 < t2) or (t1 > a1 and t2 < a2)`；简化下其实只用判断结束时间都大于a1，开始时间都小于a2即可`(t2 > a1) and (t1 < a2)`

# 求区间合并与相交的结果

这个问题复杂点，需要考虑的是输入的集合都可能是数组。

### 合并(merge)

求区间A+B的结果。例如区间`A = [(20,50)]`，区间`B = [(10,30),(40,60)]`，合并返回`[(10,60)]`

~~~elixir
  # A + B
  def merge(a, b) do
    sorted = Enum.sort_by(a ++ b, fn({st,_}) -> st end)
    {tmpst, tmped} = hd(sorted)
    merge_interval(tmpst, tmped, sorted, [])
  end

  defp merge_interval(tmpst, tmped, [], acc) do
    Enum.reverse([{tmpst,tmped} | acc])
  end
  defp merge_interval(tmpst, tmped, [{st,ed}|tail], acc) do
    case tmped > st do
      true -> merge_interval(tmpst, max(tmped, ed), tail, acc)
      false -> merge_interval(st, ed, tail, [{tmpst,tmped} | acc])
    end
  end
~~~

思路比较简单，对排序后的集合，依次判断上次的结束是否和当前区间的开始连续，连续就合并，否则记录`{tmpst,tmped}`到结果，然后将当前的区间作为下次合并的起止点。

### 相减(subtract)

求区间A-B的结果。例如区间`A = [(20,50)]`，区间`B = [(10,30),(32,38),(40,60)]`，A中与B相交的部分为`[(30,32),(38,40)]`

~~~elixir
  # A - B
  def subtract(a, b) do
    List.foldl(b, a, fn({st,ed}, acc) -> subtract_interval(acc, st, ed, []) end)
    |> Enum.filter(fn({st,ed}) -> st < ed end)
    |> Enum.sort_by(fn({st,_}) -> st end)
  end

  defp subtract_interval([], _, _, result) do
    result
  end
  defp subtract_interval([{tmpst,tmped}|tail], st, ed, result) do
    cond do
      st < tmpst -> subtract_interval(tail, st, ed, [{max(tmpst, ed), tmped}|result])
      ed > tmped -> subtract_interval(tail, st, ed, [{tmpst, min(tmped, st)}|result])
      true -> subtract_interval(tail, st, ed, [{tmpst, st}|[{ed, tmped}|result]])
    end
  end
~~~

集合相减想了比较久，感觉应该还可以再化简才对（想说以后再优化，但估计没性能问题就不会再碰这块吧→_→）

思路是将要减去的B区间，依次对A中的区域做减法。cond里先排除左右超过当前区间的情况，留下会将当前区间切成两段的第三种情况。第三种情况时切割为`{tmpst, st},{ed, tmped}`然后并入结果，将新得到的区间继续迭代直到减完所有的B区间。

最后附上个Python版的，比Elixir容易读一些：[interval_test.py](https://gist.github.com/upbit/fe8d09a3ddebf159ff61)
