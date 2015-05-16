---
layout: post
title: Pixiv老接口停用提醒，请迁移PixivPy的SAPI到新的PAPI
description: "NOTICE: Migrate PixivPy SAPI to new PAPI"
category: opensource
comments: true
share: true
---

周三晚用[RankingLog](https://github.com/upbit/Pixiv-RankingLog)爬P站图片，突然发现什么都抓不到了。因为最近要找房子这事一直拖到今天(都是借口...)，直到抓包才发现原来Pixiv已经抛弃了用了几年的SAPI。

不得不说废弃SAPI是个好事情，Public-API有更方便的json返回，并且带有分页和作品的各种详细信息。不过因为新的PAPI已经长得不太一样了，我这里还是以ranking/ranking_log为例，写一下如何从SAPI迁移到PAPI。

~~~python
# SAPI的写法，目前已经废弃
# rank_list = api.sapi.ranking("all", 'day', 1)
# for img in rank_list:
#   print img

# 使用新的PAPI代替上面的代码
rank_list = api.papi.ranking_all('daily', 1, 50)
ranking = rank_list.response[0]
for img in ranking.works:
	#print img.work
	print "[%s(id=%s)] %s" % (img.work.title, img.work.id, img.work.image_urls.px_480mw)
~~~

查询**过去排行**，可以这样增加一个date参数，此时返回的就是2015-05-01的历史周排行数据：
~~~python
rank_list = api.papi.ranking_all('weekly', 1, 50, date='2015-05-01')
~~~

ps: **请先升级PixivPy到最新版**，之后就可以用新的`papi.ranking_all`来取数据了

### 关于/v1/ranking/all

如果想确认PAPI接口的请求和返回数据，除了自己抓包还可以[看这里](https://github.com/upbit/pixivpy/wiki/sniffer)，里面列举了一些常用接口的json返回结构供参考。可以看到，新的ranking路径为`https://public-api.secure.pixiv.net/v1/ranking/all`，并且SAPI的ranking_log(过去排行榜)也合并到了这个接口：

~~~python
# 排行榜/过去排行榜
# mode: [daily, weekly, monthly, male, female, rookie, daily_r18, weekly_r18, ...]
# page: [1-n]
# date: '2015-04-01' (仅过去排行榜)
def ranking_all(self, mode='daily', page=1, per_page=50, date=None,
		image_sizes=['px_128x128', 'px_480mw', 'large'],
		profile_image_sizes=['px_170x170', 'px_50x50'],
		include_stats=True, include_sanity_level=True):
	self.api._require_auth()

	url = 'https://public-api.secure.pixiv.net/v1/ranking/all'
	headers = {
		'Authorization': 'Bearer %s' % self.api.access_token,
		'Cookie': 'PHPSESSID=%s' % self.api.session,
	}
	params = {
		'mode': mode,
		'page': page,
		'per_page': per_page,
		'include_stats': include_stats,
		'include_sanity_level': include_sanity_level,
		'image_sizes': ','.join(image_sizes),
		'profile_image_sizes': ','.join(profile_image_sizes),
	}
	if date:	# 过去排行榜
		params['date'] = date

	r = self.api._requests_call('GET', url, headers=headers, params=params)
	return self.parse_result(r)
~~~

新版接口多了一些可选字段，比如指定返回图片的大小，以及是否需要stats的计数信息。实际发送的请求如下：

~~~
GET https://public-api.secure.pixiv.net/v1/ranking/all?image_sizes=px_128x128%2Cpx_480mw%2Clarge&include_stats=true&page=1&profile_image_sizes=px_170x170%2Cpx_50x50&mode=daily&include_sanity_level=true&per_page=50 HTTP/1.1
Host: public-api.secure.pixiv.net
Referer: http://spapi.pixiv.net/
Authorization: Bearer WHDWCGnwWA2C8PRfQSdXJxjXp0G6ULRaRkkd6t5B6h8
Accept-Encoding: gzip, deflate
Accept: */*
Accept-Language: zh-cn
Connection: keep-alive
Proxy-Connection: keep-alive
User-Agent: PixivIOSApp/5.6.0
~~~

返回结果：

~~~json
{
    "status": "success",
    "response": [
        {
            "content": "all",
            "mode": "daily",
            "date": "2015-05-13",
            "works": [
                {
                    "rank": 51,
                    "previous_rank": 134,
                    "work": {
                        "id": 50332933,
                        "title": "何か用かしら",
                        "caption": null,
                        "tags": [
                            "やはり俺の青春ラブコメはまちがっている。",
                            "俺ガイル",
                            "雪ノ下雪乃",
                            "見せない構図",
                            "俺ガイル1000users入り"
                        ],
                        "tools": null,
                        "image_urls": {
                            "px_128x128": "http://i2.pixiv.net/c/128x128/img-master/img/2015/05/12/00/01/19/50332933_p0_square1200.jpg",
                            "px_480mw": "http://i2.pixiv.net/c/480x960/img-master/img/2015/05/12/00/01/19/50332933_p0_master1200.jpg",
                            "large": "http://i2.pixiv.net/img-original/img/2015/05/12/00/01/19/50332933_p0.png"
                        },
                        "width": 875,
                        "height": 775,
                        "stats": {
                            "scored_count": 230,
                            "score": 2283,
                            "views_count": 16325,
                            "favorited_count": {
                                "public": null,
                                "private": null
                            },
                            "commented_count": null
                        },
                        "publicity": 0,
                        "age_limit": "all-age",
                        "created_time": "2015-05-12 00:01:00",
                        "reuploaded_time": "2015-05-12 00:01:19",
                        "user": {
                            "id": 3145937,
                            "account": "37193719",
                            "name": "コ゛りぼて",
                            "is_following": null,
                            "is_follower": null,
                            "is_friend": null,
                            "is_premium": null,
                            "profile_image_urls": {
                                "px_170x170": "http://i3.pixiv.net/img77/profile/37193719/8134423.jpg",
                                "px_50x50": "http://i3.pixiv.net/img77/profile/37193719/8134423_s.jpg"
                            },
                            "stats": null,
                            "profile": null
                        },
                        "is_manga": null,
                        "is_liked": null,
                        "favorite_id": null,
                        "page_count": 1,
                        "book_style": "right_to_left",
                        "type": "illustration",
                        "metadata": null,
                        "content_type": null,
                        "sanity_level": "white"
                    }
                }
            ]
        }
    ],
    "count": 1,
    "pagination": {
        "previous": 1,
        "next": 3,
        "current": 2,
        "per_page": 50,
        "total": 500,
        "pages": 10
    }
}
~~~

可以看到，json结构的最外层多了个分页相关的pagination，再也不用像以前一样调SAPI翻到没数据才切日期了。response里是个数组，但大部分接口的count都是1，里面才是需要的ranking数据。

ranking数据中除了告诉这个返回的查询模式和参数，works是最终的榜单内容。其中每个illust都有rank和previous_rank来标记它的上升/下降情况，以及一个`image_urls`数组返回`image_sizes`指定的图片URL。当`include_stats=true`时，stats里会带回作品的评分、收藏计数等信息。最后还有个user，包含了该作品作者的基本信息，以及是否follow过`is_following`等标志。作品也带有`favorite_id`标记来判断是否已经收藏，这回再也不用一个个查询了..

相信后面几个版本Pixiv iOS会逐步抛弃SAPI（目前还有个拉取评论接口在用），如果你的App还有在用这些接口的，早日切换到更好用的Public-API吧。

ps: 看看日期果然好长时间没更新了，罪过... 不能因为工作忙就放弃Blog，熬夜把这篇给发了！
