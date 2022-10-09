---
layout: post
title: Pixiv Public-API (OAuth)分析 - pixivpy新版本放出
description: "Pixiv Public-API (OAuth) analysis"
category: opensource
comments: true
share: true
---

最近Pixiv将图片存储升级，已经无法通过从SAPI返回的mobileURL里截断 /mobile/***.*** 来获取图片原始地址了。而早在几个版本前，SAPI的login.php也已经无法用于获取PHPSESSID：

```
http://spapi.pixiv.net/iphone/login.php?mode={}&pixiv_id={}&pass={}&skip={}
```

好在十一期间闲下来了，有足够时间去分析Pixiv的新客户端和登录模拟。事实证明最终还是找到了模拟OAuth认证的方法，并且不像Pixitti那样使用js脚本，而是和官方客户端一样走更为安全的方法进行登录模拟。

新版的API已经更新到 [PixivPy](https://github.com/upbit/pixivpy) 和 [PixivAPI_iOS](https://github.com/upbit/PixivAPI_iOS)，发现任何问题或建议请尽管提issues吧 :P

## 分析过程

用Fiddler2分析了iOS客户端的请求，发现SAPI的 ranking.php 并未返回原始的图片地址；而点击放大镜访问的URL，显然也不是从mobileURL里推断出来的。例如下面是两组对比数据：

```
px_480mw: http://i2.pixiv.net/c/480x960/img-master/img/2014/10/07/00/08/26/46401177_480mw.jpg
large: http://i2.pixiv.net/img-original/img/2014/10/07/00/08/26/46401177_p0.jpg

px_480mw: http://i2.pixiv.net/c/480x960/img-master/img/2014/10/07/00/08/21/46401171_480mw.jpg
large: http://i2.pixiv.net/img-original/img/2014/10/07/00/08/21/46401171_p0.png
```

'px_480mw'是mobileURL的内容，'large'则是原始图片地址(需要加Referer来访问)。除了img-original/外，后面的后缀也不尽相同(例如第二个原图是png)。

出于好奇，再次祭出Fiddler2尝试抓取完整的iOS客户端访问，发现新版客户端会用HTTPS访问一个 public-api.secure.pixiv.net 的服务器。通常HTTPS访问，客户端会检查证书的有效性，所以经过Fiddler2解密的HTTPS请求Pixiv客户端会不认。好在还有 [iOS SSL Kill Switch](https://github.com/iSECPartners/ios-ssl-kill-switch) 这个Cydia插件，可以关闭iOS上的SSL证书检查。

### Public-API

分析解密后的请求发现，客户端除了访问SAPI的ranking.php获取日榜外，查看图片时还会访问如下两个Public-API的地址：

```
https://public-api.secure.pixiv.net/v1/works/{illust_id}.json
https://public-api.secure.pixiv.net/v1/users/{author_id}.json
```

其中 works/{id}.json 返回的json数据中，就有想要的图片原始地址。works/46605041.json 返回如下：

```json
{
    count = 1;
    response =     (
                {
            "age_limit" = "all-age";
            "book_style" = none;
            caption = "\U307b\U306e\U304b\U3061\U3083\U3093\U3000\U58c1\U7d19\U30b5\U30a4\U30ba\Uff081024\U00d7768\Uff09\U3067\U3059\Uff01
\n\U30a2\U30a4\U30b3\U30f3\U3082\U3069\Uff5e\U305e\Uff5e";
            "created_time" = "2014-10-18 02:31:58";
            "favorite_id" = 0;
            height = 768;
            id = 46605041;
            "image_urls" =             {
                large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p0.jpg";
                medium = "http://i2.pixiv.net/c/600x600/img-master/img/2014/10/18/02/31/58/46605041_p0_master1200.jpg";
                "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_128x128.jpg";
                "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_480mw.jpg";
                small = "http://i2.pixiv.net/c/150x150/img-master/img/2014/10/18/02/31/58/46605041_p0_master1200.jpg";
            };
            "is_liked" = 0;
            "is_manga" = 1;
            metadata =             {
                pages =                 (
                                        {
                        "image_urls" =                         {
                            large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p0.jpg";
                            medium = "http://i2.pixiv.net/c/1200x1200/img-master/img/2014/10/18/02/31/58/46605041_p0_master1200.jpg";
                            "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_p0_square1200.jpg";
                            "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_p0_master1200.jpg";
                        };
                    },
                                        {
                        "image_urls" =                         {
                            large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p1.jpg";
                            medium = "http://i2.pixiv.net/c/1200x1200/img-master/img/2014/10/18/02/31/58/46605041_p1_master1200.jpg";
                            "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_p1_square1200.jpg";
                            "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_p1_master1200.jpg";
                        };
                    },
                                        {
                        "image_urls" =                         {
                            large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p2.jpg";
                            medium = "http://i2.pixiv.net/c/1200x1200/img-master/img/2014/10/18/02/31/58/46605041_p2_master1200.jpg";
                            "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_p2_square1200.jpg";
                            "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_p2_master1200.jpg";
                        };
                    },
                                        {
                        "image_urls" =                         {
                            large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p3.jpg";
                            medium = "http://i2.pixiv.net/c/1200x1200/img-master/img/2014/10/18/02/31/58/46605041_p3_master1200.jpg";
                            "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_p3_square1200.jpg";
                            "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_p3_master1200.jpg";
                        };
                    },
                                        {
                        "image_urls" =                         {
                            large = "http://i2.pixiv.net/img-original/img/2014/10/18/02/31/58/46605041_p4.jpg";
                            medium = "http://i2.pixiv.net/c/1200x1200/img-master/img/2014/10/18/02/31/58/46605041_p4_master1200.jpg";
                            "px_128x128" = "http://i2.pixiv.net/c/128x128/img-master/img/2014/10/18/02/31/58/46605041_p4_square1200.jpg";
                            "px_480mw" = "http://i2.pixiv.net/c/480x960/img-master/img/2014/10/18/02/31/58/46605041_p4_master1200.jpg";
                        };
                    }
                );
            };
            "page_count" = 5;
            publicity = 0;
            "reuploaded_time" = "2014-10-18 02:31:58";
            stats =             {
                "commented_count" = 29;
                "favorited_count" =                 {
                    private = 109;
                    public = 1418;
                };
                score = 10278;
                "scored_count" = 1034;
                "views_count" = 11673;
            };
            tags =             (
                "\U30e9\U30d6\U30e9\U30a4\U30d6!",
                "\U9ad8\U5742\U7a42\U4e43\U679c",
                "\U30e9\U30d6\U30e9\U30a4\U30d6!1000users\U5165\U308a",
                "\U30ed\U30ea"
            );
            title = "\U307b\U306e\U304b\U3061\U3083\U3093";
            tools =             (
                SAI
            );
            type = manga;
            user =             {
                account = "yukinko-02727";
                id = 10539782;
                "is_follower" = 0;
                "is_following" = 0;
                "is_friend" = 0;
                name = "\U7950\U559c\Uff08\U3086\U304d\Uff09";
                "profile_image_urls" =                 {
                    "px_50x50" = "http://i2.pixiv.net/img142/profile/yukinko-02727/8524995_s.jpg";
                };
            };
            width = 1024;
        }
    );
    status = success;
}
```

json.response[0].image_urls.large 的结果就是原始图片地址(多张图时信息则存在metadata的pages里)，并且其中还包含不少SAPI拿不到的信息。

下面是 users/1184799.json 的返回:

```json
{
    'status': u'success',
     'count': 1,
     'response': [
        {
            'profile': {
                'tags': None,
                 'introduction': u'白髪娘を愛しています(*\'▽\'*)\r\n好きなものを好きなように描いてます！\r\nｵﾘｼﾞﾅﾙ多め｡\r\n\r\n※ﾏｲﾋﾟｸ＆ご依頼はお受けしていません｡\r\n■I don\'t accept "maipiku" application other than a friend with having met.　I am really sorry＞＜\r\n\r\n絵はたまに気まぐれで消したりします｡\r\n画像加工＆配布は一切お断りしております｡\r\n\r\n※pixivのメールには反応していませんので、何かありましたらTwitterかHPの方へお願い致します。',
                 'gender': None,
                 'contacts': None,
                 'job': None,
                 'location': None,
                 'workspace': None,
                 'birth_date': None,
                 'homepage': None,
                 'blood_type': None
            },
             'account': u'luciahreat',
             'name': u'三嶋くろね',
             'email': None,
             'is_premium': None,
             'profile_image_urls': {
                'px_170x170': u'http://i2.pixiv.net/img38/profile/luciahreat/7271308.png',
                 'px_50x50': u'http://i2.pixiv.net/img38/profile/luciahreat/7271308_s.png'
            },
             'id': 1184799
        }
    ]
}
```

不过这个 Public-API 并非真的那么公开，需要提供OAuth鉴权(Authorization 中携带 Bearer Token)。分析了下发现iOS的BearerToken每次登录或者换用户名都会变化，而Android是固定的(8mMXXWT9iuwdJvsVIvQsFYDwuZpRCMePeyagSh30ZdU)，于是拿来主义直接用到Public-API的访问模拟上了。另外一个麻烦则是需要提供 Cookie: PHPSESSID=****，完整的请求头部类似下面这样：

```
Referer: http://spapi.pixiv.net/
User-Agent: PixivIOSApp/5.1.1
Content-Type: application/x-www-form-urlencoded       // 这个很关键，不然POST请求会返回失败

Authorization: Bearer 8mMXXWT9iuwdJvsVIvQsFYDwuZpRCMePeyagSh30ZdU     // Android Bearer
Cookie: PHPSESSID=500123_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            // Mobile PHPSESSID
```

### Login (auth/token)

再来说说如何模拟 auth/token 请求获得PHPSESSID。抓包发现iOS客户端每次打开，都会访问 oauth.secure.pixiv.net，其后的请求中都会携带 PHPSESSID：

```
POST https://oauth.secure.pixiv.net/auth/token
```

分析这个POST请求，其返回的 header['Set-Cookie'] 里就包含了PHPSESSID。

```
username: username,
password: password,
grant_type: password,
client_id: bYGKuGVw91e0NMfPGp44euvGt59s,                            // iOS Client ID
client_secret: HP3RmkgAmEGro0gn1x9ioawQE8WMfvLXDz3ZqxpK,            // iOS Client Secret
```

因为是OAuth，POST请求中必须包含client_id和client_secret，好在这个ID和Secret是不变的，直接模拟iOS客户端的即可。鉴权类型是密码方式，然后在Payload里加入username和password就ok了。

至此，顺利解决登录获取PHPSESSID，以及通过Public-API获取图片原始地址问题。唯一的遗憾是不知道iOS客户端是怎么算 Authorization: Bearer Token 的，以后再想办法找找规律吧。
