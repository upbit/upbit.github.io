---
layout: post
title: 跟随HackerNews一起探索React Native：HackerNews-React-Native
description: "Follow HackerNews exploring React Native"
category: opensource
comments: true
share: true
---

这两天沉迷于[React Native](https://github.com/facebook/react-native)，不过[ES6的语法](https://github.com/lukehoban/es6features)严重不熟导致走了不少弯路。更何况官方例子缺乏连贯性，比如Navigator传值以及绑定子对象的点击事件这样简单的操作，都没有文档可查。

## react-native的更新

除了语法问题，react-native偶尔还会有些奇怪的现象。比如require一张带有子目录的本地图片时给你红脸(RED SCREEN)，navigator.push后Command + R导致crash...

遇上这种情况请务必使用如下命令，定期更新react-native：

~~~
npm install react-native -g
~~~

react-native更新相当频繁，昨晚还是v0.3.1的，今天中午就更新到v0.3.4了~

## [HackerNews-React-Native](https://github.com/iSimar/HackerNews-React-Native)

然后说说今天的主角[HackerNews-React-Native](https://github.com/iSimar/HackerNews-React-Native)。官方的UIExplorer虽然看上去包罗万象，但遇到些"简单"的写法问题就没辙了，这里推荐看iSimar的HackerNews这个例子。里面的代码组织结构比较清晰，并且告诉了如何在ListView里自定义Cell并绑定事件：

~~~javascript
// App/Views/Posts/index.js
  renderPostCell: function(post){
    return(
      <PostCell
        onSelect={() => this.selectPost(post)}
        post={post}/>
    );
  },

// App/Views/Posts/Elements/PostCell/index.js
var PostCell = React.createClass({
  render: function() {
    return (
      <TouchableHighlight onPress={this.props.onSelect}>
      ...
      </TouchableHighlight>
    );
  }
});
~~~

这样每个用到PostCell的地方都可以直接传递post进行render，并且根据View响应不同的selectPost()事件。

另外一个是我试了一天的问题，navigator.push时给后面的component传值：

~~~javascript
// App/Views/Posts/index.js
  selectPost: function(post){
    this.props.navigator.push({
      title: "Top Story #"+post.count.substring(0, post.count.length - 1),
      component: PostView,
      passProps: {post_id: UtilFuncs.getId(post.comments.href),
                  post_title: post.title.text,
                  post_by: post.username.text.split(" ")[0],
                  post_comments_count: post.comments.text.split(" ")[0],
                  post_points_count: post.points.split(" ")[0],}
    });
  },
~~~

其实只是写法问题，passProps传递参数后，对应class里用this.props.xxx来获取。

最后是`App/Utils/functions.js`里的技巧，可以通过这种方式导出多个函数。这样无论CSS还是一些网络API，都可以挪到View之外来写了，赞！

不多说了，自己顺着[index.ios.js](https://github.com/iSimar/HackerNews-React-Native/blob/master/index.ios.js)去看吧
