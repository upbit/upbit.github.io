---
layout: post
title: 在CentOS上安装 OpenStreetMap Nominatim v2.4
description: "Install OpenStreetMap Nominatim v2.4 on CentOS"
category: opensource
comments: true
share: true
---

Nominatim是一个OpenStreetMap的Server，[geocoder](https://geocoder.readthedocs.org/en/latest/providers/OpenStreetMap/)又提供了比较好的封装，于是打算搭一个本地用。

不过搭建 Nominatim 耗费了不少时间，这里记录下以免以后继续掉坑。首先最主要的参考文档，自然是官方的[Nominatim/Installation/CentOS](http://wiki.openstreetmap.org/wiki/Nominatim/Installation/CentOS)。不过腾讯云的yum源里，boost和PostgreSQL都太旧无法正确运行。

## 安装 boost v1.4.8

首先按照教程安装依赖，装完后提示找不到148以上的boost。

参考[Boost C++ library RPM packages for CentOS 6](http://vicendominguez.blogspot.jp/2014/04/boost-c-library-rpm-packages-for-centos.html)里的方法，先添加源然后就可以看到`boost148-devel`：

~~~bash
wget http://repo.enetres.net/enetres.repo -O /etc/yum.repos.d/enetres.repo
yum list boost*
~~~

安装后再运行Nominatim的configure，就不会提示找不到libboost了。

## 安装 PostgreSQL 9.2

yum里的PostgreSQL默认版本才8，而Nominatim需要9.2的PostgreSQL。参考[How To Install a PostgreSQL 9.4 Database](https://yashi.com/blog/how-install-postgresql-94-database)，新增官方源中的`pgdg-centos92-9.2-6`来安装：

~~~bash
yum install http://yum.postgresql.org/9.2/redhat/rhel-6-x86_64/pgdg-centos92-9.2-6.noarch.rpm
yum install postgresql92-server postgresql92-contrib
~~~

接着就可以初始化db了：

~~~bash
service postgresql-9.2 initdb
chkconfig postgresql-9.2 on
service postgresql-9.2 restart
~~~

另外需要export以下安装路径`export PATH="/usr/pgsql-9.2/bin/:$PATH"`，这样Nominatim就不会说找不到pg_config。

## 配置Nominatim

下载2.4.0的压缩包解压后，按教程配置：

~~~bash
cd Nominatim-2.4.0
./configure
make
~~~

如果找不到pg_config，记得检查下PATH是否添加正确。然后就可以顺利make完成了。

接着写local.php：

~~~php
<?php
   // General settings
   @define('CONST_Database_Web_User', 'apache');
   // Paths
   @define('CONST_Postgresql_Version', '9.2');
   @define('CONST_Postgis_Version', '2.0');
   // Website settings
   @define('CONST_Website_BaseURL', 'http://127.0.0.1/nominatim/');
?>
~~~

使用postgres用户创建用户：

~~~bash
sudo -u postgres createuser -s nominatim
sudo -u postgres createuser apache
~~~

## 导入数据文件

按照教程导入pdf数据，然后顺利的失败了：

~~~bash
# sudo -u postgres ./utils/setup.php --osm-file data/china-latest.osm.pbf --all --osm2pgsql-cache 24000 2>&1 | tee setup.log
WARNING: resetting cache memory to 8258
Create DB
createdb: database creation failed: ERROR:  new encoding (UTF8) is incompatible with the encoding of the template database (SQL_ASCII)
HINT:  Use the same encoding as in the template database, or use template0 as template.
ERROR: Error executing external command: createdb -E UTF-8 -p 5432 nominatim
Error executing external command: createdb -E UTF-8 -p 5432 nominatim
~~~

字符集编码问题，像提示一样增加`--template=template0`到createdb命令中：

`104:  passthruCheckReturn('createdb --template=template0 -E UTF-8 -p '.$aDSNInfo['port'].' '.$aDSNInfo    ['database']);`

接着继续导入，遇到另外的问题：

~~~bash
ERROR:  could not open extension control file "/usr/pgsql-9.2/share/extension/postgis.control": No such file or directory
~~~

缺少postgis2，`yum install postgis2_92`安装之后，删掉原来的db继续`sudo -u postgres dropdb nominatim`，然后遇到这个错误：

~~~bash
ERROR:  could not load library "/usr/pgsql-9.2/lib/postgis-2.1.so": /usr/pgsql-9.2/lib/postgis-2.1.so: undefined symbol: pj_get_spheroid_defn
~~~

按照往上的说法，是proj的版本低于4.8。`proj --version`一看果然，于是升级之`yum upgrade proj`。有一个没见过的：

~~~bash
ERROR:  function st_lineinterpolatepoint(geometry, double precision) already exists in schema "public"
ERROR: PostGIS version is not correct.  Expected 2.0 found 2.1
~~~

于是降级PostGIS 2.0，结果postgis.control又找不到了。最后无意中发现local.php里`@define('CONST_Postgis_Version', '2.0');`，看到这个已哭昏在厕所里

~~~php
<?php
   // General settings
   @define('CONST_Database_Web_User', 'apache');
   // Paths
   @define('CONST_Postgresql_Version', '9.2');
   @define('CONST_Postgis_Version', '2.1');
   // Website settings
   @define('CONST_Website_BaseURL', 'http://127.0.0.1/nominatim/');
?>
~~~

改成2.1继续。想来之前PostgreSQL 9.4也是这个原因。。。 升到2.1.7再次运行，终于开始导入了。不过跑一半提示：

~~~bash
/data/Nominatim-2.4.0/osm2pgsql/osm2pgsql: error while loading shared libraries: libgeos-3.3.2.so: cannot open shared object file: No such file or directory
~~~

再次祭出`yum list geos* --showduplicates`查看，发现装了3.4.2（你就不能向上找个新版么..）

降级无果，于是暴力链接之`ln -s /usr/lib64/libgeos-3.4.2.so /usr/lib64/libgeos-3.3.2.so`，终于出现进度条了。不过跑了一会还是出错了：

~~~bash
/data/Nominatim-2.4.0/osm2pgsql/osm2pgsql: symbol lookup error: /data/Nominatim-2.4.0/osm2pgsql/osm2pgsql: undefined symbol: _ZN4geos4geom10CoordinateD1Ev
~~~

于是寻求其他解决办法。在postgresql的yum源里发现个3.3.8的：

~~~bash
wget http://yum.postgresql.org/9.0/redhat/rhel-6-x86_64/geos-3.3.8-1.rhel6.x86_64.rpm
rpm2cpio geos-3.3.8-1.rhel6.x86_64.rpm | cpio -div
~~~

将解压出的libgeos-3.3.8.so链接成libgeos-3.3.2.so：

~~~bash
cd usr/lib64/
mv libgeos-3.3.8.so /usr/lib64/
mv libgeos_c.so.1.7.8 /usr/lib64/
ln -s /usr/lib64/libgeos-3.3.8.so /usr/lib64/libgeos-3.3.2.so
~~~

再次跑导入就会愉快的看到进度（记得留足够的磁盘空间，我第一次导入就因为根目录空间不足而失败）。

剩下的就是按照教程配置website了，启动httpd就能看到Nominatim的界面。
