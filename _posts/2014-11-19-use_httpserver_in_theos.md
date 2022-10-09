---
layout: post
title: 在Tweak中嵌入CocoaHTTPServer, 并访问任意目录文件
description: "Retrieve Wifi password form Keychain by code"
category: theos
comments: true
share: true
---

网上没有过多关于Theos中集成CocoaPods库的简单方案，只好学[iSpy](https://github.com/BishopFox/iSpy)里自己动手写了。最终顺利将CocoaHTTPServer集成到Tweak中，实现%hook UIApplication并随目标程序监听指定端口，另外记录下CocoaHTTPServer访问沙盒外文件的方法。

## 编译libCocoaHTTPServer.a

先下载[CocoaHTTPServer的源码](https://github.com/robbiehanson/CocoaHTTPServer)，并将需要的.h和.m放到同一个目录，如 CocoaHTTPServer/ 下。注意，子目录下的文件也需要放到一个目录里

接着在目录中新建一个build.sh，输入如下内容：

~~~sh
#!/bin/sh

OBJ="./objs"
LIBNAME="libCocoaHTTPServer.a"
ARCHS="-arch armv7 -arch armv7s -arch arm64"

SDK=$(ls -1d /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iP* | tail -n1)
echo $SDK

# remove object files to build nice n clean
echo '[+] Removing old object files ...'
rm -Rf ${OBJ}
mkdir ${OBJ}

# compile the Objective-C stuff
echo '[+] Compiling Objective-C files ...'
CLANG_CFLAGS="-fobjc-arc -I /usr/include/libxml2 -Wno-format -Wno-mismatched-parameter-types -Wno-unused-function -Wno-deprecated-declarations -Wno-enum-conversion"
cd ${OBJ}
clang -c ../*.m ${ARCHS} -isysroot $SDK ${CLANG_CFLAGS}

# See Makefile.* in the parent directory.
echo "[+] Creating ${LIBNAME} archive"
ar -rs ${LIBNAME} *.o >/dev/null 2>&1

mv -f ${LIBNAME} ../../
echo '[+] done.'
~~~

此脚本用于查找同级目录下的所有.m文件，并编译链接为libCocoaHTTPServer.a，移动到上一层目录中。注意，多arch时务必添加 ar -s，为这个问题搞了好久。

## 封装HTTPServer

封装一个单例模式的 CommandServer，方便在tweak中使用

### CommandServer.h

~~~objective-c
#ifndef __COMMAND_SERVER_H__
#define __COMMAND_SERVER_H__

#import <Foundation/Foundation.h>
#import "CocoaHTTPServer/HTTPServer.h"

@interface CommandServer : NSObject {
@public
	HTTPServer *httpServer;
}

+ (CommandServer *)sharedInstance;
- (void)initialize;
- (BOOL)startServer;
- (void)stopServer;

@end

#endif
~~~

### CommandServer.m

~~~objective-c
#import "CommandServer/CommandServer.xm"
#import "CommandServer.h"
#import "ZLogDefines.h"

#import "RootHTTPConnection.h"

@implementation CommandServer

+ (CommandServer *)sharedInstance {
	static CommandServer *sharedInstance = nil;
	static dispatch_once_t once;
	dispatch_once(&once, ^{
		sharedInstance = [[self alloc] init];
	});
	return sharedInstance;
}

- (void)initialize {
	static dispatch_once_t once;

	dispatch_once(&once, ^{
		ZLogNotice("Initializing CommandServer on: %s", TO_CSTR([[NSBundle mainBundle] bundleIdentifier]));

		httpServer = [[HTTPServer alloc] init];
		[httpServer setType:@"_http._tcp."];
		[httpServer setConnectionClass:[RootHTTPConnection class]];


		[httpServer setPort:8080];
		[httpServer setDocumentRoot:@"/var/www/sites"];

		NSError *error = nil;
		if (![httpServer start:&error]) {
			ZLogError("Error starting HTTP Server: %s", TO_CSTR(error));
			return;
		}

		ZLogWarn("Initialized CommandServer(%s), port %hu", TO_CSTR(httpServer.documentRoot), httpServer.port);

	});
}

- (BOOL)startServer {
	NSError *error;
	BOOL ret = [httpServer start:&error];
	if (!ret) {
		ZLogError("Error start HTTP server: %s", TO_CSTR(error));
	}
	return ret;
}

- (void)stopServer {
	[httpServer stop];
}

@end
~~~

在 initialize: 里初始化HTTPServer并监听8080端口，documentRoot设为沙盒外的路径。**这里如果直接运行会404**，需要先实现 RootHTTPConnection。

### RootHTTPConnection.h

~~~objective-c
#ifndef __ROOT_HTTP_CONNECTION_H__
#define __ROOT_HTTP_CONNECTION_H__

#import "CocoaHTTPServer/HTTPConnection.h"
@interface RootHTTPConnection : HTTPConnection {}
@end

#endif
~~~

### RootHTTPConnection.m

~~~objective-c
#import "RootHTTPConnection.h"
#import "RootStaticFileResponse.h"
#import "ZLogDefines.h"

@implementation RootHTTPConnection

// Handle static content in system path
- (NSObject<HTTPResponse> *)httpResponseForMethod:(NSString *)method URI:(NSString *)path {
	ZLogNotice("[HTTP %s] %s", TO_CSTR(method), TO_CSTR(path));

	NSString *filePath = [self filePathForURI:path allowDirectory:NO];
	BOOL isDir = NO;
	if (filePath && [[NSFileManager defaultManager] fileExistsAtPath:filePath isDirectory:&isDir] && !isDir) {
		return [[RootStaticFileResponse alloc] initWithFilePath:filePath forConnection:self];
	}

	return nil;		// 404
}
~~~

httpResponseForMethod: 里，使用NSFileManager访问外部文件。不过返回的HTTPResponse对象，这里需要封装下Headers

### RootStaticFileResponse.h

~~~objective-c
#ifndef __ROOT_STATIC_FILE_RESPONSE_H__
#define __ROOT_STATIC_FILE_RESPONSE_H__

#import "CocoaHTTPServer/HTTPFileResponse.h"
@interface RootStaticFileResponse : HTTPFileResponse {}
@end

#endif
~~~

### RootStaticFileResponse.m

~~~objective-c
#import "RootStaticFileResponse.h"

@implementation RootStaticFileResponse

- (NSDictionary *)httpHeaders {
	NSDictionary *contentTypes = @{
		@"html": @"text/html; charset=UTF-8",
		@"js": @"text/javascript",
		@"css": @"text/css",
		@"png": @"image/png",
		@"jpg": @"image/jpeg",
		@"jpeg": @"image/jpeg",
		@"ico": @"image/ico",
		@"gif": @"image/gif",
		@"svg": @"image/svg+xml",
		@"tff": @"application/x-font-ttf",
		@"eot": @"application/vnd.ms-fontobject",
		@"woff": @"application/x-font-woff",
		@"otf": @"application/x-font-otf",
	};

	/* If we don't know the mime-type we serve the file as binary data */
	NSString *contentType = [contentTypes valueForKey:[filePath pathExtension]];
	if ([contentType length] == 0) {
		contentType = @"application/octet-stream";
	}

	NSDictionary *headers = [[NSDictionary alloc] initWithObjectsAndKeys:
		contentType, @"Content-type",
	nil];

	return headers;
}

@end
~~~

更多详细代码，可以看[iSpy](https://github.com/BishopFox/iSpy)的HttpServer部分。

## Use in Tweak.xm

封装好 CommandServer，使用就很简单了：

~~~objective-c
#import "CommandServer/CommandServer.h"
#import "ZLogDefines.h"

%group InitHTTPServer

%hook UIApplication
- (id)init {
  self = %orig;
  if (self) {
    // init server when launched
    [[NSNotificationCenter defaultCenter] addObserverForName:UIApplicationDidFinishLaunchingNotification object:nil queue:[NSOperationQueue mainQueue] usingBlock:^(NSNotification *note) {
      ZLogNotice("HTTP Server init");
      [[CommandServer sharedInstance] initialize];
    }];

    // start/stop server for background
    [[NSNotificationCenter defaultCenter] addObserverForName:UIApplicationDidEnterBackgroundNotification object:nil queue:[NSOperationQueue mainQueue] usingBlock:^(NSNotification *note) {
      ZLogNotice("HTTP Server stop");
      [[CommandServer sharedInstance] stopServer];
    }];
    [[NSNotificationCenter defaultCenter] addObserverForName:UIApplicationWillEnterForegroundNotification object:nil queue:[NSOperationQueue mainQueue] usingBlock:^(NSNotification *note) {
      ZLogNotice("HTTP Server start");
      [[CommandServer sharedInstance] startServer];
    }];

  }

  return self;
}
%end

%end	// end of group InitHTTPServer


%ctor {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  ZLogPrint("Init");
  %init(InitHTTPServer);

  [pool drain];
}
~~~

%hook UIApplication的init，在其中添加3个事件：

1. UIApplicationDidFinishLaunchingNotification:  启动完成后，初始化 CommandServer
2. UIApplicationDidEnterBackgroundNotification:  进入后台时停止 CommandServer (后台运行好像要换GCDWebServer)
3. UIApplicationWillEnterForegroundNotification: 从后台唤醒时，重新启动 CommandServer

## Link with CocoaHTTPServer

Makefile如下：

~~~makefile
ARCHS := armv7 arm64

include theos/makefiles/common.mk

THEOS_PACKAGE_DIR=./debs

TWEAK_NAME = commandServer
testHttpServer_FILES = Tweak.xm $(wildcard CommandServer/*.m)
testHttpServer_CFLAGS += -I./
testHttpServer_LDFLAGS += -L./ -lCocoaHTTPServer -lxml2
testHttpServer_FRAMEWORKS = UIKit CFNetwork Security CoreGraphics

include $(THEOS_MAKE_PATH)/tweak.mk

lib::
	rm -f libCocoaHTTPServer.a
	cd CocoaHTTPServer/; ./build.sh

after-clean::
	rm -f $(THEOS_PACKAGE_DIR)/*.deb
~~~

链接CocoaHTTPServer需要libCocoaHTTPServer.a和libxml2.a，并且用到 UIKit CFNetwork Security CoreGraphics 四个FRAMEWORK。

之后make package，用 cinject 加载dylib后，就可以通过http://<IP>:8080访问到index.html了
