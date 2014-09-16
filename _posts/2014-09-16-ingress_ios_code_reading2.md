---
layout: post
title: Ingress-iOS ports代码阅读2 - Portal的绘制
description: "Ingress-iOS code reading 02"
category: opensource
comments: true
share: true
---

续[Ingress-iOS ports代码阅读1](http://blog.imaou.com/opensource/2014/09/04/ingress_ios_code_reading1.html)，研究了好久经济学原理，越是深入越无法理解需求弹性和需求价格公式斜率的关系了。实在无力继续啃曼昆的微观卷，回头继续看Ingress-iOS的代码。感兴趣的，除了第一部分的地图mask层绘制，其次就是绘制Portal的方法。

自己实验了MKAnnotation和MKOverlay两种方法，用MKAnnotation放置一个图片在指定点比较简单，不过MKAnnotation是无法支持缩放的。我想要的效果是可以缩放地图，在目标建筑小到一定程度后将隐藏或者绘制其他建筑(比如大范围看到的是一个城镇，放大进去可以看到各种店铺和景点)。MKOverlay有个很好的例子[HazardMap](https://developer.apple.com/library/ios/samplecode/HazardMap/Introduction/Intro.html)，不过里面的绘制和load部分看的云里雾里，还是直接从Ingress代码里找出Portal的处理方法把。

Portal的初始化是在 ScannerViewController 里进行的。不过在看这个之前，需要先了解下 Model - NSManagedObjects - Portal 的构成。Portal定义如下：

```objective-c
@interface Portal : NSManagedObject <MKAnnotation, MKOverlay>
@interface Portal (CoreDataGeneratedAccessors)
```

可以看到，Portal可以作为MKAnnotation和MKOverlay加入到地图中，并且它也是CoreData的成员。这里只关注其作为MKAnnotation, MKOverlay的功能。

```objective-c
@implementation Portal

- (CLLocationCoordinate2D)coordinate {
	return CLLocationCoordinate2DMake(self.latitude, self.longitude);
}

- (CLLocationDistance)distanceFromCoordinate:(CLLocationCoordinate2D)coordinate {
	CLLocation *loc1 = [[CLLocation alloc] initWithLatitude:self.latitude longitude:self.longitude];
	CLLocation *loc2 = [[CLLocation alloc] initWithLatitude:coordinate.latitude longitude:coordinate.longitude];
	return [loc1 distanceFromLocation:loc2];
}

- (BOOL)isInPlayerRange {
	return [self distanceFromCoordinate:[LocationManager sharedInstance].playerLocation.coordinate] <= SCANNER_RANGE;
}

- (MKMapRect)boundingMapRect {
	MKMapPoint upperLeft = MKMapPointForCoordinate(self.coordinate);
	double pointsPerMeter = MKMapPointsPerMeterAtLatitude(self.coordinate.latitude);
	MKMapRect bounds = MKMapRectMake(upperLeft.x - (200*pointsPerMeter/2), upperLeft.y - (200*pointsPerMeter/2), 200*pointsPerMeter, 200*pointsPerMeter);
	return bounds;
}
```

从实现可以看到，Portal的 coordinate: 由创建时的 (latitude,longitude) 确定，而玩家离该Portal的距离，则用 distanceFromCoordinate: 计算得来。boundingMapRect: 暂时没看懂，放着继续看其他部分了。。。

回到 ScannerViewController 的 refresh: 函数，里面异步的获取Portal信息，并添加到mapView中：

```objective-c
#pragma mark - Data Refresh
- (void)refresh {
   NSManagedObjectContext *context  = [NSManagedObjectContext MR_contextForCurrentThread];

	[[API sharedInstance] getObjectsWithCompletionHandler:^{

		[context performBlock:^{

			__block int addedPortals = 0;
			NSArray *fetchedPortals = [Portal MR_findAllWithPredicate:[NSPredicate predicateWithFormat:@"completeInfo = YES"] inContext:context];
			for (Portal *portal in fetchedPortals) {
				//NSLog(@"adding portal to map: %@ (%f, %f)", portal.subtitle, portal.latitude, portal.longitude);
				if (portal.coordinate.latitude == 0 && portal.coordinate.longitude == 0) { continue; }
				if (MKMapRectContainsPoint(_mapView.visibleMapRect, MKMapPointForCoordinate(portal.coordinate))) {
					// 如果Portal在视野范围内
					addedPortals++;
					dispatch_async(dispatch_get_main_queue(), ^{
						// portal既作为MKAnnotation又作为MKOverlay添加到mapView中
						[_mapView addAnnotation:portal];
						[_mapView addOverlay:portal];
					});
				}
			}

		}];
	}];
}
```

接着在 ScannerViewController 的 mapView:viewForAnnotation: 里，创建或者重用已存在的MKAnnotationView：

```objective-c
- (MKAnnotationView *)mapView:(MKMapView *)mapView viewForAnnotation:(id <MKAnnotation>)annotation {
	if ([annotation isKindOfClass:[Portal class]]) {
		static NSString *AnnotationViewID = @"portalAnnotationView";
		
		MKAnnotationView *annotationView = /*(PortalAnnotationView *)*/[_mapView dequeueReusableAnnotationViewWithIdentifier:AnnotationViewID];
		if (annotationView == nil) {
			// 没有指定Identifier的MKAnnotationView则创建一个
			annotationView = [[MKAnnotationView alloc] initWithAnnotation:annotation reuseIdentifier:AnnotationViewID];
			annotationView.canShowCallout = NO;
		}
		
		annotationView.annotation = annotation;
		
		Portal *portal = (Portal *)annotation;
		annotationView.image = [Utilities iconForPortal:portal];		// 仅仅是为了触摸？这个图像好像没用

		return annotationView;
	
	} else if ([annotation isKindOfClass:[Item class]]) {
		...
	}
	return nil;
}

- (void)mapView:(MKMapView *)mapView didSelectAnnotationView:(MKAnnotationView *)view {
	[_mapView deselectAnnotation:view.annotation animated:NO];

	if ([view.annotation isKindOfClass:[Portal class]]) {
		currentPortal = (Portal *)view.annotation;
		if (self.virusToUse) {
			if ([currentPortal distanceFromCoordinate:_mapView.centerCoordinate] <= SCANNER_RANGE) {
				if ([[NSUserDefaults standardUserDefaults] boolForKey:DeviceSoundToggleEffects]) {
					[[SoundManager sharedManager] playSound:@"Sound/sfx_ui_success.aif"];
				}

				UIActionSheet *actionSheet = [[UIActionSheet alloc] initWithTitle:@"Confirm Deployment" delegate:self cancelButtonTitle:@"Cancel" destructiveButtonTitle:nil otherButtonTitles:@"Confirm", nil];
				actionSheet.tag = 2;
				[actionSheet showInView:self.view.window];
			}
		} else {
			[self performSegueWithIdentifier:@"PortalDetailSegue" sender:self];
		}
	} else if ([view.annotation isKindOfClass:[Item class]]) {
		...
	}
}
```

mapView:didSelectAnnotationView: 里处理触摸事件，virusToUse暂时不明白是啥，不过看上去如果为YES，判断是否在范围内，然后弹出个确认对话框；如果为NO则直接 performSegueWithIdentifier: 到 PortalDetailSegue。

接着处理Portal的MKOverlayView。因为需要自定义绘制，在 Model - Overlay Views - PortalOverlayView.m 里进行了定制化的绘制：

```objective-c
@implementation PortalOverlayView

- (void)drawMapRect:(MKMapRect)mapRect zoomScale:(MKZoomScale)zoomScale inContext:(CGContextRef)context
{
	ScannerViewController *scannerVC = [AppDelegate instance].scannerViewController;
	Portal *portal = (Portal *)self.overlay;

	if (portal.completeInfo) {
		CGImageRef portalImage;

		if (portal.completeInfo && [portal.controllingTeam isEqualToString:@"ALIENS"]) {
			portalImage = scannerVC.alienPortalImage.CGImage;
		} else if (portal.completeInfo && [portal.controllingTeam isEqualToString:@"RESISTANCE"]) {
			portalImage = scannerVC.resistancePortalImage.CGImage;
		} else {
			portalImage = scannerVC.neutralPortalImage.CGImage;
		}

		MKMapPoint portalCenter = MKMapPointForCoordinate(self.overlay.coordinate);
		CGPoint portalCenterPoint = [self pointForMapPoint:portalCenter];

		// 看到这里还是没明白 boundingMapRect: 里的(200*pointsPerMeter/2)是怎么算的，Magic Number?
		CGFloat portalSize = 400;

		CGContextDrawImage(context, CGRectMake(portalCenterPoint.x-portalSize/2, portalCenterPoint.y-portalSize/2, portalSize, portalSize), portalImage);
	}

}
```

ScannerViewController 的 mapView:viewForOverlay: 比较简单：

```objective-c
- (MKOverlayView *)mapView:(MKMapView *)mapView viewForOverlay:(id<MKOverlay>)overlay
{
	if ([overlay isKindOfClass:[Portal class]]) {
		// 交给 PortalOverlayView 处理
		PortalOverlayView *overlayView = [[PortalOverlayView alloc] initWithOverlay:overlay];
		return overlayView;
	} else if ([overlay isKindOfClass:[MKPolyline class]]) {
		MKPolyline *polyline = (MKPolyline *)overlay;
		MKPolylineView *polylineView = [[MKPolylineView alloc] initWithPolyline:polyline];
		polylineView.strokeColor = [Utilities colorForFaction:polyline.portalLink.controllingTeam];
		polylineView.lineWidth = 1;
		return polylineView;
	} else if ([overlay isKindOfClass:[MKPolygon class]]) {
		MKPolygon *polygon = (MKPolygon *)overlay;
		MKPolygonView *polygonView = [[MKPolygonView alloc] initWithPolygon:polygon];
		polygonView.fillColor = [Utilities colorForFaction:polygon.controlField.controllingTeam];
		polygonView.alpha = .1;
		return polygonView;
	} else if ([overlay isKindOfClass:[MKCircle class]]) {
		MKCircle *circle = (MKCircle *)overlay;
		if (circle.deployedResonator) {
			DeployedResonatorView *circleView = [[DeployedResonatorView alloc] initWithCircle:circle];
			circleView.fillColor = [Utilities colorForLevel:circle.deployedResonator.level];
			return circleView;
        }
	} else if ([overlay isKindOfClass:[XMOverlay class]]) {
        XMOverlayView *xmOverlayView = [[XMOverlayView alloc] initWithOverlay:overlay];
        _xmOverlayView = xmOverlayView;
        return xmOverlayView;
    }
	return nil;
}
```

写了一遍才体会到，原来MKAnnotation仅仅是用来响应触摸的([Utilities iconForPortal:portal]返回的是一张30x30的空UIImage，这里只是用来占位)，而MKOverlay才是绘制实际的图片(另外要注意CGContextDrawImage绘制的是用户空间，是倒着的图片)。在Apple总部的位置，画了个商店的图标：

![Merchant Overlay Draw]({{ site.url }}/images/ingress-ios_reading2_img1.png)

触摸该MKAnnotation显示位置和class的名字。
