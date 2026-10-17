#import <Cocoa/Cocoa.h>

// Original vector artwork rendered without opening a window.
int main(int argc, const char *argv[]) {
    @autoreleasepool {
        BOOL mail = [@(argv[1]) isEqualToString:@"mail"];
        NSBitmapImageRep *bitmap = [[NSBitmapImageRep alloc]
            initWithBitmapDataPlanes:NULL pixelsWide:1024 pixelsHigh:1024
            bitsPerSample:8 samplesPerPixel:4 hasAlpha:YES isPlanar:NO
            colorSpaceName:NSCalibratedRGBColorSpace bytesPerRow:0 bitsPerPixel:0];
        [NSGraphicsContext saveGraphicsState];
        NSGraphicsContext.currentContext = [NSGraphicsContext graphicsContextWithBitmapImageRep:bitmap];
        NSColor *color = mail ? [NSColor colorWithCalibratedRed:0.12 green:0.43 blue:0.87 alpha:1]
                             : [NSColor colorWithCalibratedRed:0.42 green:0.25 blue:0.72 alpha:1];
        [color setFill];
        [[NSBezierPath bezierPathWithRoundedRect:NSMakeRect(48,48,928,928) xRadius:210 yRadius:210] fill];
        [NSColor.whiteColor setStroke];
        NSBezierPath *lines = [NSBezierPath bezierPath];
        lines.lineWidth = 42;
        lines.lineCapStyle = NSLineCapStyleRound;
        lines.lineJoinStyle = NSLineJoinStyleRound;
        if (mail) {
            NSBezierPath *envelope = [NSBezierPath bezierPathWithRoundedRect:NSMakeRect(224,310,576,404) xRadius:40 yRadius:40];
            envelope.lineWidth = 42; [envelope stroke];
            [lines moveToPoint:NSMakePoint(244,680)];
            [lines lineToPoint:NSMakePoint(512,470)];
            [lines lineToPoint:NSMakePoint(780,680)];
        } else {
            [lines moveToPoint:NSMakePoint(270,650)];
            [lines lineToPoint:NSMakePoint(430,512)];
            [lines lineToPoint:NSMakePoint(270,374)];
            [lines moveToPoint:NSMakePoint(520,374)];
            [lines lineToPoint:NSMakePoint(730,374)];
            [NSColor.whiteColor setFill];
            [[NSBezierPath bezierPathWithOvalInRect:NSMakeRect(650,600,100,100)] fill];
        }
        [lines stroke];
        [NSGraphicsContext restoreGraphicsState];
        NSData *png = [bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}];
        return [png writeToFile:@(argv[2]) atomically:YES] ? 0 : 1;
    }
}
