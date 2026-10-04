// Screen Decor: covers a screen with a fullscreen window showing that
// screen's desktop picture, in a native fullscreen Space of its own.
//
// Why: an app that activates with only a panel over another app's fullscreen
// Space (the kitty panel, hyper+z) hands the active display -- the lit menu
// bar, where Maccy opens -- to whichever display shows a desktop Space. A
// display showing a fullscreen Space is never chosen, so covering an empty
// desktop with this app keeps the active display where the panel is. See
// hammerspoon/docs/multi-monitor.md, "Which display macOS counts as active".
//
// Driven by core/screen-decor.lua:
//   launch:  ScreenDecor --cover <display UUID> [--cover <UUID>...]
//   running: the distributed notification "night.screen-decor.cover" with
//            the display UUID as its object
// Either covers that screen (creating its window and entering fullscreen
// the first time) and brings the window forward, which switches that
// display to the decor's Space. It answers with "night.screen-decor.covered"
// (object: the UUID) once the window is fullscreen and key.
//
// Build with build.sh next to this file.

#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>

static NSString *const kCover = @"night.screen-decor.cover";
static NSString *const kCovered = @"night.screen-decor.covered";

static NSString *uuidOfScreen(NSScreen *s) {
    CGDirectDisplayID d = [s.deviceDescription[@"NSScreenNumber"] unsignedIntValue];
    CFUUIDRef u = CGDisplayCreateUUIDFromDisplayID(d);
    if (!u) return nil;
    NSString *r = CFBridgingRelease(CFUUIDCreateString(NULL, u));
    CFRelease(u);
    return r;
}

static NSScreen *screenWithUUID(NSString *uuid) {
    for (NSScreen *s in NSScreen.screens) {
        if ([uuidOfScreen(s) caseInsensitiveCompare:uuid] == NSOrderedSame) return s;
    }
    return nil;
}

@interface DecorWindow : NSWindow
@property(copy) NSString *uuid;
@end

@implementation DecorWindow
- (BOOL)canBecomeKeyWindow { return YES; }
- (BOOL)canBecomeMainWindow { return YES; }
@end

@interface Decor : NSObject <NSApplicationDelegate, NSWindowDelegate>
@property(strong) NSMutableDictionary<NSString *, DecorWindow *> *windows;
@property(strong) NSMutableArray<NSString *> *initialUUIDs;
@end

@implementation Decor

- (void)applicationDidFinishLaunching:(NSNotification *)n {
    self.windows = [NSMutableDictionary dictionary];
    [[NSDistributedNotificationCenter defaultCenter] addObserver:self
                                                        selector:@selector(coverRequest:)
                                                            name:kCover
                                                          object:nil
                                              suspensionBehavior:NSNotificationSuspensionBehaviorDeliverImmediately];
    for (NSString *u in self.initialUUIDs) [self cover:u];
}

- (void)coverRequest:(NSNotification *)n {
    if ([n.object isKindOfClass:[NSString class]]) [self cover:n.object];
}

// The screen's desktop picture, scaled to fill and cropped, on black.
- (void)paint:(DecorWindow *)w screen:(NSScreen *)s {
    NSView *v = w.contentView;
    v.wantsLayer = YES;
    v.layer.backgroundColor = NSColor.blackColor.CGColor;
    v.layer.contentsGravity = kCAGravityResizeAspectFill;
    NSURL *url = [[NSWorkspace sharedWorkspace] desktopImageURLForScreen:s];
    NSImage *img = url ? [[NSImage alloc] initWithContentsOfURL:url] : nil;
    v.layer.contents = img;
}

- (void)cover:(NSString *)uuid {
    NSScreen *s = screenWithUUID(uuid);
    if (!s) {
        NSLog(@"screen-decor: no screen %@", uuid);
        return;
    }
    DecorWindow *w = self.windows[uuid];
    BOOL fresh = (w == nil);
    if (fresh) {
        w = [[DecorWindow alloc] initWithContentRect:s.frame
                                           styleMask:(NSWindowStyleMaskTitled | NSWindowStyleMaskResizable)
                                             backing:NSBackingStoreBuffered
                                               defer:NO];
        w.uuid = uuid;
        w.title = @"Screen Decor";
        w.releasedWhenClosed = NO;
        w.delegate = self;
        w.collectionBehavior = NSWindowCollectionBehaviorFullScreenPrimary;
        [w setFrame:s.frame display:NO];
        self.windows[uuid] = w;
    }
    [self paint:w screen:s];
    [NSApp activateIgnoringOtherApps:YES];
    [w makeKeyAndOrderFront:nil];
    if (fresh || !(w.styleMask & NSWindowStyleMaskFullScreen)) {
        [w toggleFullScreen:nil];
    } else {
        [self announce:w];
    }
}

- (void)announce:(DecorWindow *)w {
    [[NSDistributedNotificationCenter defaultCenter] postNotificationName:kCovered
                                                                   object:w.uuid
                                                                 userInfo:nil
                                                       deliverImmediately:YES];
}

- (void)windowDidEnterFullScreen:(NSNotification *)n {
    [self announce:(DecorWindow *)n.object];
}

// Closed or taken out of fullscreen by hand: forget it, so the next cover
// starts over rather than reusing a window that is no longer in its Space.
- (void)windowWillClose:(NSNotification *)n {
    DecorWindow *w = n.object;
    if (w.uuid) [self.windows removeObjectForKey:w.uuid];
}

- (void)windowDidExitFullScreen:(NSNotification *)n {
    DecorWindow *w = n.object;
    [w orderOut:nil];
    if (w.uuid) [self.windows removeObjectForKey:w.uuid];
}

@end

int main(int argc, const char **argv) {
    @autoreleasepool {
        NSApplication *app = [NSApplication sharedApplication];
        Decor *d = [Decor new];
        d.initialUUIDs = [NSMutableArray array];
        for (int i = 1; i + 1 < argc; i++) {
            if (!strcmp(argv[i], "--cover")) [d.initialUUIDs addObject:@(argv[i + 1])];
        }
        app.delegate = d;
        [app run];
    }
    return 0;
}
