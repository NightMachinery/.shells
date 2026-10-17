#import <Cocoa/Cocoa.h>
#import <UserNotifications/UserNotifications.h>
#import "AppDelegate.h"

@interface AppDelegate (Defaults)
+ (void)initializeUserDefaults;
@end

// A nib-free launcher shared by every branded notification app. Diagnostics use
// the public API; delivery retains terminal-notifier 2's optional DND flag.
int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSArray *args = NSProcessInfo.processInfo.arguments;
        BOOL diagnose = [args containsObject:@"--diagnose"];
        BOOL authorize = [args containsObject:@"--authorize"];
        NSApplication *app = [NSApplication sharedApplication];
        [app setActivationPolicy:NSApplicationActivationPolicyProhibited];
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (authorize ? 45 : 8) * NSEC_PER_SEC),
                       dispatch_get_main_queue(), ^{
            fprintf(stderr, "notification service timed out\n"); exit(4);
        });
        if (diagnose || authorize) {
            UNUserNotificationCenter *center = UNUserNotificationCenter.currentNotificationCenter;
            if (authorize) {
                [center requestAuthorizationWithOptions:UNAuthorizationOptionAlert | UNAuthorizationOptionSound
                                      completionHandler:^(BOOL granted, NSError *error) {
                    printf("notification permission: %s\n", granted ? "authorized" : "not authorized");
                    exit(granted && !error ? 0 : 3);
                }];
            } else {
                [center getNotificationSettingsWithCompletionHandler:^(UNNotificationSettings *settings) {
                    printf("notification authorization: %ld; alert style: %ld; alerts: %ld; sounds: %ld\n",
                           (long)settings.authorizationStatus, (long)settings.alertStyle,
                           (long)settings.alertSetting, (long)settings.soundSetting);
                    exit(settings.authorizationStatus == UNAuthorizationStatusAuthorized ? 0 : 3);
                }];
            }
            [app run];
        } else {
            [AppDelegate initializeUserDefaults];
            __attribute__((objc_precise_lifetime)) AppDelegate *delegate = [AppDelegate new];
            app.delegate = delegate;
            [app run];
        }
    }
    return 0;
}
