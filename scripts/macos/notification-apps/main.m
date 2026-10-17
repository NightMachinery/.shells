#import <Cocoa/Cocoa.h>
#import <UserNotifications/UserNotifications.h>
#import "AppDelegate.h"

@interface AppDelegate (Defaults)
+ (void)initializeUserDefaults;
@end

static NSString *option(NSString *key) {
    id value = [[NSUserDefaults standardUserDefaults] objectForKey:key];
    if (![value isKindOfClass:NSString.class]) return nil;
    return [value hasPrefix:@"\\"] ? [value substringFromIndex:1] : value;
}

static void finish(int status, NSString *diagnostic) {
    // LaunchServices does not forward the child's status. A private receipt
    // lets bootstrap/check report the actual result from the Aqua process.
    NSString *receipt = option(@"status-file");
    if (receipt) {
        NSData *data = [NSJSONSerialization dataWithJSONObject:
                        @{@"exit_status": @(status), @"diagnostic": diagnostic ?: @""}
                         options:0 error:nil];
        if (![data writeToFile:receipt atomically:YES]) exit(5);
    }
    if (diagnostic.length) fprintf(status ? stderr : stdout, "%s\n", diagnostic.UTF8String);
    exit(status);
}

static void post(UNUserNotificationCenter *center) {
    [center getNotificationSettingsWithCompletionHandler:^(UNNotificationSettings *settings) {
        if (settings.authorizationStatus != UNAuthorizationStatusAuthorized &&
            settings.authorizationStatus != UNAuthorizationStatusProvisional)
            finish(3, @"Allow notifications for this app in System Settings > Notifications.");
        UNMutableNotificationContent *content = [UNMutableNotificationContent new];
        content.title = option(@"title") ?: NSBundle.mainBundle.infoDictionary[@"CFBundleDisplayName"];
        content.subtitle = option(@"subtitle") ?: @"";
        content.body = option(@"message") ?: @"";
        NSString *sound = option(@"sound");
        if (sound) content.sound = [sound isEqualToString:@"default"]
            ? UNNotificationSound.defaultSound : [UNNotificationSound soundNamed:sound];
        NSString *image = option(@"contentImage");
        if (image) {
            UNNotificationAttachment *attachment = [UNNotificationAttachment attachmentWithIdentifier:@"image"
                URL:[NSURL fileURLWithPath:image] options:nil error:nil];
            if (attachment) content.attachments = @[attachment];
        }
        NSString *identifier = option(@"group") ?: NSUUID.UUID.UUIDString;
        content.userInfo = @{@"groupID": identifier};
        UNNotificationRequest *request = [UNNotificationRequest requestWithIdentifier:identifier
                                            content:content trigger:nil];
        [center addNotificationRequest:request withCompletionHandler:^(NSError *error) {
            // Acceptance by macOS is success. Do not wait for a banner, which
            // Focus may suppress while keeping the notification in the center.
            finish(error ? 1 : 0, error ? @"macOS rejected the notification request." : nil);
        }];
    }];
}

static void modernCommand(UNUserNotificationCenter *center) {
    NSString *remove = option(@"remove"), *list = option(@"list");
    if (remove || list) {
        NSString *group = remove ?: list;
        // Include old/bypass alerts under this same bundle identity.
        NSUserNotificationCenter *legacy = NSUserNotificationCenter.defaultUserNotificationCenter;
        for (NSUserNotification *note in legacy.deliveredNotifications) {
            if ([group isEqualToString:@"ALL"] || [note.userInfo[@"groupID"] isEqualToString:group]) {
                if (remove) [legacy removeDeliveredNotification:note];
                else printf("%s\t%s\t%s\t%s\t%s\n", [note.userInfo[@"groupID"] UTF8String],
                            note.title.UTF8String, note.subtitle.UTF8String,
                            note.informativeText.UTF8String, note.actualDeliveryDate.description.UTF8String);
            }
        }
        [center getDeliveredNotificationsWithCompletionHandler:^(NSArray<UNNotification *> *notes) {
            NSMutableArray<NSString *> *identifiers = [NSMutableArray new];
            for (UNNotification *note in notes) {
                NSString *identifier = note.request.identifier;
                if ([group isEqualToString:@"ALL"] || [identifier isEqualToString:group]) {
                    [identifiers addObject:identifier];
                    if (list) printf("%s\t%s\t%s\t%s\t%s\n", identifier.UTF8String,
                        note.request.content.title.UTF8String, note.request.content.subtitle.UTF8String,
                        note.request.content.body.UTF8String, note.date.description.UTF8String);
                }
            }
            if (remove) {
                [center removeDeliveredNotificationsWithIdentifiers:identifiers];
                if ([group isEqualToString:@"ALL"]) [center removeAllPendingNotificationRequests];
                else [center removePendingNotificationRequestsWithIdentifiers:@[group]];
                // Keep the app alive until the service has processed removal.
                [center getDeliveredNotificationsWithCompletionHandler:^(NSArray<UNNotification *> *_remaining) {
                    if (option(@"message")) post(center); else finish(0, nil);
                }];
            } else finish(0, nil);
        }];
    } else post(center);
}

// Normal commands use the public API. Only explicit DND bypass retains the
// pinned upstream implementation and its best-effort private API.
int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSArray *args = NSProcessInfo.processInfo.arguments;
        BOOL diagnose = [args containsObject:@"--diagnose"];
        BOOL authorize = [args containsObject:@"--authorize"];
        NSApplication *app = [NSApplication sharedApplication];
        [app setActivationPolicy:NSApplicationActivationPolicyProhibited];
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (authorize ? 45 : 8) * NSEC_PER_SEC),
                       dispatch_get_main_queue(), ^{ finish(4, @"notification service timed out"); });
        if (diagnose || authorize) {
            UNUserNotificationCenter *center = UNUserNotificationCenter.currentNotificationCenter;
            if (authorize) {
                [center requestAuthorizationWithOptions:UNAuthorizationOptionAlert | UNAuthorizationOptionSound
                                      completionHandler:^(BOOL granted, NSError *error) {
                    finish(granted && !error ? 0 : 3,
                           granted ? @"notification permission: authorized" :
                           @"Allow notifications for this app in System Settings > Notifications.");
                }];
            } else {
                [center getNotificationSettingsWithCompletionHandler:^(UNNotificationSettings *settings) {
                    NSString *diagnostic = [NSString stringWithFormat:
                        @"notification authorization: %ld; alert style: %ld; alerts: %ld; sounds: %ld",
                        (long)settings.authorizationStatus, (long)settings.alertStyle,
                        (long)settings.alertSetting, (long)settings.soundSetting];
                    finish(settings.authorizationStatus == UNAuthorizationStatusAuthorized ? 0 : 3, diagnostic);
                }];
            }
            [app run];
        } else if ((option(@"message") || option(@"remove") || option(@"list")) &&
                   (![args containsObject:@"-ignoreDnD"] || option(@"remove") || option(@"list"))) {
            modernCommand(UNUserNotificationCenter.currentNotificationCenter);
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
