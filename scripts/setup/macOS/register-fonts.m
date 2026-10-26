// Register copied fonts for the current graphical user.
#import <Foundation/Foundation.h>
#import <CoreText/CoreText.h>

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        if (argc < 2) {
            fputs("usage: register-fonts FONT_FILE ...\n", stderr);
            return 64;
        }
        NSMutableArray *rows = [NSMutableArray array];
        int failures = 0;
        for (int i = 1; i < argc; i++) {
            NSString *path = [NSString stringWithUTF8String:argv[i]];
            NSURL *url = [NSURL fileURLWithPath:path];
            CFErrorRef error = NULL;
            bool ok = CTFontManagerRegisterFontsForURL(
                (__bridge CFURLRef)url, kCTFontManagerScopeUser, &error);
            long code = error ? CFErrorGetCode(error) : 0;
            bool already = code == kCTFontManagerErrorAlreadyRegistered;
            bool accepted = ok || already;
            [rows addObject:@{
                @"file": path.lastPathComponent,
                @"registered": @(accepted),
                @"alreadyRegistered": @(already),
                @"code": @(code)
            }];
            if (!accepted) failures++;
            if (error) CFRelease(error);
        }
        NSData *data = [NSJSONSerialization
            dataWithJSONObject:@{@"fonts": rows, @"failures": @(failures)}
            options:NSJSONWritingPrettyPrinted error:NULL];
        fwrite(data.bytes, 1, data.length, stdout);
        putchar('\n');
        return failures ? 1 : 0;
    }
}
