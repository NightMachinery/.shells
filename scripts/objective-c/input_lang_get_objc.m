#!/usr/bin/env scriptisto

#import <Foundation/Foundation.h>
#include <Carbon/Carbon.h>

// scriptisto-begin
// script_src: main.m
// build_cmd: clang -framework Carbon -framework Foundation main.m -o ./script
// scriptisto-end

int main (int argc, const char * argv[]) {
  TISInputSourceRef current_source = TISCopyCurrentKeyboardInputSource();
  NSString *s = (__bridge NSString *)(TISGetInputSourceProperty(current_source, kTISPropertyInputSourceID));
  // get last part of string (without com.apple.keylayout.)
  NSUInteger last_dot_num = [s rangeOfString:@"." options:NSBackwardsSearch].location;
  NSString *substring = [s substringFromIndex:last_dot_num + 1];
  printf("%s", [substring UTF8String]);

  return 0;
}
