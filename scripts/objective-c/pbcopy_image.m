#!/usr/bin/env scriptisto

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import <unistd.h>

// scriptisto-begin
// script_src: main.m
// build_cmd: clang -framework AppKit -framework Carbon -framework Foundation main.m -o ./script
// scriptisto-end
///
// * I added AppKit to make this compile per [[https://stackoverflow.com/questions/12713681/problems-with-nsimage]].
//
// * @forkedFrom http://stackoverflow.com/questions/2681630/how-to-read-png-image-to-nsimage or possibly https://gist.github.com/yuchuanfeng/a01069abff01993af2af048a0886a498
///

BOOL copy_to_clipboard(NSString *path)
{
  NSImage * image;
  if([path isEqualToString:@"-"])
  {
    // http://caiustheory.com/read-standard-input-using-objective-c
    NSFileHandle *input = [NSFileHandle fileHandleWithStandardInput];
    image = [[NSImage alloc] initWithData:[input readDataToEndOfFile]];
  }else
  {
    image =  [[NSImage alloc] initWithContentsOfFile:path];
  }
  // http://stackoverflow.com/a/18124824/148668
  BOOL copied = false;
  if (image != nil)
  {
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    [pasteboard clearContents];
    NSArray *copiedObjects = [NSArray arrayWithObject:image];
    copied = [pasteboard writeObjects:copiedObjects];
    [pasteboard release];
  }
  [image release];
  return copied;
}

int main(int argc, char * const argv[])
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  if(argc<2)
  {
    printf("Usage:\n\n"
      "Copy file to clipboard:\n    pbcopy_image.m path/to/file\n\n"
      "Copy stdin to clipboard:\n    cat /path/to/file | pbcopy_image.m -");
    return EXIT_FAILURE;
  }
  NSString *path= [NSString stringWithUTF8String:argv[1]];
  BOOL success = copy_to_clipboard(path);
  [pool release];
  return (success?EXIT_SUCCESS:EXIT_FAILURE);
}
