#!/usr/bin/env scriptisto

   // See also:
   // https://stackoverflow.com/questions/65596323/how-to-programmatically-apply-color-filters-on-the-screen
   // https://superuser.com/questions/1615487/how-to-programmatically-apply-color-filters-on-the-screen

   // Forked from https://stackoverflow.com/questions/14163788/how-does-on-screen-color-inversion-work-in-os-x

   // Self-contained, has no dependencies except Xcode CLI.

   // scriptisto-begin
   // script_src: main.c
   // build_cmd: clang -g -O2 -std=c11 -Wall -framework ApplicationServices main.c -o ./script
   // scriptisto-end

   //ApplicationServices includes CoreGraphics
#import <ApplicationServices/ApplicationServices.h>
/* #include <stdio.h> */

int main(int argc, const char * argv[])
{
  double dur = 3;
  double t1 = 1;
  double t2 = 0;

  if (argc > 1) {
    dur = strtod(argv[1], NULL);
  };
  if (argc > 3) {
    t1 = strtod(argv[2], NULL);
    t2 = strtod(argv[3], NULL);
  };

  CGGammaValue table[] = {t1, t2};  // Graphics cards have two of these gamma tables to modify color output after composition into the final frame buffer. One of them can be modified by applications to alter the way the screen shows any RGB value.
  CGSetDisplayTransferByTable(CGMainDisplayID(), sizeof(table) / sizeof(table[0]), table, table, table);
  usleep(dur*1000000); // usleep is in microseconds
  return 0;
}
