#!/usr/bin/env scriptisto

   // Self-contained, has no dependencies except Xcode CLI.

   // scriptisto-begin
   // script_src: main.c
   // build_cmd: clang -g -O2 -std=c11 -Wall -framework ApplicationServices main.c -o ./script
   // scriptisto-end

#include <stdio.h>
#include <ApplicationServices/ApplicationServices.h>

CG_EXTERN bool CGDisplayUsesForceToGray(void);
CG_EXTERN void CGDisplayForceToGray(bool forceToGray);

int main(int argc, char** argv)
{
  bool isGrayscale;
  if (argc > 1) {
    if (argv[1][0] == 'y') {
      isGrayscale = false; // will toggle this to true
    } else if (argv[1][0] == 's') {
      printf("Grayscale is now: %d\n", CGDisplayUsesForceToGray());
      return 0;
    }
    else {
      isGrayscale = true;
    }
  } else {
    isGrayscale = CGDisplayUsesForceToGray();
    printf("isGrayscale = %d\n", isGrayscale);
  }
  CGDisplayForceToGray(!isGrayscale);
  printf("Grayscale is now: %d\n", CGDisplayUsesForceToGray());

  return 0;
}
