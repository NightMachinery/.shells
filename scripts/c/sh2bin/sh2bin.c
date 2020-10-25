#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

/* include some file as a writable string */
#define INCWCHR(id, file) \
        __asm__(".data\n"STR(id)": .incbin \""STR(file)"\"\n.byte 0"); \
        extern char id[];
#define STR(s)  #s
#define WCHR(c) ((char[]){STR(c)})

INCWCHR(src, SRC)

#ifndef SH
#define SH      /bin/bash
#endif
#ifndef OPT_C
#define OPT_C   -pc
#endif

int main(int ac, char **av, char **ev){
        char *bav[] = { WCHR(SH), WCHR(OPT_C), src };
        void *dl; int (*sh_main)(int, char**, char**); char **nav;
        if(!(dl = dlopen(bav[0], RTLD_LAZY|RTLD_GLOBAL)))
                errx(1, "dlopen: %s", dlerror());
        if(!(sh_main = dlsym(dl, "main")))
                errx(1, "dlsym: %s", dlerror());
        if(!(nav = malloc(sizeof bav + (ac + 1) * sizeof *av)))
                err(1, "malloc");
        memcpy(nav, bav, sizeof bav);
        memcpy((char*)nav + sizeof bav, av, (ac + 1) * sizeof *av);
        return sh_main(ac + 3, nav, ev);
}
