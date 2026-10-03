// End stale remote-login rows in the macOS login table (/var/run/utmpx).
//
// A row is stale when it is a USER_PROCESS row with a remote host whose
// process no longer exists. sshd normally ends its row at logout; a session
// that dies without that leaves the row behind, and a local terminal that
// later reuses the tty inherits it -- `who -m' then reports an SSH login, which
// is what made pure show `user@host' in plain kitty tabs. See
// ~/scripts/docs/utmpx-stale-ssh.md.
//
//   utmpx_clean_stale           # dry run: list the rows that would be ended
//   utmpx_clean_stale --apply   # end them (root only)
//
// Ending a row means writing it back as DEAD_PROCESS through pututxline, which
// is what a clean logout does, so `last' then shows the session as ended. A row
// whose pid was reused by an unrelated process is left alone until that
// process exits: we only act on a pid that provably does not exist.
//
// Installed as a root-owned snapshot by [agfi:utmpx-clean-daemon-install];
// the LaunchDaemon never runs anything from the repository.

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <utmpx.h>

static int is_stale(const struct utmpx *u) {
  if (u->ut_type != USER_PROCESS) return 0;
  if (u->ut_host[0] == '\0') return 0; // local login
  if (u->ut_pid <= 0) return 0;
  // EPERM means the process exists but is not ours: alive.
  return kill(u->ut_pid, 0) == -1 && errno == ESRCH;
}

static void print_row(const char *verb, const struct utmpx *u) {
  char when[32];
  time_t t = u->ut_tv.tv_sec;
  strftime(when, sizeof when, "%Y-%m-%d %H:%M", localtime(&t));
  printf("%s %.*s pid=%d user=%.*s host=%.*s since=%s\n", verb,
         (int)sizeof u->ut_line, u->ut_line, (int)u->ut_pid,
         (int)sizeof u->ut_user, u->ut_user,
         (int)sizeof u->ut_host, u->ut_host, when);
}

int main(int argc, char **argv) {
  int apply = 0;
  if (argc == 2 && strcmp(argv[1], "--apply") == 0) {
    apply = 1;
  } else if (argc != 1) {
    fprintf(stderr, "usage: %s [--apply]\n", argv[0]);
    return 2;
  }
  if (apply && geteuid() != 0) {
    fprintf(stderr, "%s: --apply needs root (utmpx is root-owned)\n", argv[0]);
    return 1;
  }

  // Collect first, write afterwards: pututxline moves the same cursor that
  // getutxent is walking.
  struct utmpx *stale = NULL;
  size_t n = 0, cap = 0;
  struct utmpx *u;
  setutxent();
  while ((u = getutxent()) != NULL) {
    if (!is_stale(u)) continue;
    if (n == cap) {
      cap = cap ? cap * 2 : 16;
      struct utmpx *grown = realloc(stale, cap * sizeof *stale);
      if (grown == NULL) { perror("realloc"); endutxent(); return 1; }
      stale = grown;
    }
    stale[n++] = *u;
  }
  endutxent();

  int failed = 0;
  for (size_t i = 0; i < n; i++) {
    if (!apply) { print_row("stale", &stale[i]); continue; }
    struct utmpx dead = stale[i];
    dead.ut_type = DEAD_PROCESS;
    gettimeofday(&dead.ut_tv, NULL);
    setutxent();
    if (pututxline(&dead) == NULL) {
      fprintf(stderr, "pututxline failed for %.*s: %s\n",
              (int)sizeof dead.ut_line, dead.ut_line, strerror(errno));
      failed = 1;
    } else {
      print_row("ended", &stale[i]);
    }
    endutxent();
  }
  free(stale);
  fflush(stdout);
  return failed;
}
