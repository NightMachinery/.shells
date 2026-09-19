// Which build is running, as a string the reader can say out loud.
//
// It exists because "the page is deployed" and "the page you are looking at is
// the deployed one" are different claims, and only the second one matters. An
// installed app answers from a service worker cache, so a reader can be several
// deploys behind while the server is perfectly up to date, and nothing on the
// screen says so. A build id turns "it still looks wrong" into a question with
// an answer.
//
// The value is written into the page shell at publish time, replacing the
// placeholder below. A shell that was never published still says the
// placeholder, which is not a build id and is reported as none.

declare global {
  interface Window {
    __BUILD__?: string;
  }
}

/** What the unpublished shell carries, replaced when the page is published. */
export const BUILD_PLACEHOLDER = '__BUILD_ID__';

/** The running build's id, or null when the shell was never published. */
export function buildId(): string | null {
  const value = typeof window === 'undefined' ? undefined : window.__BUILD__;
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  if (trimmed === '' || trimmed === BUILD_PLACEHOLDER) return null;
  return trimmed;
}

/** The build id as a phrase, or a plain statement that there is not one. */
export function buildLabel(): string {
  const id = buildId();
  return id === null ? 'built from source, not published' : `build ${id}`;
}

/**
 * The build the running script was published as, stamped into the bundle.
 *
 * The shell says which build it is and so does the script, separately, because
 * the two can arrive from different builds. A browser holds the page, the
 * stylesheet and the script as three cache entries, and anything that fills
 * those entries one at a time can end up holding one of each from two deploys.
 * That happened, on a phone, and the result was a board drawn by one build and
 * laid out by another: rows off the side of the screen, a state word spilling
 * out of a badge with nowhere to sit. Nothing on the server was wrong, so no
 * check against the server could see it.
 *
 * Two things now make it impossible and this is the third, which is the one that
 * cleans up after a mixture that got in anyway, from an old worker or an HTTP
 * cache filled before either of them existed.
 */
export const BUNDLE_BUILD = '__BUNDLE_BUILD_ID__';

/**
 * What an unstamped placeholder looks like, matched by shape rather than
 * written out.
 *
 * Deliberately a pattern: a literal copy of either placeholder in this file
 * would itself be replaced at publish time, and the test for "was this ever
 * published" would then answer no on every published page.
 */
const UNSTAMPED = /^__[A-Z_]+__$/;

/** The key the one permitted reload is remembered under, per tab. */
const HEALED_KEY = 'transit-shell-mismatch';

/**
 * Reload once when the shell and the script disagree about which build they are.
 *
 * Returns whether a reload was started, so a caller can stop booting into a
 * layout it is about to throw away. It asks the worker to update first, because
 * a plain reload would be served the same mixture by the same worker; the update
 * installs a whole shell at once, and the reload then lands on it.
 *
 * The flag is per tab and remembers which pair it reloaded for. A page that
 * comes back with the same disagreement is a server-side problem, not a cache
 * one, and the right answer there is to say so in the console and carry on
 * rather than to reload for ever.
 */
export function healMixedShell(): boolean {
  const shell = typeof window === 'undefined' ? undefined : window.__BUILD__;
  if (typeof shell !== 'string' || shell.trim() === '' || UNSTAMPED.test(shell.trim())) return false;
  if (UNSTAMPED.test(BUNDLE_BUILD)) return false;
  if (shell.trim() === BUNDLE_BUILD) return false;

  const pair = `${shell.trim()}|${BUNDLE_BUILD}`;
  const note = `transit: mixed shell. The page says build ${shell.trim()} and this script says build ${BUNDLE_BUILD}.`;
  let healed: string | null = null;
  try {
    healed = sessionStorage.getItem(HEALED_KEY);
  } catch {
    // A blocked storage is not a reason to skip the repair, only a reason to be
    // unable to remember it. Treat it as "not tried yet" and rely on the update
    // succeeding; a worker that cannot store anything cannot loop for long.
  }
  if (healed === pair) {
    console.warn(`${note} A reload already failed to fix it, so this is the server's copy. Leaving it alone.`);
    return false;
  }
  console.warn(`${note} Updating the worker and reloading once.`);
  try {
    sessionStorage.setItem(HEALED_KEY, pair);
  } catch {
    // As above.
  }

  const reload = (): void => window.location.reload();
  if (!('serviceWorker' in navigator)) {
    reload();
    return true;
  }
  // Capped, because a worker that never settles must not leave the reader on a
  // broken page for ever. The versioned URLs mean a plain reload is already an
  // improvement on what is on screen.
  const capped = new Promise<void>((resolve) => window.setTimeout(resolve, 3_000));
  void Promise.race([
    navigator.serviceWorker
      .getRegistration()
      .then((registration) => (registration === undefined ? undefined : registration.update()))
      .then(() => undefined)
      .catch(() => undefined),
    capped,
  ]).then(reload);
  return true;
}
