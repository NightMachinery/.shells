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
