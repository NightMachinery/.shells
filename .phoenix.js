// @docs https://github.com/kasper/phoenix/blob/master/docs/API.md
//
// The JavaScript context is (re)loaded when the file changes.
//
// See also:
// * https://github.com/fabiospampinato/phoenix (has reload)
// * https://github.com/kasper/phoenix/wiki#stable-22
/// Forked from https://github.com/lukesmurray/bootstrap/blob/master/.phoenix.js
// Guake Style Applications
// Must use the following setting
// Apple menu > System Preferences > Mission Control > Displays have Separate Spaces
// uses apple script hacks to get around the following issue
// https://github.com/kasper/phoenix/issues/209
// helper for finding application names at the bottom of the file
// while developing run log stream --process Phoenix in a console

// KNOWN ISSUE
// doesn't work on minimized apps
// https://github.com/kasper/phoenix/issues/269

// common screen locations
const topHalf = {
  left: 0,
  top: 0,
  right: 0,
  bottom: 0.5,
};

const leftHalf = {
  left: 0,
  top: 0,
  right: 0.5,
  bottom: 0,
};

const lowerLeftHalf = {
  left: 0,
  top: 0.5,
  right: 0.5,
  bottom: 0,
};

const rightHalf = {
  left: 0.5,
  top: 0,
  right: 0,
  bottom: 0,
};

const full = {
  left: 0,
  top: 0,
  right: 0,
  bottom: 0,
};

const hyper = ["cmd", "shift", "control", "alt"]
// the actual applications
quakeApp({
  // key: "m",
  key: "z",
  modifiers: hyper,
  appName: "kitty",
  windowId: 0,
  // position: topHalf,
  position: full,
  followsMouse: true,
  hideOnBlur: false, // false for sudo prompts
  preCommands: [
    // ["kitty_focused_set", "1"],
  ],
  postCommands: [
    // ["kitty_focused_set", "0"],
  ],
});

quakeApp({
  // key: "m",
  key: "x",
  modifiers: hyper,
  appName: "kitty",
  windowId: 1,
  // position: topHalf,
  // position: rightHalf,
  position: full,
  followsMouse: true,
  hideOnBlur: false, // false for sudo prompts
  preCommands: [
    ///
    // ["activate-iloop2-clipboard"],
    // ["kitty-C-c"],
    // ["kitty-send", "iloop2-clipboard"],
    // ["input-lang-push", "en"],
    /// use with iloop-clipboard:
    // ["kitty-esc"],
    // ["input-lang-push", "en"],
    ///
    ["@opts", "match", "title:Clipper", "@", "kitty-send", "        "], // to start fzf forecfully
    // ["input-lang-push", "en"],
  ],
  postCommands: [
    // ["input-lang-pop"],
    ["@opts", "match", "title:Clipper", "@", "kitty-esc"],
  ],
});

/**
 * Create a keyboard event listener which implements a quake app
 * @param {string} key the key which triggers the app
 * @param {string[]} modifiers the modifiers which must be used in combination with the key (["alt", "ctrl"])
 * @param {string} appName the name of the app
 * @param {{left: number, top: number, right: number, bottom: number}} relativeFrame the margins to place the application in.
 * @param {followsMouse} boolean whether the app should open in the screen containing the mouse
 * @param {hideOnBlur} boolean whether the window should hide when it loses focus
 */
function quakeApp({
  key,
  modifiers = hyper,
  appName,
  position,
  followsMouse = true,
  hideOnBlur = true,
  preCommands = [],
  postCommands = [],
  windowId = 0,
}) {
  Key.on(key, modifiers, async function (_, repeat) {
    // ignore keyboard repeats
    if (repeat) {
      return;
    }
    let [app, opened] = await startApp(appName, { focus: false });

    // if the app started
    var wid = windowId;
    if (app !== undefined) {
      ///
      var i = 0;
      var windowIdTmp = wid;
      for (var w of app.windows()) {
         var title = w.title();
        if (appName == "kitty") {
          if (title == "Clipper") {
            // await brishz(["fsay", "hello"]);
            if (wid == 1) {
              windowIdTmp = i;
              // w.unminimise(); // doesn't work
            } else {
              // w.minimise();
            }
          } else {
            if (wid == 0) {
              windowIdTmp = i;
              // w.unminimise();
            } else {
              // w.minimise();
            }
          }
        }
        // await brishz(["echo", title, " = title, ", i, " = i, ", wid, " = wid, ", windowIdTmp, " = widTmp"]);

        i += 1;
      }
      wid = windowIdTmp
      ///
      // move the app to the currently active space
      const { moved, space } = moveAppToActiveSpace(app, followsMouse, wid);
      // moveAppToActiveSpace(app, followsMouse, 0)
      // moveAppToActiveSpace(app, followsMouse, 1)

      // set the app position
      setAppPosition(app, position, space, wid);

      // hide the app if it is active and wasn't just opened or moved to
      // a new space
      if (app.isActive() && !opened && !moved) {
        /// @duplicatedCode3581
        app.hide();
        if (postCommands && postCommands.length >= 1) {
          for (var cmd of postCommands) {
            await brishz(cmd)
          }
        }
        ///
      } else {
        app.focus();
        const w = app.windows()[wid];
        w.raise()

        if (preCommands && preCommands.length >= 1) {
          for (var cmd of preCommands) {
            await brishz(cmd)
          }
        }
      }

      if (hideOnBlur) {
        const identifier = Event.on("appDidActivate", async function (activatedApp) {
          if (app.name() !== activatedApp.name()) {
            /// @duplicatedCode3581
            app.hide();
            if (postCommands && postCommands.length >= 1) {
              for (var cmd of postCommands) {
                await brishz(cmd)
              }
            }

            Event.off(identifier);
          }
        });
      }
    }
  });
}

/**
 * Positions an application using margins which are a percentage of the width and height.
 * left: 0 positions the left side of the app on the left side of the screen.
 * left: .5 positions the left side of the app half the width from the left side of the screen.
 * {left: 0, right: 0, top: 0, bottom: 0} would be full screen
 * {left: .25, right: .25, top: .25, bottom: .25} would be centered with half the screen height
 * {left: 0, right: .5, top: 0, bottom: .5} would be the top left quadrant
 * @param {App} app the application to set the position of
 * @param {{left: number, top: number, right: number, bottom: number}} relativeFrame the margins to place the application in.
 * @param {Space} space the space to position the app in
 */
function setAppPosition(app, relativeFrame, space, wid = 0) {
  // const mainWindow = app.mainWindow(); // get app window

  // brishz_sync(["echo", wid, " = wid (setAppPosition)"]);
  const mainWindow = app.windows()[wid];
  if (space.screens().length > 1) {
    // check one space per screen
    throw new Error(DISPLAYS_HAVE_SEPARATE_SPACES);
  } else if (space.screens().length > 0) {
    // set the position of the app
    const activeScreen = space.screens()[0];
    const screen = activeScreen.flippedVisibleFrame();

    const xmargin = screen.x
    const ymargin = screen.y
    // const xmargin = 0
    // const ymargin = 0

    const left = xmargin + relativeFrame.left * screen.width;
    ///
    // const top = ymargin + relativeFrame.top * screen.height;
    // https://github.com/kasper/phoenix/issues/270
    const top =  0
    ///
    const right = xmargin + screen.width - relativeFrame.right * screen.width;
    const bottom =
      ymargin + screen.height - relativeFrame.bottom * screen.height;
    if (mainWindow.isFullScreen()) {
      mainWindow.setFullScreen(false); // this uses the native fullscreen functionality so setting it to true is no good for popup windows
    }
    // mainWindow.maximize()
    mainWindow.setTopLeft({
      x: left,
      y: top,
    });
    mainWindow.setSize({
      width: right - left,
      height: bottom - top,
    });
  }
}

/**
 * Move the passed in App to the currently active space
 * Returns whether the app was moved and the space the app is now in.
 * @param {App} app the application to move to the active space
 * @param {boolean} followsMouse whether the app should open in the screen containing the mouse or the key with keyboard focus
 */
function moveAppToActiveSpace(app, followsMouse, wid = 0) {
  const activeSpace = followsMouse ? mouseSpace() : Space.active();
  // const mainWindow = app.mainWindow(); // get app window

  // brishz_sync(["echo", wid, " = wid (moveAppToActiveSpace)"]);
  const mainWindow = app.windows()[wid];
  let moved = false; // boolean if the app was moved to a new space
  if (mainWindow.spaces().length > 1) {
    // check one space per screen
    throw new Error(DISPLAYS_HAVE_SEPARATE_SPACES);
  }
  if (activeSpace !== undefined) {
    // check if the main window was moved
    moved = !!!(
      mainWindow.spaces().length > 0 &&
      mainWindow.spaces()[0].isEqual(activeSpace)
    );
    if (moved) {
      // otherwise remove the main window from the spaces it is in
      mainWindow.spaces().forEach((space) => {
        space.removeWindows([mainWindow]);
      });
      // add window to active space
      activeSpace.addWindows([mainWindow]);
    }
  }
  return { moved, space: activeSpace };
}

/**
 * Get or launch the application with the passed in name.
 * Returns the app and a boolean for if the app was opened. app is undefined if the application fails to start.
 * @param {string} appName the name of the application to start
 * @param {{focus: boolean}} options focus determines whether or not to focus the app on launch
 */
async function startApp(appName) {
  // https://github.com/kasper/phoenix/issues/209
  // basically a hack to get around this bug

  // get the app if it is open
  let app = App.get(appName);
  let opened = false;

  // if app is open
  if (app !== undefined) {
    // make sure it has an open window
    win_len = app.windows().length;
    // await brishz(["echo", win_len, " = win_len"]);
    if (win_len === 0) {
      // if not open a new window
      await osascript(`tell application "${appName}"
        try
            reopen
        on error
          log "can not reopen the app"
          activate
        end
          end tell
        `);
      opened = true;
    }
  } else {
    // if app is not open activate it
    await osascript(`tell application "${appName}"
            activate
          end tell
        `);

    app = App.get(appName);
    opened = true;
  }

  return [app, opened];
}

/**
 * Return a promise containing the Task handler used to run the osascript.
 * The promise is resolved or rejected with the handler based on the status.
 * @param {string} script the osascript script to run
 */
function osascript(script) {
  return new Promise((resolve, reject) =>
    Task.run("/usr/bin/osascript", ["-e", script], (handler) => {
      if (handler.status === 0) {
        return resolve(handler);
      } else {
        return reject(handler);
      }
    })
  );
}

/**
 * Get the space which contains the mouse
 */
function mouseSpace() {
  const mouseLocation = Mouse.location();
  const screen = Screen.all().find((s) =>
    screenContainsPoint(s, mouseLocation)
  );
  if (screen !== undefined) {
    return screen.currentSpace();
  }
}

/**
 * Return whether the point is contained in the screen
 * @param {Screen} screen a screen object to check for a point
 * @param {Point} point a point using flipped coordinates (origin upper left)
 */
function screenContainsPoint(screen, point) {
  const frame = screen.flippedFrame();
  return (
    point.x >= frame.x &&
    point.x <= frame.x + frame.width &&
    point.y >= frame.y &&
    point.y <= frame.y + frame.height
  );
}

/**
 * Error message for invalid display settings
 */
const DISPLAYS_HAVE_SEPARATE_SPACES = `Must set Apple menu > System Preferences > Mission Control > Displays have Separate Spaces`;

// Finding Application Names
// to find application names run the following command
// open the app you're interested in
// open a phoenix log `log stream --process Phoenix`
// uncomment the following keyboard shortcut to trigger a log of open application names
// Key.on("a", ["alt", "shift"], () => {
//   const array = App.all()
//     .map((a) => a.name())
//     .sort();
//   let chunk = 10;
//   Phoenix.log();
//   Phoenix.log("************ APPLICATIONS START *************");
//   for (let i = 0, j = array.length; i < j; i += chunk) {
//     let temp = array.slice(i, i + chunk);
//     Phoenix.log(temp);
//   }
//   Phoenix.log("************ APPLICATIONS END *************");
//   Phoenix.log();
// });

///
function brishz(...args) {
  args = [].concat.apply([], args); // flatten args
  // args = args.map(x => JSON.stringify(x))
  args = args.map(x => x.toString())

  return new Promise((resolve, reject) =>
    Task.run("/usr/local/bin/brishzq.zsh", args, (handler) => {
      if (handler.status === 0) {
        return resolve(handler);
      } else {
        return reject(handler);
      }
    })
  );
}
///
function brishz_sync(...args) {
  (async () => {
    await brishz(...args)
  }
  )();
}
brishz_sync(["bell-sc2-evil-laugh"])
