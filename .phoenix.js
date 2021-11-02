// @docs
// https://github.com/kasper/phoenix/blob/master/docs/API.md
// https://github.com/kasper/phoenix/blob/2.6.7/docs/API.md#logging-and-debugging
//
// The hotkey window code in hammerspoon: https://gist.github.com/programus/d3bf9c6b8f9b7648891d1f59b665b35e
//
// The JavaScript context is (re)loaded when the file changes.
//   So you can do `touch ~/.phoenix.js` to force a reload
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

// * KNOWN ISSUES
// ** You need to enable `Automatically hide and show the menu bar` or quake apps will look ugly
// ** doesn't work on minimized apps
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
const KITTY_APP_NAME = "kitty"

// the actual applications
///
// This works, but we need to first switch to the main space, and run the hotkey once there. I have no idea why.
quakeApp({
  key: "a",
  modifiers: hyper,
  appName: "racket",
  windowId: 0,
  position: topHalf,
  followsMouse: true,
  hideOnBlur: false,
});
///
quakeApp({
  // key: "m",
  key: "z",
  modifiers: hyper,
  appName: KITTY_APP_NAME,
  windowId: 0,
  // position: topHalf,
  position: full,
  followsMouse: true,
  // hideOnBlur: false, // set to false for sudo prompts
  hideOnBlur: true, // set to true for opening URLs easily
  preCommands: [
    // ["kitty_focused_set", "1"],
  ],
  postCommands: [
    // ["kitty_focused_set", "0"],
  ],
});

quakeApp({
  key: "f12",
  modifiers: [],
  appName: KITTY_APP_NAME,
  windowId: 0,
  position: full,
  followsMouse: true,
  hideOnBlur: true, // set to true for opening URLs easily
});

quakeApp({
  // key: "m",
  key: "x",
  modifiers: hyper,
  appName: KITTY_APP_NAME,
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
    // ["@opts", "match", "title:Clipper", "@", "kitty-send", "        "], // to start fzf forecfully
    ["@opts", "match", "title:Clipper", "@", "kitty-send", " "], // to start fzf forecfully
    // ["input-lang-push", "en"],
  ],
  postCommands: [
    // ["input-lang-pop"],
    ["@opts", "match", "title:Clipper", "@", "kitty-esc"],
  ],
});

var kittyMain = null;
var kittyClipper = null;
var quakeAppLock = false

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
    if (quakeAppLock) {
      // await brishz(['fsay', 'lock encountered']);
      return;
    }
    try {
      quakeAppLock = true;
      // var activeSpace = followsMouse ? mouseSpace() : Space.active();
      var activeSpace = Space.active();

      let [app, opened] = await startApp(appName, { focus: false });

      // if the app started
      var wid = windowId;
      const isActive = app.isActive()
      var moved = false;
      var space = activeSpace;
      if (app !== undefined) {
        ///
        var i = 0;
        // var windowIdTmp = wid;
        var windows = app.windows();
        var win_len = windows.length;

        if (appName == KITTY_APP_NAME) {
          if ((wid == 0 && !kittyMain) || (wid == 1 && !kittyClipper)) {
            // if ((!kittyMain) || (!kittyClipper)) {
            if (win_len == 0) {
              // @phoenixBug https://github.com/kasper/phoenix/issues/131
              // Windows in other spaces are NOT returned
              opened = true;
              brishz_sync(['fsay', 'Zero windows encountered']);
              app.focus();
              windows = app.windows();
              win_len = windows.length;
            }

            for (var w of windows) {
              // brishz_sync(['fsay', 'windows'])
              var title = w.title();
              if (appName == KITTY_APP_NAME) {
                if (title == "Clipper") {
                  // await brishz(["fsay", "hello"]);
                  if (wid == 1) {
                    // windowIdTmp = i;
                    kittyClipper = w
                    // brishz_sync(['fsay', 'kitty Clipper set']);
                    // w.unminimise(); // doesn't work
                  } else {
                    // w.minimise();
                  }
                } else {
                  if (wid == 0) {
                    // windowIdTmp = i;
                    kittyMain = w
                    // brishz_sync(['fsay', 'kitty main set']);
                    // w.unminimise();
                  } else {
                    // w.minimise();
                  }
                }
              }
            }
            // await brishz(["echo", title, " = title, ", i, " = i, ", wid, " = wid, ", windowIdTmp, " = widTmp"]);

            i += 1;
          }
        }
        // wid = windowIdTmp
        var window = null;

        if (appName == KITTY_APP_NAME) {
          if (wid == 0) {
            window = kittyMain
          } else {
            window = kittyClipper
          }
        } else {
          window = windows[wid];
        }
        ///


        // hide the app if it is active and wasn't just opened or moved to
        // a new space
        if (isActive) {
          // brishz_sync(['fsay', 'App', appName , 'is already active'])
          /// @duplicatedCode3581
          app.hide();
          if (postCommands && postCommands.length >= 1) {
            for (var cmd of postCommands) {
              await brishz(cmd)
            }
          }
          ///
        } else {
          if (appName == KITTY_APP_NAME) {
            moveAppToActiveSpace(app, followsMouse, wid, window, activeSpace)
            app.focus(); // do NOT window.raise() before this
            moveAppToActiveSpace(app, followsMouse, wid, window, activeSpace)
            setAppPosition(app, position, space, wid, window);
            window.raise();
            // app.focus();
            window.focus();
          } else {
            moveAppToActiveSpace(app, followsMouse, wid, window, activeSpace)
            app.focus();
            moveAppToActiveSpace(app, followsMouse, wid, window, activeSpace)
            setAppPosition(app, position, space, wid, window);
           }

          if (preCommands && preCommands.length >= 1) {
            for (var cmd of preCommands) {
              await brishz(cmd)
            }
          }
        }

        if (hideOnBlur) {
          const identifier = Event.on("appDidActivate", async function (activatedApp) {
            appName2 = app.name()
            activatedName = activatedApp.name()
            if (appName2 !== activatedName && activatedName !== "SecurityAgent" /* SecurityAgent is the name of the sudo touchID dialog */ && activatedName !== "Maccy") {

              // brishz_log('wanted: ' + appName2, 'activated: ' + activatedName)

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
      } else {
        brishz_sync(['fsay', 'App', appName , 'not defined'])
      }
    }
    finally {
      quakeAppLock = false;
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
function setAppPosition(app, relativeFrame, space, wid = 0, window = null) {
  // const mainWindow = app.mainWindow(); // get app window

  // brishz_sync(["echo", wid, " = wid (setAppPosition)"]);
  var mainWindow = window
  if (!mainWindow) {
    mainWindow = app.windows()[wid];
  }
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
    // mainWindow.maximize() // enabling this causes the app to permanently become 'shorter' and not overlap with the menubar. It's not worth it.
    mainWindow.setTopLeft({
      x: left,
      y: top,
      // y: -300,
    });
    mainWindow.setSize({
      width: right - left,
      height: (bottom - top),
      // height: 500,
    });
  }
}

/**
 * Move the passed in App to the currently active space
 * Returns whether the app was moved and the space the app is now in.
 * @param {App} app the application to move to the active space
 * @param {boolean} followsMouse whether the app should open in the screen containing the mouse or the key with keyboard focus
 */
function moveAppToActiveSpace(app, followsMouse, wid = 0, window = null, space = null) {
  // brishz_sync(['fsay', 'green'])
  // followsMouse = false;
  var activeSpace = space
  if (!activeSpace) {
    // brishz_sync(['fsay', 'no space provided'])
    activeSpace = followsMouse ? mouseSpace() : Space.active();
  }
  // const mainWindow = app.mainWindow(); // get app window

  // brishz_sync(["echo", wid, " = wid (moveAppToActiveSpace)"]);
  var mainWindow = window
  if (!mainWindow) {
    mainWindow = app.windows()[wid];
  }
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
    // moved = true;

    // brishz_sync(['fsay', 'blue'])
    if (moved) {
      // otherwise remove the main window from the spaces it is in

      // brishz_sync(['fsay', 'red'])
      mainWindow.spaces().forEach((space) => {
        // brishz_sync(['fsay', 'moved internal', moved])
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

function brishz_sync(...args) {
  (async () => {
    await brishz(...args)
  }
  )();
}

function brishz_log(...args) {
  cmd = ["alert"]
  cmd.push(args.join("\n"))
  brishz_sync(cmd)
}
///
// brishz_sync(["bell-sc2-evil-laugh"])
// brishz_sync(["fsay", "Phoenix reloaded"])
brishz_sync(['tts-glados1-cached', 'Phoenix flies']);
