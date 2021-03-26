if isDarwin ; then
    objc-compile "$NIGHTDIR/objective-c/input_lang_get_objc.m"
    brew install --cask --appdir=/Applications megacmd
fi
