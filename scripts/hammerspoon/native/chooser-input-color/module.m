// Query-only color for hs.chooser. Its public fgColor also recolors result rows.
// Uses LuaSkin's registered HSChooser conversion, without userdata layout casts.
#import <Cocoa/Cocoa.h>
#import <LuaSkin/LuaSkin.h>
#import <objc/runtime.h>

static char originalColorKey;

static int queryColor(lua_State *L) {
    LuaSkin *skin = [LuaSkin sharedWithState:L];
    [skin checkArgs:LS_TUSERDATA, "hs.chooser", LS_TTABLE | LS_TNIL | LS_TOPTIONAL, LS_TBREAK];
    id chooser = [skin toNSObjectAtIndex:1];
    if (![chooser respondsToSelector:NSSelectorFromString(@"queryField")]) {
        return luaL_error(L, "hs.chooser no longer exposes queryField");
    }
    NSTextField *field = [chooser valueForKey:@"queryField"];
    if (![field isKindOfClass:NSTextField.class]) {
        return luaL_error(L, "hs.chooser queryField is not a text field");
    }
    if (lua_gettop(L) == 1) {
        [skin pushNSObject:field.textColor];
        return 1;
    }
    NSColor *original = objc_getAssociatedObject(field, &originalColorKey);
    if (!original) {
        original = field.textColor ?: NSColor.controlTextColor;
        objc_setAssociatedObject(field, &originalColorKey, original, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
    NSColor *color = lua_isnil(L, 2) ? original : [skin luaObjectAtIndex:2 toClass:"NSColor"];
    if (!color) return luaL_error(L, "invalid query color");
    field.textColor = color;
    // Also update an already active field editor, preserving its selection.
    NSText *editor = field.currentEditor;
    if (editor) editor.textColor = color;
    lua_pushvalue(L, 1);
    return 1;
}

int luaopen_chooser_input_color(lua_State *L) {
    luaL_Reg functions[] = {{"color", queryColor}, {NULL, NULL}};
    luaL_newlib(L, functions);
    return 1;
}
