#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import <CoreText/CoreText.h>
#import <dlfcn.h>
#import <math.h>
#import <stdlib.h>
#import <string.h>
#import <lua.h>
#import <lauxlib.h>

static int push_empty_bitmap(lua_State *L) {
    lua_createtable(L, 0, 0);
    lua_pushinteger(L, 0);
    lua_pushinteger(L, 0);
    lua_pushinteger(L, 0);
    return 4;
}

static int rasterize(lua_State *L) {
    size_t len = 0;
    const char *utf8 = luaL_checklstring(L, 1, &len);
    const char *fontCStr = luaL_optstring(L, 2, "Tahoma");
    double size = luaL_optnumber(L, 3, 12.0);
    if (!isfinite(size)) size = 12.0;
    if (size < 6.0) size = 6.0;
    if (size > 64.0) size = 64.0;
    if (len == 0) return push_empty_bitmap(L);

    @autoreleasepool {
        NSString *text = [[NSString alloc] initWithBytes:utf8 length:len encoding:NSUTF8StringEncoding];
        if (!text) return luaL_error(L, "invalid UTF-8 text");
        NSUInteger strLen = text.length;
        if (strLen == 0) return push_empty_bitmap(L);

        NSString *fontName = fontCStr ? [NSString stringWithUTF8String:fontCStr] : nil;
        if (!fontName || fontName.length == 0) fontName = @"Tahoma";

        CTFontRef font = CTFontCreateWithName((__bridge CFStringRef)fontName, (CGFloat)size, NULL);
        if (!font) return luaL_error(L, "cannot create font");

        NSDictionary *attrs = @{
            (__bridge id)kCTFontAttributeName: (__bridge id)font,
            (__bridge id)kCTForegroundColorFromContextAttributeName: @YES
        };
        CFAttributedStringRef attrStr = CFAttributedStringCreate(
            kCFAllocatorDefault, (__bridge CFStringRef)text, (__bridge CFDictionaryRef)attrs);
        if (!attrStr) {
            CFRelease(font);
            return luaL_error(L, "cannot create attributed string");
        }

        CTLineRef line = CTLineCreateWithAttributedString(attrStr);
        if (!line) {
            CFRelease(attrStr);
            CFRelease(font);
            return luaL_error(L, "cannot create text line");
        }

        CGFloat ascent = 0, descent = 0, leading = 0;
        double lineW = CTLineGetTypographicBounds(line, &ascent, &descent, &leading);
        if (!isfinite(lineW) || lineW < 0) lineW = 0;
        if (!isfinite(ascent) || ascent < 0) ascent = 0;
        if (!isfinite(descent) || descent < 0) descent = 0;

        int pad = 8;
        int w = (int)ceil(lineW) + 2 * pad;
        int h = (int)ceil(ascent + descent) + 2 * pad;
        if (w < 16) w = 16;
        if (h < 16) h = 16;
        if (w > 4096) w = 4096;
        if (h > 4096) h = 4096;

        size_t pxCount = (size_t)w * (size_t)h;
        int *clusterOf = calloc(strLen + 1, sizeof(int));
        int *clusterLit = calloc(strLen + 1, sizeof(int));
        int *clusterRank = calloc(strLen + 1, sizeof(int));
        uint8_t *buf = calloc(pxCount, 1);
        uint8_t *best = calloc(pxCount, 1);
        int *owner = calloc(pxCount, sizeof(int));

        CGColorSpaceRef cs = CGColorSpaceCreateDeviceGray();
        CGContextRef ctx = (buf && cs)
            ? CGBitmapContextCreate(buf, (size_t)w, (size_t)h, 8, (size_t)w, cs, (CGBitmapInfo)kCGImageAlphaNone)
            : NULL;

        if (!clusterOf || !clusterLit || !clusterRank || !buf || !best || !owner || !cs || !ctx) {
            if (ctx) CGContextRelease(ctx);
            if (cs) CGColorSpaceRelease(cs);
            free(buf); free(best); free(owner);
            free(clusterOf); free(clusterLit); free(clusterRank);
            CFRelease(line); CFRelease(attrStr); CFRelease(font);
            return luaL_error(L, "cannot allocate raster buffer");
        }

        // Map each UTF-16 code unit to a 1-based composed character cluster ID in logical order.
        CFStringRef cfText = (__bridge CFStringRef)text;
        for (NSUInteger i = 0; i < strLen;) {
            CFRange r = CFStringGetRangeOfComposedCharactersAtIndex(cfText, (CFIndex)i);
            int cid = (int)r.location + 1;
            NSUInteger end = (NSUInteger)(r.location + r.length);
            for (NSUInteger j = (NSUInteger)r.location; j < end && j < strLen; j++) {
                clusterOf[j] = cid;
            }
            i = end > i ? end : i + 1;
        }

        CGContextSetAllowsAntialiasing(ctx, false);
        CGContextSetShouldAntialias(ctx, false);
        CGContextSetGrayFillColor(ctx, 1.0, 1.0);
        CGFloat originY = ceil(descent) + (CGFloat)pad;

        // Draw each glyph separately so CoreText keeps exact Arabic/Persian shaping and ligatures
        // while we track which logical character cluster owns each lit pixel.
        CFArrayRef runs = CTLineGetGlyphRuns(line);
        CFIndex runCount = CFArrayGetCount(runs);
        for (CFIndex r = 0; r < runCount; r++) {
            CTRunRef run = (CTRunRef)CFArrayGetValueAtIndex(runs, r);
            CFDictionaryRef runAttrs = CTRunGetAttributes(run);
            CTFontRef runFont = font;
            if (runAttrs) {
                id customFont = ((__bridge NSDictionary *)runAttrs)[(__bridge id)kCTFontAttributeName];
                if (customFont) runFont = (__bridge CTFontRef)customFont;
            }
            CFIndex count = CTRunGetGlyphCount(run);
            if (count <= 0) continue;

            CGGlyph *glyphs = malloc((size_t)count * sizeof(CGGlyph));
            CGPoint *positions = malloc((size_t)count * sizeof(CGPoint));
            CFIndex *indices = malloc((size_t)count * sizeof(CFIndex));
            if (!glyphs || !positions || !indices) {
                free(glyphs); free(positions); free(indices);
                CGContextRelease(ctx); CGColorSpaceRelease(cs);
                free(buf); free(best); free(owner);
                free(clusterOf); free(clusterLit); free(clusterRank);
                CFRelease(line); CFRelease(attrStr); CFRelease(font);
                return luaL_error(L, "cannot allocate glyph arrays");
            }

            CTRunGetGlyphs(run, CFRangeMake(0, count), glyphs);
            CTRunGetPositions(run, CFRangeMake(0, count), positions);
            CTRunGetStringIndices(run, CFRangeMake(0, count), indices);

            for (CFIndex g = 0; g < count; g++) {
                CFIndex idx = indices[g];
                int cid = (idx >= 0 && (NSUInteger)idx < strLen) ? clusterOf[idx] : 1;
                if (cid < 1 || (NSUInteger)cid > strLen) cid = 1;
                memset(buf, 0, pxCount);
                CGPoint pos = CGPointMake((CGFloat)pad + positions[g].x, originY + positions[g].y);
                CTFontDrawGlyphs(runFont, &glyphs[g], &pos, 1, ctx);
                for (size_t p = 0; p < pxCount; p++) {
                    if (buf[p] >= 128 && buf[p] >= best[p]) {
                        best[p] = buf[p];
                        owner[p] = cid;
                        clusterLit[cid] = 1;
                    }
                }
            }
            free(glyphs); free(positions); free(indices);
        }

        // Dense 1..letters numbering over visible clusters preserves logical reading order.
        int letters = 0;
        for (NSUInteger c = 1; c <= strLen; c++) {
            if (clusterLit[c]) clusterRank[c] = ++letters;
        }

        int minX = w, minY = h, maxX = -1, maxY = -1;
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                if (owner[(size_t)y * (size_t)w + (size_t)x] > 0) {
                    if (x < minX) minX = x;
                    if (x > maxX) maxX = x;
                    if (y < minY) minY = y;
                    if (y > maxY) maxY = y;
                }
            }
        }

        lua_createtable(L, 0, 0);
        int outW = 0, outH = 0;
        if (maxX >= minX && maxY >= minY) {
            outW = maxX - minX + 1;
            outH = maxY - minY + 1;
            lua_Integer n = 0;
            for (int y = minY; y <= maxY; y++) {
                for (int x = minX; x <= maxX; x++) {
                    int cid = owner[(size_t)y * (size_t)w + (size_t)x];
                    if (cid > 0) {
                        lua_createtable(L, 0, 3);
                        lua_pushinteger(L, x - minX);
                        lua_setfield(L, -2, "x");
                        lua_pushinteger(L, y - minY);
                        lua_setfield(L, -2, "y");
                        lua_pushinteger(L, clusterRank[cid]);
                        lua_setfield(L, -2, "letter");
                        lua_rawseti(L, -2, ++n);
                    }
                }
            }
        }

        CGContextRelease(ctx);
        CGColorSpaceRelease(cs);
        free(buf); free(best); free(owner);
        free(clusterOf); free(clusterLit); free(clusterRank);
        CFRelease(line); CFRelease(attrStr); CFRelease(font);

        lua_pushinteger(L, outW);
        lua_pushinteger(L, outH);
        lua_pushinteger(L, letters);
        return 4;
    }
}

int luaopen_text_bitmap(lua_State *L) {
    Dl_info info;
    if (dladdr((const void *)luaopen_text_bitmap, &info) && info.dli_fname) {
        // Keep Objective-C/CoreText classes pinned in process until exit.
        dlopen(info.dli_fname, RTLD_NOW | RTLD_NODELETE);
    }
    luaL_Reg functions[] = {
        {"rasterize", rasterize},
        {NULL, NULL},
    };
    luaL_newlib(L, functions);
    return 1;
}
