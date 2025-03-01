# Install a Scheme interpreter

Now the idea is to install a modern interpreter to script our way the Nabaztag if possible
with an as rich as possible standard library, that allow FFI so we can re-use the dbus libraries (or bind a statically compiled one).

Compile Chez for Karotz:

```bash
./configure \
  --cross \
  -m=pb \
  --toolprefix=arm-buildroot-linux-musleabi- \
  --disable-curses \
  --disable-x11 \
  --disable-auto-flags \
  CC_FOR_BUILD=cc \
  CFLAGS="-march=armv4t" \
  LDFLAGS="-static"
```
