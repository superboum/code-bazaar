# A toolchain

We want to build software for the Karotz then. We need to build a C toolchain first.
We will use Buildroot for that. Some info on the Karotz:

```
-bash-4.1# cat /proc/cpuinfo
Processor       : ARM920T rev 0 (v4l)
BogoMIPS        : 201.93
Features        : swp half
CPU implementer : 0x41
CPU architecture: 4T
CPU variant     : 0x1
CPU part        : 0x920
CPU revision    : 0

Hardware        : RABBITV3 BY PRAGMATEC
Revision        : 0000
Serial          : 0000000000000000
-bash-4.1# uname -a
Linux karotz 2.6.28-pragmatec -g53e06b1-dirty #1 Tue May 31 17:50:18 CEST 2011 armv4tl GNU/Linux
```

Used Latest long term support release: 2024.02.11

Manually patched linux 2.6.18 in the `output` folder with this patch (that did not get automatically applied):
https://github.com/cristim/buildroot/blob/master/toolchain/kernel-headers/linux-2.6.28.10-unifdef-getline.patch
