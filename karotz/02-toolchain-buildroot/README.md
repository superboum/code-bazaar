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
