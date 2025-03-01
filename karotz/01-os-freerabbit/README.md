# Free Rabbit

Copy-pasted from https://www.freerabbits.nl/fros/
I'm affraid that this website goes offline like openkarotz...

## Custom instructions

Format as FAT32 your usb drive:

```bash
parted /dev/sdb
# mklabel msdos
# mkpart os fat32 1MiB 100%
# set 1 boot on
# quit

mkfs.fat -F 32 /dev/sdb1
```

Then copy paste this rootfs:

```bash
mkdir -p /mnt/usb
mount /dev/sdb1 /mnt/usb
cp -r . /mnt/usb
```

Then edit your WiFi settings and unmount the fs:

```bash
vim /mnt/usb/waitfornetwork.sh
# set the following values according to your network properties.
# WEP, WPA1, WPA2 only. 2.4GHz only
# IP="192.168.1.129"
# DNS="192.168.1.1"
# GW="192.168.1.1"
# NM="255.255.255.0"
# SSID="changeme"
# PWD="changeme"

sync /mnt/usb
umount /mnt/usb
```

Then:
 - Poweroff your Karotz
 - Reset it
   - keep the top button pressed while starting it with the volume button
   - keep the top button pressed while the led is flashing pink, release it when it becomes solid pink
   - wait (do nothing) while the led turns 1) dark blue 2) red 3) pulsing orange 4) red again
   - once it's fixed cyan, your rabbit is reset
   - you can power it off with the volume button
 - Plug your USB drive
 - Power on the Rabbit with the volume button
 - Listen to the instruction, let the installation complete, it will automatically reboot
 - Once it's rebooted, you can access it through telnet.

Telnet example:

```bash
telnet 192.168.1.129
# login: karotz
# no password is required
```

There is also a listening HTTP web server on port 80.
You can then install OpenKarotz if you want from this part.
