# Install

## Overview
These Nix derivations create install media used to bootstrap a virtual machine. Upon initial boot, you're greeted with the same NixOS prompt as if you're installing from a USB stick.

## Steps to install
The trick is to configure your installation system just enough to give Nix enough information to go on. Nix will detect most settings and do the final heavy lifting.

In practice, this "state" I'm talking about requires:

1. Disk partitions created
2. ZFS pools and datasets created (or whatever you use)
3. Successfully mounting your "ideal" system

Don't worry, I go into more detail below.

### Boot the VM
### Partition disks
I use ZFS, but you can just as easily use ```ext4``` with ```fdisk```.

#### Verify we see a disk with no partition

```sh
$ lsblk -p
```

#### Create and format disk partitions
Move over to `sudo` and bring `sgdisk` into your path.

```sh
$ sudo su -
$ nix-shell -p gptfdisk
```
Create a small partition for the MBR and the rest to ZFS. This command is a bit archaic so let me breakdown what we're doing.

```sh
$ sgdisk -a1 -n2:34:2047 -t2:EF02 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
$ sgdisk -n1:0:0 -t1:BF01 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
```

> 1. a1 = set alignment to 1, default is 2048. We want to grab the extra bits for our boot record, so move this forward.
> 2. n1 or n2 = new partition followed by the assigned number, start and end sectors.
> 3. t1 = set partition's type code. For a full list, sgdisk -L.

Proxmox virtual machines by default boot using SeaBIOS, the old BIOS MBR boot record scheme. This is not modern day UEFI. So we partition for the machine to reflect this, using type code `EF02` (BIOS Boot) for a small 2MB part.

For ZFS, we use type code `BF01` (Solaris & Apple ZFS).
More info on Proxmox and ZFS: https://pve.proxmox.com/wiki/ZFS_on_Linux

#### Configure ZFS
##### Create zpool
`compression=on` lets ZFS choose the best compression available, not always `lz4`.

```sh
$ zpool create -O mountpoint=none -O atime=off -O compression=on -O xattr=sa -O acltype=posixacl -R /mnt rpool /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0-part1
```
##### Create datasets
Nix requires `legacy` mountpoint for ZFS so that everything boots in the correct order.

```sh
$ zfs create -o mountpoint=none rpool/root
$ zfs create -o mountpoint=legacy rpool/root/nixos
$ zfs create -o mountpoint=legacy rpool/home
$ zfs create -o mountpoint=legacy rpool/data
```
##### Mount datasets
We need to grab the NixOS installation media and mount it locally, so that Nix can detect it. Additionally, we need to mount our other datasets.

```sh
$ mkdir /mnt
$ mkdir /mnt/home
$ mkdir /mnt/data
$ mount -t zfs rpool/root/nixos /mnt
$ mount -t zfs rpool/home /mnt/home
$ mount -t zfs rpool/data /mnt/data
```

### Generate Nix configuration
/mnt is where we stuck the NixOS installation media after booting from ISO.

```sh
$ nixos-generate-config --root /mnt
```

### Edit final configuration
Open the configuration and add any remaining packages, configuration, etc. 99% of the time Nix doesn't detect everything and I have to add in packages or other services.

Note, hardware-configuration should have all ZFS datasets.

```sh
$ vim /mnt/etc/nixos/configuration.nix /mnt/etc/nixos/hardware-configuration.nix
```

### Finish installation
Install, reboot, and enjoy.
```sh
$ nixos-install
```
