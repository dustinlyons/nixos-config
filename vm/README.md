# Installing NixOS from bare metal or virtual machine

## Overview
The instructions below walk you through building and creating a NixOS install from scratch. Later, we update the machine with all of it's software.

## Steps to install

What are we looking to do?

1. Boot NixOS from an ISO, either VM or USB stick
2. Partition and format your hard drive
3. Mount the new partition
4. Make final edits and install NixOS

## 1. Boot NixOS from an ISO, either VM or USB stick
First, build the image inside `install-media`. From the project root:
```
$ nix-build vm/install/install-media.nix
```
> This assumes you have Nix installed and are targeting a virtual machine. Don't have Nix to run `nix-build`? [Check out the official NixOS ISO.](https://nixos.org/download.html) 

The build produces an ISO. Burn it to USB with your tool of choice, like `dd`. In this example, replace `/dev/sdX` with the linux path to your USB device.
```
$ dd if=result.iso of=/dev/sdX
```
Next, boot the USB and get to a terminal screen to continue on with the steps below.

## 2. Partition and format your hard drive
I use ZFS, but you can just as easily use ```ext3``` with ```fdisk```. Our first step is to just verify we're ready to work. The _install media_ has everything available for these commands by default.

### Verify we see a disk with no partition

```sh
$ lsblk -p
```
> Note: You are logged-in automatically as `nixos`. The `nixos` user account has an empty password so you can use `sudo` without a password.

### Create and format disk partitions
Next, let's create our first partition. Use `sudo` to act as root and bring `sgdisk` into your path. 
> We're running `nix-shell` here which will magically bring in our dependencies.

```sh
$ sudo su -
$ nix-shell -p gptfdisk
```
We want to create a small partition for the MBR and leave the rest to ZFS. Note, I said MBR, _not_ UEFI. I use Proxmox and by default these VMs prefer MBR. You'll also want to change the `/dev/disk/by-id` path to whatever you see when viewing the directory in your local terminal.

```sh
$ sgdisk -a1 -n2:34:2047 -t2:EF02 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
$ sgdisk -n1:0:0 -t1:BF01 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
```

This command is a bit archaic so let me breakdown what we're doing.

* `-a1` means set the "sector alignment" multiple to 1MB. Not sure if this is really needed, as 1MB is the default, but I leave it anyway. It helps to inform older hard drives the size of each sector.
* `-n2:34:2047` and `-n1:0:0` describes a new partition followed by the assigned number, start and end sectors. So in our example, partition 2 starts at sector 34 and ends at 2047. Partition 1 (the MBR) starts at sector 0 and ends at sector 0 (it's 512 bytes). 
* `-t1:BF01` and `-t2:EF02` define the partition's type code. For a full list, sgdisk -L. We use `EF02` (BIOS Boot) and `EF02` (Solaris & Apple ZFS).

> As I mentioned, these VMs use the old BIOS MBR, not UEFI, as Proxmox by default uses SeaBIOS which prefers MBR.
>
> [Learn more](https://pve.proxmox.com/wiki/ZFS_on_Linux) about Proxmox and ZFS.

### Configure ZFS
Okay, we have some empty partitions. What next? Let's create the filesystem, which in our case is ZFS. In practice this means creating a "zpool" and ZFS "datasets", which is just ZFS jargon for the basic "container" of filesystems and the filesystems themselves.

> If you don't want ZFS, the most common linux filesystem is ext3. Use `mkfs -t ext3 /dev/path/to/your/partition`. In this step, we're just looking to get to a working filesystem. So format your drive accordingly. I found [this guide](https://www.computernetworkingnotes.com/linux-tutorials/manage-linux-disk-partition-with-gdisk-command.html) if you're interested. Good luck.

Create a zpool at the root of `/mnt` using the partition we just created. Note, we choose `/mnt` as the root (-R) because by default, NixOS will look for a mounted partition at this location to perform install. Defaults save us time. 

```sh
$ zpool create -O mountpoint=none -O atime=off -O compression=on -O xattr=sa -O acltype=posixacl -R /mnt rpool /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0-part1
```
* `compression=on` lets ZFS choose the best compression available, not always `lz4`. 
* The dataset containing journald’s logs (where /var lives) should have `xattr=sa` and `acltype=posixacl` set to allow regular users to read their journal.
* Nix doesn’t use `atime`, so `atime=off` on the /nix dataset is fine.

> What is a zpool? In ZFS land, the filesystem you use belongs to a zpool. A zpool itself is constructed of virtual devices (named vdevs), and vdevs are  constructed of block devices: files, hard drive partitions, or entire drives.

### Create datasets
Nix requires `legacy` mountpoint for ZFS so that everything boots in the correct order. `legacy` just means we use "legacy" tools `mount` and `umount` (a few steps down).

```sh
$ zfs create -o mountpoint=none rpool/root
$ zfs create -o mountpoint=legacy rpool/root/nixos
$ zfs create -o mountpoint=legacy rpool/home
$ zfs create -o mountpoint=legacy rpool/data
```
## 3. Mount the new partition
### Mount datasets
We need to grab the NixOS installation media and mount it locally, so that Nix can detect it. Additionally, we need to mount our other datasets.

```sh
$ mkdir /mnt
$ mkdir /mnt/home
$ mkdir /mnt/data
$ mount -t zfs rpool/root/nixos /mnt
$ mount -t zfs rpool/home /mnt/home
$ mount -t zfs rpool/data /mnt/data
```

## 4. Make final edits and install NixOS
Generate the configuration at `/mnt`, where the filesystem was mounted for Nix to do it's installation.

```sh
$ nixos-generate-config --root /mnt
```
This creates your `configuration.nix` file that is read by the NixOS installer.

### Edit final configuration
Open the configuration and add any remaining packages, configuration, etc. I add in packages or other services relevant to the machine. [The configs listed in this directory](https://github.com/dustinlyons/nixos-config/tree/main/vm) are what I'm currently using.

> Note: `hardware-configuration.nix` should have all ZFS datasets.

```sh
$ vim /mnt/etc/nixos/configuration.nix /mnt/etc/nixos/hardware-configuration.nix
```

### Finish installation
Install.
```sh
$ nixos-install
```

Reboot and enjoy your new VM. 

For the final steps, I'll usually login to the machine as my user, checkout this repo and create symlinks from a [VM-specific configuration](https://github.com/dustinlyons/nixos-config/tree/main/vm) to the running `configuration.nix` in `/etc/nixos`. Then I'll run my first `nixos-rebuild switch` to install any final packages.
