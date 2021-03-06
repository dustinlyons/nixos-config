# Install

## Overview
These Nix derivations create the _install media_ used to bootstrap a virtual machine. The instructions below walk you through building and creating a VM from scratch.

## Steps to install

First, build the image with `nix-build install-media.nix`. Then, boot the ISO image in your hypervisor of choice.

For our installation, we need to configure our installation system so that Nix has _just enough_ information to go on, and no more. Nix will detect most settings and do the final heavy lifting.

What does this mean in practice? Basically, we need to ensure our disks are partitioned with the filesystem we want and we need to mount our initial NixOS system. Afterwards, running the installer will copy our Nix media from memory to a durable location, i.e the hard disk.

Don't worry, I go into more detail below.

### Boot the VM
### Partition disks
I use ZFS, but you can just as easily use ```ext4``` with ```fdisk```. Our first step is to just verify we're ready to work.

#### Verify we see a disk with no partition

```sh
$ lsblk -p
```
> Note: You are logged-in automatically as `nixos`. The nixos user account has an empty password so you can use `sudo` without a password.

#### Create and format disk partitions
Next, let's create our first partition. Move over to `sudo` and bring `sgdisk` into your path.

```sh
$ sudo su -
$ nix-shell -p gptfdisk
```
We want to create a small partition for the MBR and leave the rest to ZFS. Note, I said MBR, _not_ UEFI. More later. 

```sh
$ sgdisk -a1 -n2:34:2047 -t2:EF02 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
$ sgdisk -n1:0:0 -t1:BF01 /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0
```

This command is a bit archaic so let me breakdown what we're doing.

* `-a1` means set the "sector alignment" multiple to 1MB. Not sure if this is really needed, as 1MB is the default, but I leave it anyway. It helps to inform older hard drives the size of each sector.
* `-n2:34:2047` and `-n1:0:0` describes a new partition followed by the assigned number, start and end sectors. So in our example, partition 2 starts at sector 34 and ends at 2047. Partition 1 (the MBR) starts at sector 0 and ends at sector 0 (it's 512 bytes). 
* `-t1:BF01` and `-t2:EF02` define the partition's type code. For a full list, sgdisk -L. We use `EF02` (BIOS Boot) and `EF02` (Solaris & Apple ZFS).

As I mentioned, these VMs use the old BIOS MBR, not UEFI. Why? I use Proxmox and by default it prefers virtual machines use SeaBIOS. I like defaults, so I keep it.

[Learn more](https://pve.proxmox.com/wiki/ZFS_on_Linux) about Proxmox and ZFS.

#### Configure ZFS
Okay, we have some empty partitions. What next? Let's create the filesystem, which in our case is ZFS. In practice this means creating a "zpool" and ZFS "datasets", which is just ZFS jargon for the basic "container" of filesystems and the filesystems themselves.

##### Create zpool
Create a zpool at the root of `/mnt` using the partition we just created. 

Note, we choose `/mnt` as the root (-R) because by default, NixOS will look for a mounted partition at this location to perform install. This happens on our final step, `nixos-install`.

```sh
$ zpool create -O mountpoint=none -O atime=off -O compression=on -O xattr=sa -O acltype=posixacl -R /mnt rpool /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0-part1
```
* `compression=on` lets ZFS choose the best compression available, not always `lz4`. 
* The dataset containing journald???s logs (where /var lives) should have `xattr=sa` and `acltype=posixacl` set to allow regular users to read their journal.
* Nix doesn???t use `atime`, so `atime=off` on the /nix dataset is fine.

##### Create datasets
Nix requires `legacy` mountpoint for ZFS so that everything boots in the correct order. `legacy` just means we use "legacy" tools `mount` and `umount` (a few steps down).

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
Generate the configuration at `/mnt`, where the filesystem was mounted for Nix to do it's installation.

```sh
$ nixos-generate-config --root /mnt
```

### Edit final configuration
Open the configuration and add any remaining packages, configuration, etc. 99% of the time Nix doesn't detect everything and I have to add in packages or other services.

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
