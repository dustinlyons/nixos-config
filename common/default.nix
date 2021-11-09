{ config, pkgs, ...}:
{
  networking.extraHosts =
    ''
      192.168.0.67 BRN008077D92A06.local
    '';
}
