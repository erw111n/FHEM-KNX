# FHEM-KNX
## This branch is obsolete as the modul 10_KNX.pm is now in the official FHEM SVN !
### How to use: 
To keep your FHEM up-to-date with the new 10_KNX.pm, add following line:

```update add https://raw.githubusercontent.com/erw111n/FHEM-KNX/main/controls_KNXevolution.txt```

to avoid overwriting 10_KNX.pm with the original file from fhem.de, pls. add:

```attr global exclude_from_update fhem.de.*:FHEM/10_KNX.pm```

Details of the update process (e.g. how to remove that stuff again): [FHEM-Wiki](https://wiki.fhem.de/wiki/Update)
