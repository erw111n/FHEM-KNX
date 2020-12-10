# FHEM-KNX
## This directory contains updated code for FHEM, especially for 10_KNX.pm module
and/or patches, plus a few utilities.
### How to use: 
To keep your FHEM up-to-date with the new 10_KNX.pm, add following line:

```update add https://raw.githubusercontent.com/erw111n/FHEM-KNX/main/controls_KNXevolution.txt```

to avoid overwriting 10_KNX.pm with the original file from fhem.de, pls. add:

```attr global exclude_from_update fhem.de.*:FHEM/10_KNX.pm```

Details of the update process (e.g. how to remove that stuff again): [FHEM-Wiki](https://wiki.fhem.de/wiki/Update) ```
