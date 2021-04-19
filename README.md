# FHEM-KNX Beta Versions
## the files in this dir are beta versions, not fully tested!!! 
### save yr. environment before downloading, especially fhem.cfg & fhem.save  

##### 00_KNXTUL.pm - patched version - more robust parse function

### How to use: 
To keep your FHEM up-to-date with the new 00_KNXTUL.pm, add following line:

```update add https://raw.githubusercontent.com/erw111n/FHEM-KNX/beta/controls_KNXTUL_beta.txt```

to avoid overwriting 00_KNXTUL.pm with the original file from fhem.de, pls. add:

```attr global exclude_from_update fhem.de.*:FHEM/00_KNXTUL.pm```

Details of the update process (e.g. how to remove that stuff again): [FHEM-Wiki](https://wiki.fhem.de/wiki/Update)
