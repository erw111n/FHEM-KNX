# FHEM-KNX
### The modul 10_KNX.pm in this branch in obsolete as it is now in the official FHEM SVN !
## This branch is now the beta test source of a new IO-module for KNX.
The module is in its early staes of development, especially error handling needs to be improved.
###the following modes of operations are supported:
1)  H - Host Mode - connect to a KNX-router with UDP point-point protocol.
    This is the mode also used by ETS when you specify KNXNET/IP as protocol. You do not need a KNXD installation. The protocol is complex and timing critical!
    If you have delays in FHEM processing close to 1 sec, the protocol may disconnect. It should recover automatically,
    however KNX-messages could have been lost!
2)  M - Multicast mode - connect to KNXD's or KNX-router's multicast-tree.  
    This is the mode also used by ETS when you specify KNXNET/IP Routing as protocol.
    If you have a KNX-router that supports multicast, you do not need a KNXD installation. Default address:port is 224.0.23.12:3671<br/>
    Pls. ensure that you have only <b>one</b> GW/KNXD in your LAN that feed the multicast tree!<br/>
    This mode requires the <code>IO::Socket::Multicast</code> perl-module to be installed on yr. system.
    On Debian systems this can be achieved by <code>apt-get install libio-socket-multicast-perl</code>.
3)  T - TCP Mode - uses a TCP-connection to KNXD (default port: 6720).
    This mode is the successor of the TUL-modul, but does not support direct Serial/USB connection to a TPUart-USB Stick.
    If you want to use a TPUart-USB Stick or any other serial KNX-GW, use either the TUL Module, or connect the USB-Stick to KNXD and in turn use modes M,S or T to connect to KNXD.
4)  S - Socket mode - communicate via KNXD's UNIX-socket on localhost. default Socket-path: <code>/var/run/knxd</code><br/>
    Path might be different, depending on knxd-version or -config specification!
### How to use: 
To keep your FHEM up-to-date with the new 10_KNX.pm, add following line:

```update add https://raw.githubusercontent.com/erw111n/FHEM-KNX/main/controls_KNXIO.txt```

Details of the update process (e.g. how to remove that stuff again): [FHEM-Wiki](https://wiki.fhem.de/wiki/Update)
