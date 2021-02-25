##############################################
#$Id: 00_KNXTUL.pm 20852 2019-12-30 10:43:45Z jewuma $
#31.01.2019: Checked Message Format to prevent FHEM-Crash
#13.03.2019: Let only normal (no System-Messages) pass to prevent Creating Fake-Devices
## MH  2021-01-28 rework KNXTUL_Parse - check received messages integrity   
## MH  2021-02-03 fixed error in read (all incoming msgs marked as read....)

package main;

use strict;
use warnings;
use Time::HiRes qw(gettimeofday);
use IO::Socket::INET;

my $KNXTUL_hasMulticast = 1;

sub KNXTUL_Attr(@);
sub KNXTUL_Clear($);
sub KNXTUL_Parse($$$$$);
sub KNXTUL_Read($);
sub KNXTUL_Ready($);
sub KNXTUL_Write($$$);

sub KNXTUL_OpenDev($$);
sub KNXTUL_CloseDev($);
sub KNXTUL_Disconnected($);
sub KNXTUL_Shutdown($);

my %gets = (    # Name, Data to send to the TUL, Regexp for the answer
  "raw"      => ["r", '.*'],
);

my %sets = (
  "raw"       => "",
);

sub KNXTUL_Initialize($)
{
	my ($hash) = @_;
	eval("use IO::Socket::Multicast");
	$KNXTUL_hasMulticast = 0 if($@);

	# Provider
	$hash->{ReadFn}  = "KNXTUL_Read";
	$hash->{WriteFn} = "KNXTUL_Write";
	$hash->{ReadyFn} = "KNXTUL_Ready";

	# Normal devices
	$hash->{DefFn}   = "KNXTUL_Define";
	$hash->{UndefFn} = "KNXTUL_Undef";
	$hash->{StateFn} = "KNXTUL_SetState";
	$hash->{AttrFn}  = "KNXTUL_Attr";

	$hash->{AttrList}= "do_not_notify:1,0 " .
					   "dummy:1,0 " .
					   "showtime:1,0 " .
					   "verbose:0,1,2,3,4,5 ";
	$hash->{ShutdownFn} = "KNXTUL_Shutdown";
	$hash->{Clients} = "KNX";
	$hash->{MatchList} = { "1:KNX" => "^C.*" };
}

#####################################
sub KNXTUL_Define($$)
{
	my ($hash, $def) = @_;
	my @a = split("[ \t][ \t]*", $def);
  my $argcount=scalar(@a);
	if($argcount < 3)
	{
		my $msg = "wrong syntax: define <name> KNXTUL <knx addr>";
		return $msg;
	}
  	return "install IO::Socket::Multicast to use KNXTUL" if(!$KNXTUL_hasMulticast);
  	$hash->{"HAS_IO::Socket::Multicast"} = $KNXTUL_hasMulticast;

	KNXTUL_CloseDev($hash);

	my $name = $a[0];
	my $devaddr = KNXTUL_str2hex($a[2]);
	$hash->{DeviceAddress} = $devaddr;
  if ($argcount<4) {
	  $hash->{IPAddress} = "224.0.23.12";
    $hash->{UseDirectConnection}=0;
  } else {
    $hash->{IPAddress}= $a[3];
    $hash->{UseDirectConnection}=1;
  }
	$hash->{Port} = 3671;
#	$hash->{Clients} = "KNX";

	my $ret = KNXTUL_OpenDev($hash, 0);
	return $ret;
}

########################
sub KNXTUL_OpenDev($$)
{
	my ($hash, $reopen) = @_;
	my $name = $hash->{NAME};
	my $host = $hash->{IPAddress};
	my $port = $hash->{Port};
  my $UseDirectConnection = $hash->{UseDirectConnection};
	$hash->{PARTIAL} = "";
	Log 3, "KNXTUL opening $name" if(!$reopen);

	# This part is called every time the timeout (5sec) is expired _OR_
	# somebody is communicating over another TCP connection. As the connect
	# for non-existent devices has a delay of 3 sec, we are sitting all the
	# time in this connect. NEXT_OPEN tries to avoid this problem.
	return if($hash->{NEXT_OPEN} && time() < $hash->{NEXT_OPEN});
  my $conn=0;
  if ($UseDirectConnection) {
    $conn = new IO::Socket::INET(PeerHost => $host,PeerPort=>$port,Proto=>'udp') or Log3($name,0,"Connection to ".$host." can't be established");
  } else {
    $conn = IO::Socket::Multicast->new(Proto=>'udp',LocalPort=>$port,LocalAddr=>$host,ReuseAddr=>1);
    $conn->mcast_add($host) || Log3 ($name, 3,"Can't set group: $host");
    $conn->mcast_loopback(0); ##MH
    $conn->mcast_dest($host.":".$port);
  }
	if($conn)
	{
		delete($hash->{NEXT_OPEN})
	}
	else
	{
		Log3 ($name, 3, "Can't connect: $!") if(!$reopen);
		$readyfnlist{"$name"} = $hash;
		$hash->{STATE} = "disconnected";
		$hash->{NEXT_OPEN} = time()+60;
		return "";
	}
	$hash->{CD} = $conn;
  $hash->{FD} = $conn->fileno();
	delete($readyfnlist{"$name"});
	$selectlist{"$name"} = $hash;
	if($reopen)
	{
		Log3 ($name, 1, "KNXTUL reappeared ($name)");
	}
	else
	{
		Log3 ($name, 3, "KNXTUL device opened");
	}

	$hash->{STATE}="";       # Allow InitDev to set the state
	my $ret  = KNXTUL_DoInit($hash);

	if($ret)
	{
		KNXTUL_CloseDev($hash);
		Log (1, "OpenDev: Cannot init KNXTUL-Device, ignoring it");
	}

	DoTrigger($name, "CONNECTED") if($reopen);
	return $ret;
}

#####################################
# called from the global loop, when the select for hash->{FD} reports data
sub KNXTUL_Read($)
{
	my ($hash) = shift;
	my $name = $hash->{NAME};

	my $buf = q{};

	Log3($name,5,'KNXTUL_read: started');
	my $len=$hash->{CD}->recv($buf, 1024);

	if( !defined($len) || !$len ) {
		Log3($name,1, 'KNXTUL_read: no data - disconnect');
		KNXTUL_Disconnected($hash);
		return '';
	}
	$buf = $hash->{CHUNK} . $buf if (defined($hash->{CHUNK}));

	if (length($buf) < 6) { # min required for first unpack
		$hash->{CHUNK} = $buf;
		return '';
	}

	# header format: 0x06 - header size / 0x10 - KNXNET-IPVersion / 0x0530 - Routing Indicator / 0xYYYY - Header size + size of cEMIFrame 
	my ($header_size, $header_version, $header_routingI, $total_length) = unpack("ccnn",$buf);

	Log3($name,5, 'KNXTUL_read: header -size= ' . sprintf("%02x",$header_size) . ', -version= ' . sprintf("%02x",$header_version) . ', -routing= ' . sprintf("%04x",$header_routingI) . ", TotalLength= $total_length \(dezimal\)" ); 

	if (($header_size != 0x06 || $header_version != 0x10 ) ) {
		Log3($name,4, 'KNXTUL_read: invalid header size or version');
		$hash->{CHUNK} = undef; # delete all we have so far 
#		KNXTUL_Disconnected($hash); #?
		return '';
	}

	if (length($buf) < $total_length) {  #  6 Byte header + min 11 Byte data
		Log3($name,4, 'KNXTUL_read: still waiting for complete packet (short packet length)');
		$hash->{CHUNK} = $buf; # still not enough
		return '';
	}
	else {
		$hash->{CHUNK} = substr($buf,$total_length);
		$buf = substr($buf,0,$total_length);
	}

	##### now, the buf is complete check if routing-Frame 
	if (($header_routingI == 0x0530) && ($total_length >= 17)) {  #  6 Byte header + min 11 Byte data 
		# this is the correct frame type, continue
	}
	elsif ($header_routingI == 0x0531) {
		# routing Lost Message
		Log3($name,3, 'KNXTUL_read: a routing-lost packet was received !!! - Problems with bus or KNX-router ???');
		#shall we set refused here ? 
		return '';
	}
	else {
		Log3($name,4, 'KNXTUL_read: a packet with unsupported service type ' . sprintf("%04x",$header_routingI) . ' was received. - discarded');
		return '';
	}

	Log3($name,5,'KNXTUL RawBuf read: ' . unpack('H*',$buf));

### from here on, it should be in KNXTUL_Parse...
###	$buf = substr($buf,$header_size,$total_length);
###	KNXTUL_Parse($hash,$buf);

	my ($idval, $ctrlbyte1, $ctrlbyte2, $src, $dst, $tcf, $acpi, @data) = unpack("x" . $header_size . "nCCnnCCC*",$buf);
	Log3($name,5,"KNXTUL_read: frame -idval= " . sprintf("%04x",$idval) . ", ctrl1= " . sprintf("%02x",$ctrlbyte1) . ", ctrl2= " . sprintf("%02x",$ctrlbyte2) .", src= " . sprintf("%04x",$src) .", dst= " . sprintf("%04x",$dst) . ", tcf= " . sprintf("%02x",$tcf) . ", acpi= " . sprintf("%02x",$acpi) . ", data= " . sprintf('%02x' x scalar(@data),@data) );  

	if ($idval != 0x2900) { 
		Log3($name,1,'KNXTUL_read: wrong idval ' . sprintf("%04x",$idval) . ', discard packet');
		return '';
	}

	if (($ctrlbyte1 & 0xF0) != 0xB0) { # standard frame/no repeat/broadcast - see 03_06_03 EMI_IMI specs
#	if ($ctrlbyte1 != 0xBC) { # shall we check on 0x80 only ? (standard frame) see 03_06_03 EMI_IMI specs
		Log3($name,1,'KNXTUL_read: wrong ctrlbyte1' . sprintf("%02x",$ctrlbyte1)  . ', discard packet');
		return '';
	}

	my $prio = ($ctrlbyte1 & 0x0C) >>2; # priority

	my $dest_addrType = ($ctrlbyte2 & 0x80) >> 7; # MSB  0 = indiv / 1 = group 
	my $hop_count = ($ctrlbyte2 & 0x70) >> 4; # bits 6-4      

	my $leng = $tcf; # number of NPDU octets, TPCI octet not included!
#	my $leng = ($tcf & 0x0F); #?
#	my $isGAaddr = ($tcf & 0x80) >> 7; #?
#	my $RTcount = ($tcf & 0x70) >> 4; #?

	$src = KNXTUL_addr2hex($src,0); # always a phy-address
	$dst = KNXTUL_addr2hex($dst,$dest_addrType);

	my $acpi1 = ($data[0] & 0xC0) >> 6;
	$acpi = ((($acpi & 0x03) << 2) | $acpi1);
#	$acpi = ((($acpi & 0x03) << 2) | (($data[0] & 0xC0) >> 6));

	my @acpicodes = ('read','preply','write','invalid');
	my $rwp = $acpicodes[$acpi1];
#	my $rwp = $acpicodes[$acpi];
	if ((! defined($rwp)) || ($rwp eq 'invalid')) { 
		Log3($name,1,'KNXTUL_read: no valid acpi-code (read/reply/write) received, discard packet');
		return '';
	}

	Log3 ($name,4,"KNXTUL_read: src=$src - dst=$dst - destaddrType=$dest_addrType  - prio=$prio - hop_count=$hop_count - leng=$leng/" . scalar(@data) . " - R/W=$rwp - acpi=$acpi - data=" . sprintf('%02x' x scalar(@data),@data) );

	if ($leng != scalar(@data)) {
		Log3($name,1, 'KNXTUL_read: Datalength not consistent');
		return '';
	}

	if ( scalar(@data) > 1 ) {
		shift @data; # byte 0 is ununsed if length > 1
	}
	else {
		$data[0] = ($data[0] & 0x3f); # 6 bit data
	}

	my $outbuf = "C" . $src . substr($rwp,0,1) . $dst . sprintf('%02x' x scalar(@data),@data); 

###  not so nice alternative for above
#	foreach my $c (@data) {
#		$outbuf .= sprintf ("%02x", $c);
#	}

	#place KNX-Message
	Log3 $name, 5, "KNXTUL_Read outbuf: $outbuf";
	KNXTUL_Parse($hash, $hash, $name, $outbuf, $hash->{initString});
	return '';
}

#####################################
sub KNXTUL_Undef($$)
{
	my ($hash, $arg) = @_;
	my $name = $hash->{NAME};

	foreach my $d (sort keys %defs)
	{
		if(defined($defs{$d}) && defined($defs{$d}{IODev}) && $defs{$d}{IODev} == $hash)
		{
			my $lev = ($reread_active ? 4 : 2);
			Log (GetLogLevel($name,$lev), "deleting port for $d");
			delete $defs{$d}{IODev};
		}
	}

	KNXTUL_CloseDev($hash);
	return undef;
}

#####################################
sub KNXTUL_Shutdown($)
{
	my ($hash) = @_;
	KNXTUL_CloseDev($hash);
	return undef;
}

#####################################
sub KNXTUL_SetState($$$$)
{
	my ($hash, $tim, $vt, $val) = @_;
	return undef;
}

sub KNXTUL_Clear($)
{
	my $hash = shift;

	#Clear the pipe
	#TUL has no pipe....
}

#####################################
sub KNXTUL_DoInit($)
{
	my $hash = shift;
	my $name = $hash->{NAME};
	my $err;

	KNXTUL_Clear($hash);

	$hash->{STATE} = "Initialized" if(!$hash->{STATE});

	# Reset the counter
	delete($hash->{XMIT_TIME});
	delete($hash->{NR_CMD_LAST_H});
	return undef;
}

#####################################
sub KNXTUL_Parse($$$$$)
{
	my ($hash, $iohash, $name, $rmsg, $initstr) = @_;

	# there is nothing specal to do at the moment.
	# just dispatch

	my $dmsg = $rmsg;
	$hash->{"${name}_MSGCNT"}++;
	$hash->{"${name}_TIME"} = TimeNow();
	$hash->{RAWMSG} = $rmsg;
	my %addvals = (RAWMSG => $rmsg);

	Dispatch($hash, $dmsg, \%addvals);
#	Dispatch($hash, $dmsg, \%addvals,1); #MH dont Log "unknown code xxx pls help me"
}


#####################################
sub KNXTUL_Ready($)
{
	my ($hash) = @_;
	return KNXTUL_OpenDev($hash, 1) if($hash->{STATE} eq "disconnected");
}

########################
sub KNXTUL_Write($$$)
{
	my ($hash,$fn,$msg) = @_;
	return if(!$hash);
	my $name = $hash->{NAME};
  Log3($name,5,"KNXTUL - Write started");
	return if(!defined($fn));

	# Discard message if TUL is disconnected
	return if($hash->{STATE} eq "disconnected");

	Log3 ($name, 5, "KNXTUL: sending $fn $msg");
	$msg = "$fn$msg";

	# Msg must have the format B(w,r,p)g1g2g3v....
	# w-> write, r-> read, p-> reply
	# g1,g2,g3 are the hex parts of the group name
	# v is a simple (1 Byte) or complex value (n bytes)
	if ($msg =~ /^[BC](.)(.{5})(.*)$/)
	{
		my $eibmsg;
		if($1 eq "w")
		{
			$eibmsg->{'type'} = 'write';
		}
		elsif ($1 eq "r")
		{
			$eibmsg->{'type'} = 'read';
		}
		elsif ($1 eq "p")
		{
			$eibmsg->{'type'} = 'reply';
		}

		$eibmsg->{'dst'} = $2;
		my $hexvalues = $3;

		#The array has to have a given length. During Hex-conversion Trailing
		#0 are recognizes for warnings.
		#Therefore we backup the length, trim, and reappend the 0
		#
		#save length and trim right side
		my $strLen = length ($hexvalues) / 2;
		$hexvalues =~ s/\s+$//;
		#convert hex-string to array with dezimal values
		my @data =  map hex($_), $hexvalues =~ /(..)/g;
		#re-append 0x00
		for (my $i=0; $strLen - scalar @data; $i++)
		{
			push (@data, 0);
		}

		# check: first byte is only allowed to contain data in the lower 6bits
		#        to make sure all is fine, we mask the first byte
		$data[0] = $data[0] & 0x3f if(defined($data[0]));

		$eibmsg->{'data'} = \@data;

		KNXTUL_sendGroup($hash, $eibmsg);
	}
	else
	{
		Log3 ($hash->{NAME}, 1,"Could not parse message $msg");
		return undef;
	}

	select(undef, undef, undef, 0.001);
}

sub KNXTUL_sendGroup($$)
{
    my ($hash,$msgref) = @_;
    my $dst = $msgref->{'dst'};
    my $src = $hash->{DeviceAddress};
    $msgref->{'src'} = $src;
	my @encmsg = KNXTUL_encode_eibd($hash,$msgref);

	Log3($hash->{NAME},5,"KNXTUL_sendGroup: dst: $dst, msg: @encmsg \n");

	my $str=pack("nCC*", @encmsg);
  	my $host = $hash->{IPAddress};
  	my $port = $hash->{Port};
  	my $size = length($str);
	my $completemsg=pack("H*","06100530").pack("n",$size+12).pack("H*","2900BCD0").pack("n",$size).$str;
  	Log3 ($hash->{NAME},5,"KNXTUL_sendRequest: ".$host.":".$port." msg: ".unpack("H*",$completemsg). "\n");
  	return undef unless $hash->{CD}->mcast_send($completemsg,$host.":".$port);
    return 1;
}


########################
sub KNXTUL_CloseDev($)
{
	my ($hash) = @_;
	my $name = $hash->{NAME};
	my $dev = $hash->{DeviceName};

	return if(!$dev);

	if($hash->{FD})
	{
		$hash->{FD}->close();
		delete($hash->{FD});
	}
	elsif($hash->{USBDev})
	{
		$hash->{USBDev}->close() ;
		delete($hash->{USBDev});
	}

	delete($selectlist{"$name.$dev"});
	delete($readyfnlist{"$name.$dev"});
	delete($hash->{FD});
}


########################
sub KNXTUL_Disconnected($)
{
	my $hash = shift;
	my $name = $hash->{NAME};

	return if(!defined($hash->{FD}));                 # Already deleted or RFR

	Log3 ($name, 1, "KNXTUL disconnected, waiting to reappear");
	KNXTUL_CloseDev($hash);
	$readyfnlist{"$name"} = $hash;               # Start polling
	$hash->{STATE} = "disconnected";

	# Without the following sleep the open of the device causes a SIGSEGV,
	# and following opens block infinitely. Only a reboot helps.
	sleep(5);

	DoTrigger($name, "DISCONNECTED");
}

########################
sub KNXTUL_Attr(@)
{
	my ($cmd,$name,$aName,$aVal) = @_;
	$aVal = 'undefined' if (! defined($aVal));
	Log3 ($name, 5, "changing value, ATTR: $aName, VALUE: $aVal");

	return undef;
}

# Utility functions
sub KNXTUL_hex2addr
{
	my $str = lc($_[0]);
	if ($str =~ /([0-9a-f]{2})([0-9a-f])([0-9a-f]{2})/)
	{
        return (hex($1) << 11) | (hex($2) << 8) | hex($3);
    }
    else
    {
		return;
    }
}

sub KNXTUL_addr2hex
{
	my $a = $_[0];
	my $b = $_[1];  # 1 if local (group) address, else physical address
    my $str ;

    if ($b == 1)
	{
		#logical address used
		$str = sprintf "%02x%01x%02x", ($a >> 11) & 0x1f, ($a >> 8) & 0x7, $a & 0xff;
    }
    else
	{
		$str = sprintf "%02x%01x%02x", $a >> 12, ($a >> 8) & 0xf, $a & 0xff;
    }

    return $str;
}

sub KNXTUL_str2hex
{
	my $str = $_[0];
	my $hex;

    if (($str =~ /(\d+)\/(\d+)\/(\d+)/) or ($str =~ /(\d+)\.(\d+)\.(\d+)/))
	{
		# logical address
		$hex = sprintf("%02x%01x%02x",$1,$2,$3);
		return $hex;
    }
}

# For mapping between APCI symbols and values
my @apcicodes = ('read', 'reply', 'write');
my %apcivalues = ('read' => 0, 'reply' => 1, 'write' => 2,);

# decode: unmarshall a string with an EIB message into a hash
# The hash has the follwing fields:
#	- type: APCI (symbolic value)
#	- src: source address
#	- dst: destiniation address
#	- data: array of integers; one for each byte of data
sub KNXTUL_decode_eibd($)
{
    my ($buf) = @_;
    my $drl = 0xe1; # dummy value
    my %msg;
    my @data;
    my ($src, $dst,$bytes) = unpack("nnxa*", $buf);
    my $apci;

    $apci = vec($bytes, 3, 2);
	# mask out apci bits, so we can use the whole byte as data:
    vec($bytes, 3, 2) = 0;
    if ($apci >= 0 && $apci <= $#apcicodes)
	{
		$msg{'type'} = $apcicodes[$apci];
    }
    else
	{
		$msg{'type'} = 'apci ' . $apci;
    }
    $msg{'src'} = KNXTUL_addr2hex($src,0);
    $msg{'dst'} = KNXTUL_addr2hex($dst,1);

    @data = unpack ("C" . length($bytes), $bytes);
    my $datalen = @data;
    Log (5, "KNXTUL_decode_eibd: byte len: " . length($bytes) . " array size: $datalen");

    # in case of data len > 1, the first byte (the one with apci) seems not to be used
    # and only the following byte are of interest.
    if($datalen>1)
	{
    	shift @data;
    }

    $msg{'data'} = \@data;
    return \%msg;
}

# encode: marshall a hash into a EIB message string
sub KNXTUL_encode_eibd($$)
{
    my ($hash,$mref) = @_;
    my @msg;
    my $APCI;
    my @data;

    $APCI = $apcivalues{$mref->{'type'}};
    if (!(defined $APCI))
	{
		Log3($hash->{NAME},3,"KNXTUL_encode_eibd: Bad KNX message type $mref->{'type'}\n");
		return;
    }
    @data = @{$mref->{'data'}};

    @data = (0x0) if(!@data || !defined($data[0])); #make sure data has at least one element
    my $datalen = @data;
    Log3 ($hash->{NAME},5,"KNXTUL_encode_eibd: dst: $mref->{'dst'} apci: $APCI datalen: $datalen data: @data");
    @msg = (
	    KNXTUL_hex2addr( $mref->{'dst'}), 	# Destination address
		$datalen,
	    0x0 | ($APCI >> 2), 	# TPDU type, Sequence no, APCI (msb)
	    (($APCI & 0x3) << 6) | $data[0],
	    );
    if ($datalen > 1)
	{
    	shift(@data);
		push @msg, @data;
    }
    return @msg;
}



1;

=pod
=begin html

<a name="KNXTUL"></a>
<h3>KNXTUL</h3>
<ul>
  The KNXTUL module is the representation of a EIB / KNX connector in FHEM without the need for knxd or similar.
  <a href="#KNX">KNX</a> instances represent the EIB / KNX devices and will need a KNXTUL as IODev to communicate with the EIB / KNX network.<br>
  The TUL module is designed to connect to KNX network via Standard Multicast Address (224.0.23.12)
  Note: This module requires Perl Module IO::Socket::Multicast which needs to be installed before use.

  <a name="KNXTULdefine"></a>
  <b>Define</b>
  <ul>
    <code>define &lt;name&gt; KNXTUL &lt;physical address&gt;</code> <br>
    <br>
	<ul>
    <br><br>
	  Example:<br>
	  <code>define tul KNXTUL 1.1.249</code>
    </ul>
    <br>
    The physical address is used as the source address of telegrams sent to EIB network.
  </ul>
  <br>

  <a name="TULattr"></a>
  <b>Attributes</b>
  <ul>
    <li><a href="#do_not_notify">do_not_notify</a></li><br>
    <li><a href="#attrdummy">dummy</a></li><br>
    <li><a href="#showtime">showtime</a></li><br>
    <li><a href="#verbose">verbose</a></li><br>
  </ul>
  <br>
</ul>

=end html
=device
=item summary Connects FHEM to KNX-Bus (Base-device)
=item summary_DE Verbindet FHEM mit dem KNX-Bus (Basisger&umlat)
=begin html_DE

<a name="KNXTUL"></a>
<h3>KNXTUL</h3>
<ul>

  Das Modul KNXTUL stellt die Verbindung von FHEM zu KNX dar.
  <a href="#KNX">KNX</a> Instanzen stellen die Vrbindung zu den KNX-Gruppen dar und ben&ouml;tigen ein TUL-Device als IO-Schnittstelle.<br>
  Das Modul KNXTUL kommuniziert mit dem KNX über die KNX-Multicast-Adresse 224.0.23.12. Voraussetzung ist ein KNX/IP-Router im selben Netztwerk wie die FHEM-Instanz.

  Anmerkung: das Modul ben&ouml;tigt das Perl Modul IO::Socket::Multicast.
  <a name="KNXTULdefine"></a>
  <b>Define</b>
  <ul>
    <code>define &lt;name&gt; KNXTUL &lt;Physikalische KNX-Adresse&gt;</code> <br>
    <br>
	  Beispiel:<br>
	  <code>define tul KNXTUL 1.1.249</code>
    <br>
	Die physikalische Adresse wird als Absender f&uuml;r KNX-Telegramme genutzt.
  </ul>
  <br>

  <a name="TULattr"></a>
  <b>Attribute</b>
  <ul>
    <li><a href="#do_not_notify">do_not_notify</a></li><br>
    <li><a href="#attrdummy">dummy</a></li><br>
    <li><a href="#showtime">showtime</a></li><br>
    <li><a href="#verbose">verbose</a></li><br>
  </ul>
  <br>
</ul>

=end html_DE

=cut
