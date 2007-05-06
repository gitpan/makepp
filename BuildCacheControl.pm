# TODO when discontinuing 5.6: Revert to preprocess undoing the 1.5 change.

# $Id: BuildCacheControl.pm,v 1.11 2007/03/15 22:42:23 pfeiffer Exp $

package BuildCacheControl;
use strict;
require Exporter;

our @ISA = 'Exporter';
our @EXPORT = qw(c_clean c_create c_show);

use FileInfo;
use BuildCache;
use FileInfo_makepp;
use Makecmds;
use POSIX;

sub is_special {
  $File::Find::prune =
    $_ eq $FileInfo::build_info_subdir ||
    $_ eq $BuildCache::build_cache_options_file ||
    $_ eq $BuildCache::incoming_subdir;
}

sub c_clean {
  local @ARGV = @_;
  my( $min_atime, $atime, $max_atime,
      $min_mtime, $mtime, $max_mtime,
      $min_inc_mtime, $inc_mtime, $max_inc_mtime,
      $min_ctime, $ctime, $max_ctime,
      $min_size, $size, $max_size,
      $set, $user, $predicate, $weekbase);
  my %unit =
    (s => 1,
     m => 60,
     h => 60 * 60,
     d => 24 * 60 * 60,
     w => 7 * 24 * 60 * 60);
  $unit{''} = $unit{d};
  my $time = time;
  $inc_mtime = '+2h';		# default is 2 hours old or older.

  my ($target_files_deleted, $build_info_files_deleted) = (0, 0);
  my $wanted = sub {
    return if &is_special || $File::Find::dir =~ /\/$FileInfo::build_info_subdir$/;
    my( $nlink, $uid, $gid, $size, $atime, $mtime, $ctime ) = (lstat)[3..5, 7..10];
				# Do this 1st, otherwise -d does a stat.
    defined($nlink) or do {
      my $msg = "stat $_: $!";
      if($! == POSIX::ENOENT || $! == POSIX::ESTALE) {
	warn "$msg\n";
	return;
      }
      die $msg;
    };
    if( -d _ ) {
      for( "$_/$FileInfo::build_info_subdir", $_ ) {
	opendir my( $dh ), $_ or next;
	my $entry;
	$entry =~ /^\.\.?$/ or return while $entry = readdir $dh;
	closedir $dh;
	rmdir or warn "$0: can't delete `$_'--$!\n";
      }
    } elsif( $nlink == 1 ) {	# Clean only files not used elsewhere.
				# There may still be copies though.
      if( $predicate ) {
	my $value = &$predicate;
	goto UNLINK if $value;
	goto SET_USER if defined $value;
      }
      map {
	goto SET_USER if defined $_->[0] && $_->[1] < $_->[0]
	  or defined $_->[2] && $_->[2] < $_->[1]; # Found one that's out of bounds.
      } [$min_atime, $atime, $max_atime],
	[$min_mtime, $mtime, $max_mtime],
	[$min_ctime, $ctime, $max_ctime],
	[$min_size,  $size,  $max_size];
    UNLINK:
      eval { Makecmds::perform { unlink } "delete `$_'" };
      if( $::verbose ) {
	if( $@ ) {
	  warn $@;
	} else {
	  ++$target_files_deleted;
	}
      }
      if(unlink "$FileInfo::build_info_subdir/$_.mk") {
	++$build_info_files_deleted;
      } else {
        warn "unlink $FileInfo::build_info_subdir/$_.mk: $!\n" if $::verbose;
      }
    } else {			# Retained file.
    SET_USER:
      my $have_build_info = -f "$FileInfo::build_info_subdir/$_.mk";
      if( $have_build_info && defined FileInfo::load_build_info_file file_info $File::Find::name ) {
	Makecmds::perform { chown $user, $gid, $_ } "set owner $user for `$_'"
	  if defined $user && $user != $uid;
      } elsif( $time - $mtime > 600 ) {	# Missing or corrupted build info (see BuildCache::fix_ok).
	eval { Makecmds::perform { unlink } "delete `$_'" };
	if( $::verbose ) {
	  if( $@ ) {
	    warn $@;
	  } else {
	    ++$target_files_deleted;
	  }
	}
	++$build_info_files_deleted if $have_build_info; # load_build_info_file wiped it.
      }
    }
  };
  my $clean_incoming = sub {
    return unless -e $_;
    unlink $_ unless -d _ || (stat _)[9] > $max_inc_mtime;
  };

  Makecmds::frame {
    if( $weekbase ) {
      $weekbase = $unit{w};	# 7 days after epoch.
      my( $min, $hour, $wday ) = (localtime $weekbase)[1, 2, 6];
      $weekbase -= --$wday * $unit{d} + $hour * $unit{h} + $min * $unit{m};
				# Count back to monday 0:00.
    }
    map {
      if( defined $_->[1] ) {
	%unit =
	  ('' => 1,
	   c => 1,
	   k => 2 ** 10,
	   M => 2 ** 20,
	   G => 2 ** 30) if $_->[3];
	$set = 1;
	# '+-1' is useful for testing.  We rely on ([-+]?) being ungreedy here.
	$_->[1] =~ /^([-+]?)(\d+(?:\.\d+)?|-1)([wdhmsckMG]?)/ or
	  die "$0: `$_->[1]' is not a valid specification\n";
        # We unlink the ones that are IN the range, so '+' (unlink older than)
        # means to set the max, and '-' (unlink newer than) means to set the
        # min (except that size is opposite).
        if($_->[3]) { # size
	  if( $1 eq '-' ) {
	    ${$_->[2]} = $2 * $unit{$3}; # max
	  } else {
	    ${$_->[0]} = $2 * $unit{$3}; # min
	    ${$_->[2]} = ${$_->[2]} + $unit{$3} if !$1; # range
	  }
        } else { # time
	  if( $1 eq '-' ) {
	    ${$_->[0]} = $time - $2 * $unit{$3}; # min
	  } else {
	    ${$_->[2]} = $time - $2 * $unit{$3}; # max
	    ${$_->[0]} = ${$_->[2]} - $unit{$3} if !$1; # range
	  }
	  if( defined $weekbase ) {
	    defined and
	    $_ -= (int( ($time - $weekbase) / $unit{w} ) - int( ($_ - $weekbase) / $unit{w} )) *
				# Count both weeks since monday after the epoch.
	      2 * $unit{d}	# Subtract number of weeks times 2 days.
	      for ${$_->[0]}, ${$_->[2]};
	  }
        }
      }
    } [\$min_atime, $atime, \$max_atime],
      [\$min_mtime, $mtime, \$max_mtime],
      [\$min_inc_mtime, $inc_mtime, \$max_inc_mtime],
      [\$min_ctime, $ctime, \$max_ctime],
      [\$min_size,  $size,  \$max_size, 1]; # NOTE: $size must be last!
    $set or die "$0: one of --atime, --mtime, --ctime or --size is needed\n";
    @ARGV or die "$0: no build cache directories given\n";
    $min_inc_mtime and die "Minimum incoming mtime not supported\n";

    # Traverse desired filesystems
    require File::Find;
    for(map { "$_/$BuildCache::incoming_subdir" } @ARGV) {
      -e $_ or next;
      File::Find::find({ wanted => $clean_incoming }, $_);
    }
    File::Find::finddepth( { wanted => $wanted },
			   @ARGV );
    print "Deleted $target_files_deleted target files and $build_info_files_deleted build info files.\n"
      if $::verbose;
  } ['a', qr/a(?:ccess[-_]?)?time/, \$atime, 1],
    ['m', qr/m(?:odification[-_]?)?time/, \$mtime, 1],
    ['M', qr/in(?:coming)?[-_]?m(?:odification[-_]?)?time/, \$inc_mtime, 1],
    ['c', qr/c(?:hange[-_]?)?time/, \$ctime, 1],
    ['p', qr/p(?:erl|redicate)/, \$predicate, 1,
     sub { $predicate = Makecmds::eval_or_die( "sub { $predicate }" ) }],
    [qw(s size), \$size, 1],
    ['u', qr/(?:set[-_]?)?user/, \$user, 1,
     sub { defined( $user = getpwnam $user ) or die "$0: user unknown\n" if $user !~ /^\d+$/ }],
    [qw(v verbose), \$::verbose],
    [qw(w workdays), \$weekbase];
}


#
# Create the build cache for the first time.
#
sub c_create {
  local @ARGV = @_;
  my @subdir_chars = (2, 4);    # Set the default directory configuration.
  my( $subdir_chars, $mode );
  Makecmds::frame {
    if( defined $mode ) {
      $mode =~ /^[0-7]+$/ or die "$0: mode `$mode' is not octal\n";
      $mode = oct $mode;
    }
    if( $subdir_chars ) {
      my $last_len = 0;         # Do some quick validation:
      @subdir_chars = map {
        /^\d+$/ or
          die "$0: specify a list of numbers to --subdir-chars\n";
        $_ > $last_len or
          die "$0: parameters to --subdir-chars must be in increasing order\n";
	$last_len = $_;
      } split / *, */, $subdir_chars;
    }
    @ARGV or die "$0: no build cache directories given\n";

    require BuildCache;
    for( @ARGV ) {
      die "$0: `$_' already exists\n"
	if -e;
      Makecmds::perform {
	new BuildCache( $_,
			{ SUBDIR_CHARS => \@subdir_chars,
			  ACCESS_PERMISSIONS => $mode } );
      } "create build cache `$_'";
    }
  } ['m', qr/mode|access[-_]?permisssions/, \$mode, 1],
    ['s', qr/subdir[-_]?chars/, \$subdir_chars, 1];
}


#
# This is a sort of recursive stat command, which takes into account that the
# owner of the cached file may have been changed, while the metadata file
# retains the original owner.
#
sub c_show {
  local @ARGV = @_;
  my $deletable;
  my %user;
  my $time = time;
  my $wanted = sub {
    return if &is_special;
    my( $mode, $nlink, $uid, $size, $atime, $mtime, $ctime ) = (lstat)[2..4, 7..10];
				# Do this 1st, otherwise -d does a stat.
    return if -d _ or
      $deletable && $nlink > 1;
    my $muid = (stat "$FileInfo::build_info_subdir/$_.mk")[4];
    undef $muid if $muid == $uid;
    defined and $_ = $user{$_} ||= getpwuid( $_ ) || $_
      for $uid, $muid;
    $muid &&= " (original: $muid)";
    printf "%s
  owner: %s%s    size: %d    nlink: %d    mode: %3o
  atime: %s
  mtime: %s
  ctime: %s\n", $File::Find::name,
      $uid, $muid || '', $size, $nlink, $mode & 07777,
      map {
	my $res = scalar( localtime $_ ) . '  (';
	$_ = $time - $_;
	$res . int( $_ / (24 * 60 * 60) ) . 'd or ' .
	  int( $_ / (60 * 60) ) . 'h or ' .
	  int( $_ / 60 ) . 'm)';
      } $atime,  $mtime, $ctime;
  };

  Makecmds::frame {
    require File::Find;
    File::Find::find( { wanted => $wanted },
		      @ARGV );
  } 'f', qw(o O),		# fails in 5.6: qw(f o O);
    [qw(d deletable), \$deletable];
}



no warnings 'redefine';
sub ::metahelp { print STDERR <<EOF }
usage:	makepp_build_cache_control command [option ...] directory ...
	makeppbuiltin -MBuildCacheControl command [option ...] directory ...
  available commands:	clean, create, show
  to see options do:	makepp_build_cache_control command --help
EOF

sub ::helpfoot { die <<'EOF' }

Look at @htmldir@/makepp_build_cache.html for more details,
or at http://makepp.sourceforge.net/1.50/makepp_build_cache.html
or type "man makepp_build_cache".
EOF
