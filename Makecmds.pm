# $Id: Makecmds.pm,v 1.45 2007/04/27 20:08:26 pfeiffer Exp $
###############################################################################
#
# This package contains builtin commands which can be called from a rule.
#
# It may be copied only under the terms of either the Artistic License or the
# GNU General Public License, which may be found in the Perl 5 source kit.
#


# TODO: autoload these commands only when needed
# Why don't <> commands report unreadable files???
# Use file_info -- or don't, because we'd have to refresh it before every new cmd.

package Makesubs;		# Make these importable.

use strict;
use TextSubs ();
use FileInfo ();
use Makesubs ();
use POSIX ();

#
# Execute an external Perl script within the running interpreter.
#
sub run(@) {
  local( $0, @ARGV ) = @_;		# Name the arguments.
  $0 = Makesubs::f_find_program $0,
    $Makesubs::rule ? $Makesubs::rule->{MAKEFILE} : $Makesubs::makefile,
    $Makesubs::rule ? $Makesubs::rule->{RULE_SOURCE} : $Makesubs::makefile_line
    unless -f $0;		# not relative or absolute
  local $SIG{__WARN__} = local $SIG{__DIE__} = 'DEFAULT';
  die $@ || "$0 failed--$!\n"
    if !defined do $0 and $@ || $!;
}

#
# Execute an external Perl script within a child of the running interpreter.
#
sub run_forked(@) {
  warn "run_forked is deprecated, run script as external action\n";
  if( !::is_windows() || $^O !~ /^MSWin/ ) {
    my $pid = fork;
    if( $pid == -1 ) {
      die "could not fork: $!\n";
    } elsif( $pid ) {
      waitpid $pid, 0;
    } else {
      &run;
      close STDOUT; close STDERR;
      POSIX::_exit 0;		# We didn't die.
    }
  } else {
    system ::PERL, '-S', @_
      and die 'system ', ::PERL, "@_ failed: $?";
  }
}



# builtin commands
package Makecmds;

sub eval_or_die($) {
  $File::maybe_open++;
  Makesubs::eval_or_die $_[0], $Makesubs::rule->{MAKEFILE}, $Makesubs::rule->{RULE_SOURCE};
}


# perform one step and log or die as appropriate
our( $install_date, $install_log );
sub perform(&$;$) {
  if( eval { &{$_[0]} } && !$@ ) {
    print STDERR "$0: $_[1]\n" if $::verbose;
    if( $install_log ) {
      my $msg = $_[1];
      my $cwd = FileInfo::absolute_filename( $FileInfo::CWD_INFO );
      $msg =~ s|`(?=[^/`][^`]*$)|`$cwd/|;
      if( $MakeEvent::max_proc > 1 ) {
	flock $install_log, 2;	# Lock exclusive.
	if( $install_date =~ /^ / ) { # Additional output
	  if( (stat $install_log)[7] > tell $install_log ) {
				# Size changed by concurrent proc
	    seek $install_log, 0, 2;
	    print $install_log "$install_date  $msg\n" or die $!;
	  } else {
	    print $install_log "  $msg\n" or die $!;
	  }
	} else {
	  seek $install_log, 0, 2;
	  print $install_log "$install_date  $msg\n" or die $!;
	  $install_date = "  $install_date";
	}
	flock $install_log, 8;	# Unlock.
      } else {
	print $install_log "$install_date  $msg\n" or die $!;
      }
      $install_date = '';
    }
    1;
  } elsif( !$_[2] ) {
    die "could not $_[1]--". ($@ || $!) . "\n";
  }
}


# deletion files first, then directories depth first
sub _rm {
  my @dirs;
  perform { unlink } "delete `$_'"
    for grep { -l || !-d _ ? $_ : !push @dirs, $_ } @_;
  perform { rmdir } "delete directory `$_'"
    for sort { my( $A, $B ) = ($a, $b); ($B =~ tr:/::d) <=> ($A =~ tr:/::d) } @dirs;
}


# Explicit variables for options shared by several commands.
my( $noescape, $force, $inpipe );


# Frame which handles options, I/O and adds omnipresent --verbose.
sub frame(&@) {
  my( $code, @opts, @stdopts ) = @_;
  local $::verbose = $::verbose;

  my( $args, $infail, $inout, $output, $pipe, $select, $outfail, $separator );
  # Standard options shared by several commands.
  my %opt =
    (E => ['E', qr/no[-_]?escape/, \$noescape],
     f => [qw(f force), \$force],
     i => [qw(i inpipe), \$inpipe, 1],
     I => [qw(I infail), \$infail],
     o => [qw(o output), \$output, 1],
     O => [qw(O outfail), \$outfail],
     r => ['r', qr/record[-_]?size/, \$separator, 1, sub { $separator = eval "\\$separator" }],
     s => [qw(s separator), \$separator, 1]);

  @opts = grep {		# Put stds last, so they don't override short opts.
    ref or !push @stdopts, $opt{$_};
  } @opts;
  TextSubs::getopts @opts, @stdopts,
    ['A', qr/arg(?:ument)?s?[-_]?file/, \$args, 1,
     sub {
       open my $fh, $args or die "$0: cannot open args-file `$args'--$!\n";
       unshift @ARGV, map { chomp; $_ } <$fh>;
     }],
    [qw(v verbose), \$::verbose];

  # Setup input context.
  local $/ = $separator if defined $separator;
  #local *STDIN if $inpipe or $output && $output =~ /^\+</; # Causes input to hang :-(
  if( $inpipe ) {
    $inpipe =~ s/\|$//s;
    perform { open STDIN, '-|', $inpipe } "call `$inpipe|'";
  }

  # Setup output context.
  local *STDOUT if $output;
  if( $output ) {
    my $first = ord $output;
    $pipe = $first == ord '|';
    if( $pipe or $first != ord '+' or $output !~ /^\+</ ) {
      if( !$pipe and $force ) {
	my $file = $output;
	$file =~ s/^>\s*//;
	_rm $file if -e $file;
      }
      perform { open STDOUT, ($pipe or $first == ord '>') ? $output : ">$output" } "write `$output'";
    } else {
      $output =~ s/^\+<((.*?)[^\/]+)$/$1/;
      # open temp file in same dir as output, so we can rename later
      $inout = "$2.makepp.$$" . substr rand, 1;
      perform { open STDIN, $output and open STDOUT, '>', $inout }
	"read `$output' and write to tempfile";
    }
    $select = select STDOUT;
  }
  local $SIG{PIPE} = 'IGNORE' if $pipe;

  my @res = eval { &$code };
  my $err = $@;

  # throw the proper diagnostics as to why close failed
  sub _closedie($) {
    die "`$_[0]' ",
      $! ? "failed to close--$!" :
	($? & 127) ? "died with signal " . ($? & 127) :
	"exited with value " . ($? >> 8);
  }

  # Cleanup if any I/O is open.
  if( $inpipe && $infail or $inout ) {
    close STDIN or _closedie $inpipe;
    open STDIN, '/dev/null';	# placeholder file handle as long as not local
  }
  if( $output ) {
    close STDOUT or $outfail && _closedie $output;
    perform { rename $inout, $output } "rename tempfile to `$output'" if $inout;
    select $select;
  }
  undef ${$$_[2]} for @stdopts;
  die $err if $err;
  @res;
}


# variant that returns what it chomped
sub _chomp(\$) {
  return '' if ref $/;
  my $tail = ${$_[0]};
  my $len = chomp ${$_[0]};
  return '' if !$len;
  substr $tail, -$len;
}


# quoting as in double quoted strings, but without var interpolation
sub _escape(\$) {
  return if $noescape;
  for( ${$_[0]} ) {
    s/([\$\@!])/\\$1/g; # protect variable chars and next line's quote
    $_ = eval "qq!$_!";
    die $@ if $@;
  }
}



sub c_cat {
  local @ARGV = @_;
  frame {
    require File::Copy;
    perform { File::Copy::copy( $_, \*STDOUT ) } "copy `$_'"
      for @ARGV;
  } 'f', qw(i I o O); # fails in 5.6: qw(f i I o O);
}


sub c_chmod {
  local @ARGV = @_;
  frame {
    my $mode = oct shift @ARGV;
    perform { chmod $mode, $_ } "set mode for `$_'"
      for @ARGV;
  };
}



sub c_cp {
  my $mv;	     # must separate my from if, or it survives multiple calls
  local @ARGV = @_;
  $mv = shift @ARGV if ref $_[0];
  my $dest = pop @ARGV;
  my $link;
  frame {
    require File::Copy;
    my $cmd = $mv ? 'move' : 'copy';
    $mv = $mv ? \&File::Copy::move : \&File::Copy::syscopy;
    for( @ARGV ) {
      my $d = -d $dest;
      my $dirdest = $d ? $dest . '/' . Makesubs::f_notdir $_ : $dest;
      _rm $dirdest if $force && ($d ? -e( $dirdest ) : defined $d);
      $link && perform { link $_, $dirdest } "link `$_' to `$dirdest'", 1
	or perform { &$mv( $_, $dirdest ) } "$cmd `$_' to `$dirdest'";
    }
  } 'f',
    ($mv ? () : [qw(l link), \$link]);
}
sub c_mv { c_cp \1, @_ }


sub c_cut {
  local @ARGV = @_;		# for <>
  my( $delimiter, $characters, $fields, $lines, $matching, $printf ) = "\t";
  my $err = "one of --characters, --fields or --lines must be given\n";
  frame {
    my $split = eval
      'sub { @::F = ' . (!$characters ? "split /\Q$delimiter\E/ }" : ($] < 5.008) ? 'split // }' : 'unpack "(a1)*", $_ }');
    my( @idxs, $eol );
    local @::F;	     # Use Perl's autosplit variable into Makeppfile's package
    for( $fields ) {
      @idxs = eval_or_die $_
	unless /^[-+.,\d\s]+$/ && s/((?:,|^)\d+\.\.)-/$1\$#::F+1-/g;
    }
    if( defined $lines ) {
      warn "options --matching or --printf make no sence with --lines\n" if $matching or defined $printf;
      if( @idxs ) {
	print +(<>)[@idxs] or die $!;
      } else {
	@::F = <>;
	print @::F[(eval_or_die $fields)] or die $!; # () suppress overzealous Perl warning
      }
    } elsif( defined $fields ) {
      while( <> ) {
	$eol = _chomp $_;
	&$split;
	next if $matching && @::F == 1;
	@::F = map { defined() ? $_ : '' } @::F[@idxs ? @idxs : eval_or_die $fields];
	if( $printf ) {
	  printf $printf, @::F or die $!;
	} else {
	  print join( $delimiter, @::F ), $eol or die $!;
	}
      }
    } else {
      die $err;
    }
  } qw(E f i I o O r s),
    [qw(c characters), \$characters, 1, sub { $delimiter = ''; warn $err if defined $fields; $fields = $characters }],
    [qw(d delimiter), \$delimiter, 1],
    [qw(f fields), \$fields, 1, sub { warn $err if defined $characters or defined $lines }],
    [qw(l lines), \$lines, 1, sub { warn $err if defined $fields; $fields = $lines }],
    [qw(m matching), \$matching],
    [qw(p printf), \$printf, 1, sub { _escape $printf }];
}


sub c_echo {
  my $cmd = 0;
  local @ARGV = @_;
  $cmd = ${shift @ARGV} if ref $_[0];
  my $nonewline;
  frame {
    my $res;
    if( $cmd == 1 ) {
      $res = eval_or_die "@ARGV" if @ARGV;
      $res = '' unless defined $res;
    } else {
      $res = $cmd == 2 ? shift @ARGV : "@ARGV";
      _escape $res;
      $res = sprintf $res, @ARGV if $cmd == 2;
    }
    if( $cmd == 3 ) {
      $res ||= 'y';
      # This will break out when the pipe reader exits, but not die
      1 while print $res, $nonewline ? () : "\n";
    } else {
      print $res, $nonewline || $cmd == 2 ? () : "\n" or die $!;
    }
    die "false\n" if $cmd == 1 && !$res;
  } qw(f o O),
    $cmd != 1 ? 'E' : (),
    $cmd != 2 ? ['n', qr/no[-_]?newline/, \$nonewline] : ();
}
sub c_expr { c_echo \1, @_ }
sub c_printf { c_echo \2, @_ }
sub c_yes { c_echo \3, @_ }


sub c_grep {
  my $cmd = 0;
  local @ARGV = @_;		# for <>
  $cmd = ${shift @ARGV} if ref $_[0];
  my( $fn, $n, $revert, $count, $list, $waste ) = (0, 0, 0);
  frame {
    my $prog = eval_or_die 'sub {' . shift( @ARGV ) . "\n}";
    perform {
      open my $fh, '>', $waste;
      $waste = $fh;
    } "write `$waste'" if $waste;
    while( <> ) {
      if( $cmd ) {
	&$prog;
	print or die $! if $cmd == 2;
      } elsif( $revert == 1 ? !&$prog : &$prog ) {
	$n++;
	if( !$list ) {
	  $count or print or die $!;
	} elsif( $revert == 2 ) {
	  $fn++ ;
	} else {
	  print "$ARGV\n" or die $!;
	  close ARGV;
	}
      } elsif( $waste ) {
	print $waste $_ or die $!;
      }
      if( $revert > 1 && $list && eof ) {
	print "$ARGV\n" or die $! if !$fn;
	$fn = 0;
      }
    }
    print "$n\n" or die $! if $count;
    close $waste if $waste;	# Sometimes needed on Solaris with V5.6.
    die "no matches\n" unless $cmd or $n or $count;
  } qw(f i I o O r s),
    ($cmd ? () :
     ([qw(c count), \$count],
      ['l', qr/list|files[-_]?with[-_]?matches/, \$list],
      ['v', qr/vice[-_]?versa|(?:re|in)vert[-_]?match/, \$revert],
      ['w', qr/waste[-_]?file/, \$waste, 1]));
}
sub c_perl { c_grep \1, @_ }
sub c_sed { c_grep \2, @_ }


sub c_install {
  local @ARGV = @_;
  my $dest = pop @ARGV;
  my( $mode, $gid, $uid, $copy, $link, $symbolic, $resolve, $log, $directory, $strip ) = 755;
  frame {
    my $i = 2;
    --$i, defined() && !/^\d+$/ and
      defined( $_ = $i ? getpwnam( $_ ) : getgrnam( $_ )) || die $i ? 'user' : 'group', " unknown\n"
      for $uid, $gid;
    local $install_date = localtime() . " [$$]\n";
    my $fh;	    # Don't open $install_log directly as that logs to itself.
    $log ||= $ENV{INSTALL_LOG} ||
      ($FileInfo::CWD_INFO->{ROOT} ? FileInfo::relative_filename( $FileInfo::CWD_INFO->{ROOT} ) : '.') .
      '/.install_log';
    perform { open $fh, '>>', $log } "append to `$log'";
    local $install_log = $fh;
    my @dest;
    if( $directory ) {
      warn "options --copy, --link or --strip make no sence with --directory\n"
	if $copy || $link || $symbolic || $resolve || $strip;
      @dest = c_mkdir( '-pm', $mode, @ARGV, $dest );
    } else {
      ($copy, $link) = $link ? (\&c_cp, '-l') :
	$copy ? \&c_cp :
	  $symbolic ? (\&c_ln, '-s') :
	    $resolve ? (\&c_ln, '-r') :
	      \&c_mv;
      for( @ARGV ) {
	&$copy( $link ? $link : (), $_, $dest );
	-d( $dest ) ? s!^(?:.*/)?!$dest/! : ($_ = $dest);
	push @dest, $_;
      }
      perform { system 'strip', @dest } "strip `@dest'" if $strip;
      c_chmod $mode, @dest;
    }
    if( defined $uid || defined $gid ) {
      # Why doesn't Perl make undef the portable way of leaving id unchanged? :-(
      perform { chown defined $uid ? $uid : (stat)[4], defined $gid ? $gid : (stat)[5], $_ }
	defined $uid && defined $gid ? "set owner $uid and group $gid for `$_'" :
	defined $uid ? "set owner $uid for `$_'" :
	"set group $gid for `$_'"
	for @dest;
    }
  } [qw(c copy), \$copy],
    [qw(d directory), \$directory],
    [qw(g group), \$gid, 1],
    [qw(l link), \$link],
    [0, qr/log(?:file)?/, \$log, 1],
    [qw(m mode), \$mode, 1],
    [qw(o owner), \$uid, 1],
    ['S', qr/sym(?:bolic(?:[-_]?link)?|link)/, \$symbolic],
    ['r', qr/resolve[-_]?(?:sym(?:bolic(?:[-_]?link)?|link))?/, \$resolve],
    [qw(s strip), \$strip];
}


sub c_ln {
  local @ARGV = @_;
  my $dest = pop @ARGV;
  $dest =~ s|/+$||;
  my $d = -d $dest;
  my( $symbolic, $resolve );
  frame {
    for( @ARGV ) {
      my $dirdest = $d ? $dest . '/' . Makesubs::f_notdir $_ : $dest;
      _rm $dirdest if $force && ($d ? -l( $dirdest ) || -e _ : defined $d);
      if( $ENV{MAKEPP_LN_CP} and $ENV{MAKEPP_LN_CP} & ($resolve || $symbolic ? 1 : 2) ) {
	$_ = Makesubs::f_relative_filename Makesubs::f_dir( $dirdest ) . $_
	  if $symbolic && !$resolve;
	require File::Copy;
	perform { File::Copy::syscopy( $_, $dirdest ) } "copy `$_' to `$dirdest'";
      } elsif( $resolve || $symbolic ) {
	$_ = Makesubs::f_relative_to $_ . ',' . Makesubs::f_dir $dirdest if $resolve;
	perform { symlink $_, $dirdest } "link `$_' to `$dirdest' symbolically";
      } else {
	perform { link $_, $dirdest } "link `$_' to `$dirdest'";
      }
    }
  } 'f',
    ['r', qr/resolve[-_]?(?:sym(?:bolic(?:[-_]?link)?|link))?/, \$resolve],
    ['s', qr/sym(?:bolic(?:[-_]?link)?|link)/, \$symbolic];
}


sub c_mkdir {
  local @ARGV = @_;
  my $mode = 755;	 # Makefile must set the option, if this is not right.
  my $parent;
  frame {
    $mode = oct $mode;
    my $umask = umask 0;	# Ignore environment, to guarantee result.
    my @created;
    for( @ARGV ) {
      if( $parent ) {
	my $dir = '';
	for( split /(?=\/)/ ) {
	  $dir .= $_;
	  next if -d $dir;
	  _rm $dir if $force && -e _;
	  perform { (my $d = $dir) =~ s!/+$!!; mkdir $d, $mode or $! == POSIX::EEXIST && -d $dir } "create directory `$dir'";
	  push @created, $dir;
	}
      } elsif( ! -d ) {
	_rm $_ if $force && -e _;
	perform { (my $d = $_) =~ s!/+$!!; mkdir $d, $mode } "create directory `$_'";
	push @created, $_;
      }
    }
    umask $umask;
    @created;
  } 'f',
    [qw(m mode), \$mode, 1],
    [qw(p parent), \$parent];
}


my $package_seed = 0;
sub c_preprocess {
  local @ARGV = @_;
  my( $command_line_vars, $assignment, $tmp ) = {};
  frame {
    # Functions need not be eliminated because they get explicitly invoked via $(...), whereas
    # any word at the beginning of line might be a statement, so import only the usefuls.
    my $re = $assignment ?
      qr/f_|s_(?:_?include|(?:make)?(?:perl|sub)|perl_begin|define|(?:un)?export)/ :
      qr/f_|s_(?:_?include|(?:make)?(?:perl|sub)|perl_begin)/;
    my $cwd = $FileInfo::CWD_INFO;
    for( @ARGV ) {
      perform {
	local $Makefile::c_preprocess = $assignment ? 1 : 2;
	my $finfo = FileInfo::file_info $_, $cwd;
	local $Makefile::makefile = bless
	  { MAKEFILE => $finfo,
	    PACKAGE => 'preprocess_' . $package_seed++,
	    CWD => $finfo->{'..'},
	    COMMAND_LINE_VARS => $command_line_vars,
	    ENVIRONMENT => $Makesubs::rule->{MAKEFILE}{ENVIRONMENT},
	    RE_COUNT => 0		# Force initial creation of statement RE
	   }, 'Makefile';
	FileInfo::chdir $finfo->{'..'};
	eval "package $Makefile::makefile->{PACKAGE}; use Makesubs \$re";
	Makefile::read_makefile( $Makefile::makefile, $finfo ); 1;
      } "preprocess `$_'";
    }
  } $command_line_vars,
    qw(f o O),
    [qw(a assignment), \$assignment],
    [qw(h hashref), \$tmp, 1, sub { $tmp = eval_or_die $tmp; $command_line_vars->{$_} = $tmp->{$_} for keys %$tmp }];
}


sub c_rm {
  local @ARGV = @_;
  my $meta;
  frame {
    my %makepp;
    if( $meta ) {
      for( @ARGV ) {
	m!(.*?)([^/]+)$!;
	$makepp{"$1.makepp"} = 1 if unlink "$1.makepp/$2.mk";
      }
    }
    @ARGV = grep -e || -l, @ARGV if $force;
    rmdir for keys %makepp;
    _rm @ARGV;
  } 'f',
    [qw(m metainfo), \$meta];
}



sub c_sort {
  local @ARGV = @_;		# for <>
  my( $uniq, $cmp, $rev, $transform, $detransform ) = '';
  frame {
    $uniq &&= 'grep { $a = $_, 1 if !defined $a or ' . ($cmp ? "do { \$b = \$_; $cmp }}" : '$a cmp $_ }');
    $uniq .= ' reverse' if $rev;
    print
      $transform ? eval_or_die "map { $detransform } $uniq sort " . ($cmp ? "{ $cmp } " : '') . "map { $transform } <>" :
	$cmp ? eval_or_die "$uniq sort { $cmp } <>" :
	  $uniq ? eval_or_die "$uniq sort <>" :
	    $rev ? reverse sort <> :
	      sort <>
      or die $!;
    eval_or_die 'undef $a' if $uniq;
  } qw(f i I o O r s),
    [qw(c compare), \$cmp, 1],
    ['n', qr/num(?:eric(?:[-_]?sort)?)?/, \$cmp, 0, q{ do { no warnings; $a <=> $b }}],
				# Eliminate ugly warning about trailing non-digits
    [qw(r reverse), \$rev],
    [qw(t transform), \$transform, 1],
    [qw(d detransform), \$detransform, 1],
    ['u', qr/uniq(?:ue)?/, \$uniq];
}



sub c_template {
  local @ARGV = @_;		# for <>
  my( %macros, $tmp );
  my( $pre, $suf, $Pre, $Suf, $afterPre, $afterSuf, $re ) = ('@', qr/@(?:\\\n)?/, qw(@@ @@ @@));
  frame {
    $re = $re ? join( '|', keys %macros ) : qr/\w[-\w.]*/;
    my $handler = sub {
      if( defined $_[3] ) {	# @macro=def@
	if( exists $macros{$_[3]} ) {
	  return '' if $_[4];	# @macro?=def@
	} else {
	  $re = ($re ? "$re|" : '') . $_[3];
	}
	$macros{$_[3]} = $_[6] ? eval_or_die( "sub $_[6]" ) : $_[5];
				# @macro { Perlcode }@
	'';
      } elsif( defined $_[2] ) { # @{ Perlcode }@
	eval_or_die $_[2];
      } else {			# @macro@ or @macro(arg1,arg2...)@
	my $repl = $macros{$_[0]};
	if( !defined $repl ) {
	  $repl = '';
	} else {
	  my @args = map { s/$pre($re)$suf/$macros{$1}/g; $_ } split ',', $_[1] if $_[1];
	  if( ref $repl ) {
	    $repl = &$repl( @args );
	  } elsif( $_[1] ) {		# @macro(arg1,arg2...)@
	    $repl =~ s/\$([1-9])/$1 > @args ? '' : $args[$1-1]/ge;
	    $repl =~ s/\$\{([1-9]\d*)\}/$1 > @args ? '' : $args[$1-1]/ge;
	  }
	}
	$repl;
      }
    };
    while( <> ) {
      if( $Pre and s/(.*?) $Pre (?:($re)(?:\((.*?)\))? | \{(.*?)\}) $Suf//x ) {
				# Substitute anything containg @@xyz@@ ... @@
	my( $before, $name, $args, $perl ) = ($1, $2, $3, $4);
	my $end = $afterSuf ? qr/$name$afterSuf/ : '';
	while() {
	  last if s/.*?$afterPre$end/ $before . &$handler( $name, $args, $perl ) /e;
	  $_ = <>;
	}
      }
      s/$pre (?:($re)(?:\((.*?)\))? | \{(.*?)\} | (\w[-\w.]*)(?:(\??)=(.*?) | \s*(\{.*\}))) $suf/
	&$handler( $1, $2, $3, $4, $5, $6, $7 ) /xeg;
				# Substitute anything containg @xyz@.
      print or die $!;
    }
  } \%macros, qw(f i I o O),
    [qw(h hashref), \$tmp, 1, sub { $tmp = eval_or_die $tmp; $macros{$_} = $tmp->{$_} for keys %$tmp }],
    [qw(s simple), \$pre, 1,
     sub {
       my $split = substr $pre, 0, 1; $split = qr/\Q$split/;
       (undef, $pre, $suf) = split $split, $pre;
       undef $Pre;
     }],
    [qw(m multiline), \$Pre, 1,
     sub {
       my $split = substr $Pre, 0, 1; $split = qr/\Q$split/;
       (undef, $Pre, $Suf, $afterPre, $afterSuf) = split $split, $Pre;
     }],
    [qw(d defined), \$re];
}


sub c_touch {
  local @ARGV = @_;
  frame {
    my $time = time;
    for( @ARGV ) {
      if( -f ) {
	perform { utime $time, $time, $_ } "update timestamp on `$_'";
      } else {
	perform { open my $fh, '>', $_ } "create `$_'";
      }
    }
  };
}


sub c_uninstall {
  local @ARGV = @_;             # for <>
  frame {
    @ARGV = $ENV{INSTALL_LOG} ||
      ($FileInfo::CWD_INFO->{ROOT} ? FileInfo::relative_filename( $FileInfo::CWD_INFO->{ROOT} ) : '.') .
      '/.install_log'
      unless $inpipe || @ARGV;
    my %files;
    /^  .* `(.+)'/ and $files{$1} = 1 while <>;
    eval { _rm sort keys %files };
  } 'i', 'I'; # fails in 5.6: qw(i I);
}


sub c_uniq {
  local @ARGV = @_;             # for <>
  my $cmp;
  frame {
    $cmp = eval_or_die "sub {$cmp\n}" if defined $cmp;
    no strict 'refs';
    local *a = \${"$Makesubs::rule->{MAKEFILE}{PACKAGE}::a"};
    local *b = \${"$Makesubs::rule->{MAKEFILE}{PACKAGE}::b"};
    undef $a;
    while( $b = <> ) {
      print $b or die $! if !defined $a or $cmp ? &$cmp() : $a ne $b;
      $a = $b;
    }
  } qw(f i I o O r s),
    [qw(c compare), \$cmp, 1];
}

1;
