#!/usr/bin/perl -w
#
# This script does the pod to html generation with some heavy massageing.
#
# $Id: html.pl,v 1.10 2012/05/29 21:31:20 pfeiffer Exp $
#

package Mpp::html;

use strict;

use File::Copy;
use File::Path;

our $podroot;

BEGIN { unshift @INC, 'html' }	# For now bundle 1.11, the last working version
use Pod::Html ();

BEGIN {
    *html_short_names = $Pod::Html::VERSION < 1.0901 ? sub() {} : sub() { 1 };
    no warnings;
    my %entity = qw(< lt > gt & amp);
    *Pod::Html::process_pre = sub { # undo bad magic, should do all ours here
	1 while ${$_[0]} =~ s/^([^\t\n]*)(\t+)/$1 . (' ' x (length($2) * 8 - length($1) % 8))/egm; # untabify
	${$_[0]} =~ s/([<>&])/&$entity{$1};/g;
    };
}

our @extra_files = <html/makepp.css html/makepp.js html/*.png>;

our $webroot ||= 'http://makepp.sourceforge.net/';
our $docroot ||= '';
our $docindex ||= 'index.html';
our $relindex ||= 'index.html';

our( @pods, $target_dir );
die unless @pods;

my %alias = (makepp => 'Makepp Introduction',
	     makepp_tutorial_compilation => 'Makepp Compilation Tutorial',

	     makepp_build_check => 'Makepp Build Check Methods',
	     makepp_builtin => 'Makepp Builtin Rules',
	     makepp_builtins => 'Makepp Builtin Commands',
	     makepp_faq => 'Makepp FAQ',

	     makepp_command => 'makepp,&nbsp; mpp',
	     makeppbuiltin => 'makeppbuiltin,&nbsp; mppb',
	     makeppclean => 'makeppclean,&nbsp; mppc',
	     makeppdeclude => 'makeppdeclude,&nbsp; mppd',
	     makeppgraph => 'makeppgraph,&nbsp; mppg',
	     makeppinfo => 'makeppinfo,&nbsp; mppi',
	     makepplog => 'makepplog,&nbsp; mppl',
	     makeppreplay => 'makeppreplay,&nbsp; mppr');

my @nav =
 ([qw(Overview
      makepp
      makepp_tutorial
      makepp_tutorial_compilation
      makepp_release_notes
      makepp_incompatibilities
      makepp_speedup
      perl_performance)],
  [qw(Q&amp;A
      makepp_cookbook
      makepp_faq)],
  my $features = [qw(Features)],
  [qw(Commands
      makepp_command
      makeppbuiltin
      makeppclean
      makeppgraph
      makeppinfo
      makepplog
      makeppreplay)],
  [qw(Index
      makepp_index)]);
my %nav_header;
{				# Put all others into Features
  my( %seen, %not_seen );
  @seen{map @$_, @nav} = ();	# make keys exist (headers too, but they don't collide with files)
  for( @pods ) {
    my $base = substr $_, 0, -4; # sans .pod
    $alias{$base} ||= join ' ', map "\u$_", split '_', $base;
    exists $seen{$base} or undef $not_seen{$base}; # make key exist
  }
  push @$features, sort keys %not_seen;
  for my $nav ( @nav ) {
    for my $i ( 1..$#$nav ) {
      $nav_header{$nav->[$i] .= '.html'} = $nav;
    }
  }
}

my( %nolink, %link, @links );
my( %prev, %next, $prev, @tabmeta );
sub init {
  for( @pods ) {
    my $base = substr $_, 0, -4;
    my $file = "$base.html";
    my $title = $alias{$base};
    $title =~ s/^Makepp //;	# Don't need this in navbar
    $nolink{$file} = "<b>$title</b>";
    $link{$file} = "<a href='$docroot" . ($file eq 'makepp.html' ? $docindex : $file) . "'>$title</a>";
  }
  @links = map {
    map { $_ eq 'makepp.html' ? $docindex : $_ } @{$_}[1..$#$_];
  } @nav;

  for( @links ) {
    if( $prev ) {
      $prev{$_} = $prev;
      $next{$prev} = $_;
    }
    $prev = $_;
  }

  @tabmeta =			# title, alt, href, img-src-base
   (['Makepp Homepage', 'Home', $webroot, 'home'],
    ['Frequently Asked Questions', 'FAQ', "${docroot}makepp_faq.html", 'faq'],
    [('Documentation') x 2, "${docroot}$docindex", 'doc'],
    [('Gallery') x 2, "${webroot}gallery/", 'gallery'],

    [('Download') x 2, 'http://sourceforge.net/projects/makepp/files/2.1/snapshots/', 'download'],
    ['Makepp on SourceForge', 'SourceForge', 'https://sourceforge.net/projects/makepp/', 'sourceforge'],
    ['Makepp on CPAN', 'CPAN', 'http://search.cpan.org/dist/makepp/', 'cpan']);
}
&init;


sub frame($$$) {
  my( $file, $tab ) = @_;
  my( $head, $body ) = split "\f", $_[2];
  die unless $body;

  my $tabs = '';
  for my $meta ( @tabmeta ) {
    my $on = $tab eq $meta->[1] ? '_on' : '';
    $tabs .=
      "<a href='$meta->[2]'><img title='$meta->[0]' id='_$meta->[3]$on' src='${docroot}tabs.png' alt='$meta->[1]'/></a>";
  }

  my $nav = '';
  for( @nav ) {
    if( $file && $_ == $nav_header{$file} ) {
      $nav .= @$_ == 2 ?
	 "<li id='_act'><b>$_->[0]</b>" :
	 "<li id='_act' class='unfold' onclick='fold(this)'><b>$_->[0]</b><ul>";
    } else {
      $nav .= @$_ == 2 ?
	 "<li>$link{$_->[1]}" :
	 "<li class='fold' onclick='fold(this)'>$_->[0]<ul>";
    }
    if( @$_ > 2 ) {
      for my $i ( 1..$#$_ ) {
	$nav .= $file eq $_->[$i] ?
	  "<li>$nolink{$_->[$i]}</li>" :
	  "<li>$link{$_->[$i]}</li>";
      }
    }
    $nav .= @$_ == 2 ?
      "</li>" :
      "</ul></li>";
  }
  my $ret;
  for my $piece ( split /(div id='_main'>|<pre>.*?<\/pre>)/s, "<?xml version='1.0'?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.1//EN' 'http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd'>
 <html xmlns='http://www.w3.org/1999/xhtml'>
<head>
  <link rel='stylesheet' href='${docroot}makepp.css' type='text/css'/>
  $head
  <meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>
  <link rel='shortcut icon' type='image/png' href='${docroot}url.png'/>
  <link rel='top' href='$webroot'/>
  <link rel='index' href='${docroot}makepp_index.html'/>
  <link rel='contents' href='${docroot}index.html'/>
  <link rel='help' href='http://sourceforge.net/projects/makepp/support'/>
  <script src='${docroot}makepp.js' type='text/javascript'></script>
  <meta name='keywords' content='makepp, make++, Make, build tool, repository, cache, Perl, Make alternative, Make replacement, Make enhancement, Make improvement, Make substitute, dmake, gmake, GNU Make, nmake, pmake, easymake, imake, jmake, maketool, mmake, omake, ppmake, PVM gmake, shake, SMake, ant, maven, cook, jam, Cons, SCons, cc -M, gcc -MM, g++ -MM, makedepend, makedep, mkdep, CCache, Compilercache, cachecc1, Make::Cache, automake'/>
</head>

<body>
<div id='_head'>
  <form action='http://www.google.com/search'>
    <p>
      <input type='hidden' name='as_sitesearch' value='makepp.sourceforge.net/2.1'/>
      <input type='text' name='as_q'/>
      <input type='submit' value='Go'/>
    </p>
  </form>
  <a title='Makepp Homepage' href='$webroot'><img src='${docroot}makepp.png' alt='Makepp'/></a>
  <div>$tabs</div>
</div>
<ul>
  <li id='_close'>
    <script type='text/javascript'>r()</script>
    <span title='Flip side' onclick='lr(this)'>\x{2194}</span><span title='Collapse' onclick='roll(this)'>\x{2013}</span><span title='Expand' onclick='roll(this,1)'>\xa4</span><span title='Close' onclick='nonav(this)'>\xd7</span>
  </li>$nav
</ul>
<div id='_main'>$body</div><div id='_clear'/></body></html>" ) {
    if( !$ret ) {		# compact html
      ($ret = $piece) =~ s/>\s+</></g;
    } else {
      if( $piece !~ /^<pre>/ ) {
	$piece =~ s/\s+/ /g;
	$piece =~ s! />!/>!g;
	$piece =~ s!(</?)strong([> ])!$1b$2!g;
	$piece =~ s!(</?)em([> ])!$1i$2!g; # Use <i> for italic, because of u
	$piece =~ s!(</?)code([> ])!$1tt$2!g;
	$piece =~ s! ?(</?(?:li|ul|p|h[1-6]|hr/|d[dlt])\b[^>]*>) ?!$1!g;
      }
      $piece =~ s!(</?)u([> ])!$1em$2!g; # Use now free em for deprecated u -- crap!!!
      $ret .= $piece;
    }
  }
  $ret;
}

our( $pre, $end, %seen );

sub highlight_keywords() {
  s!\G(\s*)((?:noecho\s+|ignore_error\s+|makeperl\s+|perl\s+|[-\@]|&amp;(?:cat|chmod|cp|mv|cut|echo|expr|printf|yes|grep|sed|(?:un)?install|ln|mkdir|perl|preprocess|rm|sort|template|touch|uniq)\b)+)!$1<b>$2</b>! or

  s!\G((?:override )?(?:define|export|global)|override|ifn?def|makesub|sub)(&nbsp;| +)([-.\w]+)!<b>$1</b>$2<i>$3</i>! or
  s!\G(register[_-](?:(?:command[_-])?parser|scanner)|signature)(&nbsp;| +)([-.\w]+)!<b>$1</b>$2<u>$3</u>! or
  # repeat the above, because they may appear in C<> without argument
  s!\G(\s*(?:and |or |else )?if(?:n?(?:def|eq|sys|true|xxx)|(?:make)?perl)|build[_-]cache|else|endd?[ei]f|export|global|fi|[_-]?include|load[_-]makefile|makeperl|no[_-]implicit[_-]load|override|perl(?:|[_-]begin|[_-]end)|repository|runtime|unexport|define|makesub|sub|register[_-](?:(?:command[_-])?parser|scanner)|signature|vpath)\b!<b>$1</b>! && s|xxx|<i>xxx</i>| or

    # highlight assignment
    s,\G\s*(?:([-.\w\s%*?\[\]]+?)(\s*:\s*))?((?:override\s+)?)([-.\w]+)(?= *(?:[:;+?!]|&amp;)?=),
      ($1 ? "<u>$1</u>$2" : '') . ($3 ? "<b>$3</b>" : '') . "<i>$4</i>"
    ,e or

    # highlight rule targets -- EOL trickery to mostly not underline Perl or C++
    $pre && !/define|export|global|override/ && s!\G(\s*)([^&\s].*?)(?=\s*:(?:$|.*?[^;{]\n))!$1<u>$2</u>!m;

  # highlight rule options
  s!(: *)(build_c(?:ache|heck)|(?:command[_-])?parser|foreach|include|scanner|signature)(&nbsp;| +)([-_/\w%.]+)!$1<b>$2</b>$3<u>$4</u>!g or
  # repeat the above, because they may appear in C<> without argument
  s!(: *)(build_c(?:ache|heck)|(?:command[_-])?parser|foreach|include|last_chance|quickscan|scanner|signature|smartscan)\b!$1<b>$2</b>!g;
}

sub highlight_variables() {
  s((\$[\{\(]{1,2})([-\w]+)([\}\)]{0,2})){
    my( $prefix, $name, $suffix ) = ($1, $2, $3);
    $name = "<b>$name</b>" if
      $name =~ /absolute[_-]filename|
	add(?:pre|suf)fix|
	basename|
	call|
	CURDIR|
	dependenc(?:y|ies)|
	dir(?:[_-]noslash)?|
	error|
	filesubst|
	filter(?:[_-]out)?|
	find(?:[_-](?:program|upwards)|file|string)|
	first(?:[_-]available|word)|
	foreach|
	if|
	infer[_-](?:linker|objects)|
	inputs?|
	join|
	make(?:perl)?|
	map|
	mktemp|
	notdir|
	only[_-](?:generated|stale|(?:non)?targets)|
	origin|
	outputs?|
	patsubst|
	perl|
	phony|
	print|
	PWD|
	relative[_-](?:filename|to)|
	shell|
	sort(?:ed_dependencies|ed_inputs)?|
	stem|
	strip|
	subst|
	suffix|
	targets?|
	warning|
	wildcard|
	word(?:list|s)|
	xargs/x;
    $name =~/>(?:outputs?|stem|targets?)</ ?
      "<u>$prefix<i>$name</i>$suffix</u>" :
      "$prefix<i>$name</i>$suffix";
  }eg;
  s!(\$[\@%*])!<u>$1</u>!g;
}

sub simplify($$$) {
  copy $_[0], "/tmp/p2h.$Pod::Html::VERSION" if $_[1] eq 'makepp_signatures';
  open my( $tmpfile ), $_[0] or die;
  binmode $tmpfile, ':utf8' if $] > 5.008; # suppress warning
  my $base = $_[1];
  my $timestamp = $_[2];
  my $file = "$base.html";
  my $author = '';
  my $has_commands_with_args =
    $file =~ /^makepp(?:_(?:builtins|command|extending|functions|statements)|builtin|graph|log)/;
  my $title = $alias{$base};
  my %count;
  my $next_tall = '';

  my $contents = $next{$file} ? "<link rel='next' href='$next{$file}'/>" : '';
  $contents .= "<link rel='prev' href='$prev{$file}'/>" if $prev{$file};

  {
    local $/ = '<body>';
    my $head = <$tmpfile>;
    $head =~ /<title>(\w+) -- (.+)<\/title>/is;
    substr $contents, 0, 0, "<title>$alias{$base} \x{2014} $2</title>";
    $contents .= "\f<h1>$alias{$base}</h1><p><b>$2</b></p>";
  }

  my $index;
  while( <$tmpfile> ) {
    s/(<li><a href="#.+">.*<code>-)(\w\w)/$1-$2/; # it swallows one - :-(
    last if /<h1>.*(?:DESCRIPTION|SYNOPSIS)/;
    if ( /<(?:\/?ul|li)>/ ) {
      # These are visible anyway when the index is.
      next if /NAME|DESCRIPTION|SYNOPSIS|AUTHOR/;
      $index .= $_;
    }
  }
  for( $index ) {
    # Rearrange the index, because we threw out some items, discard it if empty.
    s!<ul>\s+</ul>!!;
    m!<ul>\s+<ul>! &&
      s!</ul>\s+</ul>!</ul>! &&
	s!<ul>\s+<ul>!<ul>!;
    1 while s@(</li>)\s*(<ul>.*?</ul>)(?!</li>)(.+?)\Z@$2$1$3@sg; # Go backwards because of nesting
    $contents .= $_ if $_;
  }

  while( <$tmpfile> ) {
    if( s!</(?:body|html)>.*!! ) {
      $contents .= $_;
      last;
    } elsif( s/<pre>\n// ) {
      $pre = 1;
    } elsif( $pre ) {

      if( /^(.*#.*\|.*\|.*#.*\|.*\|.*)<\/pre>$/ ) { # Special case for compatibility table.

	my $row = $1;
	my @list = split /[#|]/, $row;
	s/^\s+//, s/\s+$// for @list;
	if( $list[0] ) {
	  $_ = '<tr><th align="left">' . shift( @list ) . '</th>';
	  for my $elem ( @list ) {
	    if( $elem eq 'x' ) {
	      $_ .= '<th class="good">x</th>';
	    } elsif( $elem eq '/' ) {
	      $_ .= '<th class="bad"><i>/</i></th>';
	    } elsif( $elem ) {
	      $_ .= "<th class='soso'>x<a href='#\L$elem'><sup>*)</sup></a></th>";
	    } else {
	      $_ .= '<th>&nbsp;</th>';
	    }
	  }
	} else {		# Heading line.
	  shift @list;
	  $_ = '<tr><th></th>';
	  if( $list[0] ne '.0' ) {
	    (undef, @list) = split /#/, $row;
	    for my $elem ( @list ) {
	      my $span = $elem =~ tr/|//d + 1;
	      $elem =~ tr/ \t//d;
	      $_ .= "<th colspan='$span'>&nbsp;$elem&nbsp;</th>";
	    }
	  } else {
	    for my $elem ( @list ) {
	      $_ .= "<th>&nbsp;$elem&nbsp;</th>";
	    }
	  }
	}
	$_ .= "</tr>\n";
	$pre = 0;

      } else {

	# unindent initial whitespace which marks <pre> in pod
	s/^ {1,7}\t(\t*)(?!#)/$1    / or s/^    ?//;

	if( /^\s+$/ ) {
	  $next_tall = '<b class="tall"> </b>';
	  next;
	} else {
	  # don't grok comments
	  $end = s/(#|# .*?)?((?:<\/pre>)?)$// ?
	    ($1 ? "<span class='comment'>$1</span>$next_tall$2" : "$next_tall$2") :
	      $next_tall;
	  $next_tall = '';
	}

	s!^([%\$]? ?)(makepp(?:builtin|clean|log|graph|replay|_build_cache_control)?)\b!$1<b>$2</b>!g or
	  s!^([%\$]? ?)(mpp(?:[bclgr]c{0,2})?)\b!$1<b>$2</b>!g or
				# g creates BOL \G for keywords
	    highlight_keywords;
	highlight_variables;

	# put comment back in
	s/$/$end/ if $end;
	$_ = '<pre>' . $_, $pre++ if $pre == 1;
	$pre = 0 if m!</pre>!;

      }
    } elsif( s!<dt><strong><a name="(\w*)"[^>]*>(.*)</a></strong></dt>!my $id = $1 ? " id='$1'" : ''; "<dt$id><b>$2</b></dt>"!e ) {
      if( !html_short_names ) {
	s!(<strong>-. )(.+?<)!$1<i>$2/i><! ||	# Repetitions of same don't get itemized.
	  s!("item_[^"]*">--[^<=]*=)(.+?) ?<!$1<i>$2</i><! ||
	    s!("item_[^"]*">[^ <,]* )(.+?) ?<!$1<i>$2</i><!
	      if $has_commands_with_args;	# italicize args
	s!"item_(\w+)[^"]*">(\1)!"$1">$2!i;	# fix =item anchor
	s!"item_(_2d\w+)">!"$1">! ||		# fix =item hexcode-anchor
	  s! name="item_(_\w+?)(?:__[25][db].*?)?">!$seen{$1}++ ? '>' : " name='$1'>"!e;
      }
    } else {
      s!<h([123])><a name=(.*?)</a></h\1>!<h$1 id=$2</h$1>!;
      s/<a&nbsp;href/<a href/g;	# what the heck
      s!(<a href="[^>]+">)(.*?)<a href="[^>]+">(.+?)</a>!index( $2, '</a>' ) > -1 ? $& : "$1$2$3"!eg and
	s!(<a href="[^>]+">)(.*?)<a href="[^>]+">(.+?)</a>!index( $2, '</a>' ) > -1 ? $& : "$1$2$3"!eg;
      #s!([\s>]|^)([Mm]ake)pp([\s,.:])!$1<i><span class="makepp">$2</span>pp</i>$3!g;
      s!("#_)(?=\w+">&amp;)!${1}26!g;		# fix builtins index link
      s!<li></li>!<li>!;
      if( !html_short_names ) {
	s!(<strong>-. )(.+?<)!$1<i>$2/i><! ||	# Repetitions of same don't get itemized.
	  s!("item_[^"]*">--[^<=]*=)(.+?) ?<!$1<i>$2</i><! ||
	    s!("item_[^"]*">[^ <,]* )(.+?) ?<!$1<i>$2</i><!
	      if $has_commands_with_args;		# italicize args
	s!"item_(\w+)[^"]*">(\1)!"$1">$2!i;	# fix =item anchor
	s!"item_(_2d\w+)">!"$1">! ||		# fix =item hexcode-anchor
	  s! name="item_(_\w+?)(?:__[25][db].*?)?">!$seen{$1}++ ? '>' : " name='$1'>"!e;
	s!#item_(\w+)">!#$1">!g;		# fix =item link
      }
      s!\./(\.html.+? in the (.+?) manpage<)!$2$1!g;		  # at least up to 5.8.5
      highlight_keywords while /<code>/g;	# g creates "pseudo-BOL" \G for keywords
      highlight_variables;
      if( /<h1.+AUTHOR/ ) {	# Put signature at bottom right.
	$_ = <$tmpfile>;
	/p>(.+?) (?:\(|&lt;)(<a href="mailto:.+?">)/i;
	$author = "$2$1</a>";
	$_ = '';
      }
    }
				# uniquify =item labels (and simplify them)
    no warnings 'uninitialized';
    s!(<[^>]+) id=['"]([^>'"]*)['"]!$1 id="$2$count{$2}"! &&
      ++$count{$2};
    s!\./\./makepp!makepp!g;
    $contents .= $_;
  }

  if( $timestamp || $author ) {
    $timestamp = ($author ? '<br/>' : '') .
      "Last modified: $timestamp" if $timestamp;
    $contents .= $author ?
      "<address>$author$timestamp</address>" :
	"<hr/><address>$timestamp</address>";
  }

  frame $file, $file =~ /cookbook|faq/ ? 'FAQ' : 'Documentation', $contents;
}

sub pods2html {
  my $libpods = join ':', map { /(makepp.+)\.pod/ ? $1 : () } @pods;
  my $tmp = 'tmp' . substr rand, 1;
  mkpath $target_dir;
  for( @pods ) {
    my $timestamp = '';
    {
      open my( $podfile ), $_;
      for( 1..10 ) {		# Expect it in first 10 lines.
	if( <$podfile> =~ m!\$Id: .+,v [0-9.]+ (\d{4})/(\d{2})/(\d{2}) ! ) {
	  $timestamp = "$1-$2-$3";
	}
      }
    }
    if( fork ) {		# separate pod2html, or they append numbers to same name in different files
      wait;
    } else {
      Pod::Html::pod2html qw'--podpath=. --htmlroot=. --css=makepp.css --libpods', $libpods,
	'--infile', $_, '--outfile', $tmp;
      exit;
    }
    s/\.pod$//;
    open my $outfile, ">$target_dir/$_.html" or die "can't create `$target_dir/$_.html'--$!";
    chmod 0644, "$target_dir/$_.html";
    binmode $outfile, ':utf8' if $] > 5.008; # suppress warning
    print $outfile simplify $tmp, $_, $timestamp;
  }

  unlink $tmp;

  for( @extra_files ) {
    my $file = $_;
    substr $_, 0, 5, '';	# sans html/
    copy $file, "$target_dir/$_";
    chmod 0644, "$target_dir/$_";
  }

  my( $m, $i ) = ("$target_dir/makepp.html", "$target_dir/index.html");
  eval { symlink 'makepp.html', $i; -f $i } or
    eval { link $m, $i; -f $i } or
    copy $m, $i;
}

1;
