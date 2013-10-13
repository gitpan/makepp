#!/usr/bin/env perl
#
# This script does the pod to html generation with some heavy massageing.
#
# $Id: html.pl,v 1.16 2013/10/13 19:42:43 pfeiffer Exp $
#

#use Pod::Html ();		# Stole module to have consistent id-attributes.
BEGIN {package
# hide from CPAN parser
Pod::Html;
use strict;
require Exporter;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
$VERSION = 1.11;
@ISA = qw(Exporter);
@EXPORT = qw(pod2html htmlify);
@EXPORT_OK = qw(anchorify);

use Carp;
use Config;
use Cwd;
use File::Spec;
use File::Spec::Unix;
use Getopt::Long;

use locale;	# make \w work right in non-ASCII lands

my($Cachedir);
my($Dircache, $Itemcache);
my @Begin_Stack;
my @Libpods;
my($Htmlroot, $Htmldir, $Htmlfile, $Htmlfileurl);
my($Podfile, @Podpath, $Podroot);
my $Css;

my $Recurse;
my $Quiet;
my $HiddenDirs;
my $Verbose;
my $Doindex;

my $Backlink;
my($Listlevel, @Listtype);
my $ListNewTerm;
use vars qw($Ignore);  # need to localize it later.

my(%Items_Named, @Items_Seen);
my($Title, $Header);

my $Top;
my $Paragraph;

my %Sections;

# Caches
my %Pages = ();			# associative array used to find the location
				#   of pages referenced by L<> links.
my %Items = ();			# associative array used to find the location
				#   of =item directives referenced by C<> links

my %Local_Items;
my $Is83;

my $Curdir = File::Spec->curdir;

init_globals();

sub init_globals {
    $Cachedir = ".";		# The directory to which item and directory
				# caches will be written.

    $Dircache = "pod2htmd.tmp";
    $Itemcache = "pod2htmi.tmp";

    @Begin_Stack = ();		# begin/end stack

    @Libpods = ();	    	# files to search for links from C<> directives
    $Htmlroot = "/";	    	# http-server base directory from which all
				#   relative paths in $podpath stem.
    $Htmldir = "";	    	# The directory to which the html pages
				# will (eventually) be written.
    $Htmlfile = "";		# write to stdout by default
    $Htmlfileurl = "";		# The url that other files would use to
				# refer to this file.  This is only used
				# to make relative urls that point to
				# other files.

    $Podfile = "";		# read from stdin by default
    @Podpath = ();		# list of directories containing library pods.
    $Podroot = $Curdir;	        # filesystem base directory from which all
				#   relative paths in $podpath stem.
    $Css = '';                  # Cascading style sheet
    $Recurse = 1;		# recurse on subdirectories in $podpath.
    $Quiet = 0;		        # not quiet by default
    $Verbose = 0;		# not verbose by default
    $Doindex = 1;   	    	# non-zero if we should generate an index
    $Backlink = '';		# text for "back to top" links
    $Listlevel = 0;		# current list depth
    @Listtype = ();		# list types for open lists
    $ListNewTerm = 0;		# indicates new term in definition list; used
    				# to correctly open/close <dd> tags
    $Ignore = 1;		# whether or not to format text.  we don't
				#   format text until we hit our first pod
				#   directive.

    @Items_Seen = ();	        # for multiples of the same item in perlfunc
    %Items_Named = ();
    $Header = 0;		# produce block header/footer
    $Title = '';		# title to give the pod(s)
    $Top = 1;			# true if we are at the top of the doc.  used
				#   to prevent the first <hr /> directive.
    $Paragraph = '';		# which paragraph we're processing (used
				#   for error messages)
    %Sections = ();		# sections within this page

    %Local_Items = ();
    $Is83 = $^O eq 'dos';       # Is it an 8.3 filesystem?
}

#
# clean_data: global clean-up of pod data
#
sub clean_data($){
    my( $dataref ) = @_;
    for my $i ( 0..$#{$dataref} ) {
	${$dataref}[$i] =~ s/\s+\Z//;

        # have a look for all-space lines
      if( ${$dataref}[$i] =~ /^\s+$/m and $dataref->[$i] !~ /^\s/ ){
	    my @chunks = split( /^\s+$/m, ${$dataref}[$i] );
	    splice( @$dataref, $i, 1, @chunks );
	}
    }
}


sub pod2html {
    local(@ARGV) = @_;
    local($/);
    local $_;

    init_globals();

    $Is83 = 0 if (defined (&Dos::UseLFN) && Dos::UseLFN());

    # cache of %Pages and %Items from last time we ran pod2html

    #undef $opt_help if defined $opt_help;

    # parse the command-line parameters
    parse_command_line();

    # escape the backlink argument (same goes for title but is done later...)
    $Backlink = html_escape($Backlink) if defined $Backlink;

    # set some variables to their default values if necessary
    my $pod;
    unless (@ARGV && $ARGV[0]) {
	if ($Podfile and $Podfile ne '-') {
	    open $pod, '<', $Podfile
		or die "$0: cannot open $Podfile file for input: $!\n";
	} else {
	    open $pod, '-';
	}
    } else {
	$Podfile = $ARGV[0];  # XXX: might be more filenames
	$pod = *ARGV;
    }
    $Htmlfile = "-" unless $Htmlfile;	# stdout
    $Htmlroot = "" if $Htmlroot eq "/";	# so we don't get a //
    $Htmldir =~ s#/\z## ;               # so we don't get a //
    if (  $Htmlroot eq ''
       && defined( $Htmldir )
       && $Htmldir ne ''
       && substr( $Htmlfile, 0, length( $Htmldir ) ) eq $Htmldir
       )
    {
	# Set the 'base' url for this file, so that we can use it
	# as the location from which to calculate relative links
	# to other files. If this is '', then absolute links will
	# be used throughout.
        $Htmlfileurl= "$Htmldir/" . substr( $Htmlfile, length( $Htmldir ) + 1);
    }

    # read the pod a paragraph at a time
    warn "Scanning for sections in input file(s)\n" if $Verbose;
    $/ = "";
    my @poddata  = <$pod>;
    close $pod;

    # be eol agnostic
    for (@poddata) {
	if (/\r/) {
	    if (/\r\n/) {
		@poddata = map { s/\r\n/\n/g;
				 /\n\n/ ?
				     map { "$_\n\n" } split /\n\n/ :
				     $_ } @poddata;
	    } else {
		@poddata = map { s/\r/\n/g;
				 /\n\n/ ?
				     map { "$_\n\n" } split /\n\n/ :
				     $_ } @poddata;
	    }
	    last;
	}
    }

    clean_data( \@poddata );

    # scan the pod for =head[1-6] directives and build an index
    my $index = scan_headings(\%Sections, @poddata);

    unless($index) {
	warn "No headings in $Podfile\n" if $Verbose;
    }

    # open the output file
    my $html;
    if($Htmlfile and $Htmlfile ne '-') {
        open $html, ">", $Htmlfile
            or die "$0: cannot open $Htmlfile file for output: $!\n";
    } else {
        open $html, ">-";
    }

    # put a title in the HTML file if one wasn't specified
    if ($Title eq '') {
	TITLE_SEARCH: {
 	    for (my $i = 0; $i < @poddata; $i++) {
		if ($poddata[$i] =~ /^=head1\s*NAME\b/m) {
 		    for my $para ( @poddata[$i, $i+1] ) {
			last TITLE_SEARCH
			    if ($Title) = $para =~ /(\S+\s+-+.*\S)/s;
		    }
		}

	    }
	}
    }
    if (!$Title and $Podfile =~ /\.pod\z/) {
	# probably a split pod so take first =head[12] as title
 	for (my $i = 0; $i < @poddata; $i++) {
	    last if ($Title) = $poddata[$i] =~ /^=head[12]\s*(.*)/;
	}
	warn "adopted '$Title' as title for $Podfile\n"
	    if $Verbose and $Title;
    }
    if ($Title) {
	$Title =~ s/\s*\(.*\)//;
    } else {
	warn "$0: no title for $Podfile.\n" unless $Quiet;
	$Podfile =~ /^(.*)(\.[^.\/]+)?\z/s;
	$Title = ($Podfile eq "-" ? 'No Title' : $1);
	warn "using $Title" if $Verbose;
    }
    $Title = html_escape($Title);

    my $csslink = '';
    my $bodystyle = ' style="background-color: white"';
    my $tdstyle = ' style="background-color: #cccccc"';

    if ($Css) {
      $csslink = qq(\n<link rel="stylesheet" href="$Css" type="text/css" />);
      $csslink =~ s,\\,/,g;
      $csslink =~ s,(/.):,$1|,;
      $bodystyle = '';
      $tdstyle = '';
    }

      my $block = $Header ? <<END_OF_BLOCK : '';
<table border="0" width="100%" cellspacing="0" cellpadding="3">
<tr><td class="block"$tdstyle valign="middle">
<big><strong><span class="block">&nbsp;$Title</span></strong></big>
</td></tr>
</table>
END_OF_BLOCK

    print $html <<END_OF_HEAD;
<?xml version="1.0" ?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>$Title</title>$csslink
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
<link rev="made" href="mailto:$Config{perladmin}" />
</head>

<body$bodystyle>
$block
END_OF_HEAD

    # load/reload/validate/cache %Pages and %Items
    get_cache($Dircache, $Itemcache, \@Podpath, $Podroot, $Recurse);

    # scan the pod for =item directives
    scan_items( \%Local_Items, "", @poddata);

    # put an index at the top of the file.  note, if $Doindex is 0 we
    # still generate an index, but surround it with an html comment.
    # that way some other program can extract it if desired.
    $index =~ s/--+/-/g;

    my $hr = ($Doindex and $index) ? qq(<hr name="index" />) : "";

    unless ($Doindex)
    {
        $index = qq(<!--\n$index\n-->\n);
    }

    print $html <<"END_OF_INDEX";

<!-- INDEX BEGIN -->
<div name="index">
<p><a name=\"__index__\"></a></p>
$index
$hr
</div>
<!-- INDEX END -->

END_OF_INDEX

    # now convert this file
    my $after_item;             # set to true after an =item
    warn "Converting input file $Podfile\n" if $Verbose;
    foreach my $i (0..$#poddata){
	$_ = $poddata[$i];
	$Paragraph = $i+1;
	if (/^(=.*)/s) {	# is it a pod directive?
	    $Ignore = 0;
	    $after_item = 0;
	    $_ = $1;
	    if (/^=begin\s+(\S+)\s*(.*)/si) {# =begin
		process_begin($html, $1, $2);
	    } elsif (/^=end\s+(\S+)\s*(.*)/si) {# =end
		process_end($1, $2);
	    } elsif (/^=cut/) {			# =cut
		process_cut();
	    } elsif (/^=pod/) {			# =pod
		process_pod();
	    } else {
		next if @Begin_Stack && $Begin_Stack[-1] ne 'html';

		if (/^=(head[1-6])\s+(.*\S)/s) {	# =head[1-6] heading
		    process_head( $html, $1, $2, $Doindex && $index );
		} elsif (/^=item\s*(.*\S)?/sm) {	# =item text
		    process_item( $html, $1 );
		    $after_item = 1;
		} elsif (/^=over\s*(.*)/) {		# =over N
		    process_over();
		} elsif (/^=back/) {		# =back
		    process_back( $html );
		} elsif (/^=for\s+(\S+)\s*(.*)/si) {# =for
		    process_for( $html, $1, $2 );
		} else {
		    /^=(\S*)\s*/;
		    warn "$0: $Podfile: unknown pod directive '$1' in "
		       . "paragraph $Paragraph.  ignoring.\n" unless $Quiet;
		}
	    }
	    $Top = 0;
	}
	else {
	    next if $Ignore;
	    if (@Begin_Stack) {
		print $html $_ if $Begin_Stack[-1] eq 'html';
		next;
	    }
	    my $text = $_;

	    # Open tag for definition list as we have something to put in it
	    if( $ListNewTerm ){
		print $html "<dd>\n";
		$ListNewTerm = 0;
	    }

	    if( $text =~ /\A\s+/ ){
		process_pre( \$text );
	        print $html "<pre>\n$text</pre>\n";

	    } else {
		process_text( \$text );

		# experimental: check for a paragraph where all lines
		# have some ...\t...\t...\n pattern
		if( $text =~ /\t/ ){
		    my @lines = split( "\n", $text );
		    if( @lines > 1 ){
			my $all = 2;
			foreach my $line ( @lines ){
			    if( $line =~ /\S/ && $line !~ /\t/ ){
				$all--;
				last if $all == 0;
			    }
			}
			if( $all > 0 ){
			    $text =~ s/\t+/<td>/g;
			    $text =~ s/^/<tr><td>/gm;
			    $text = '<table cellspacing="0" cellpadding="0">' .
                                    $text . '</table>';
			}
		    }
		}
		## end of experimental

		print $html "<p>$text</p>\n";
	    }
	    $after_item = 0;
	}
    }

    # finish off any pending directives
    finish_list( $html );

    # link to page index
    print $html "<p><a href=\"#__index__\"><small>$Backlink</small></a></p>\n"
	if $Doindex and $index and $Backlink;

    print $html <<END_OF_TAIL;
$block
</body>

</html>
END_OF_TAIL

    # close the html file
    close $html or die "Failed to close $Htmlfile: $!";

    warn "Finished\n" if $Verbose;
}

##############################################################################



sub parse_command_line {
    my ($opt_backlink,$opt_cachedir,$opt_css,$opt_flush,$opt_header,$opt_help,
	$opt_htmldir,$opt_htmlroot,$opt_index,$opt_infile,$opt_libpods,
	$opt_netscape,$opt_outfile,$opt_podpath,$opt_podroot,$opt_quiet,
	$opt_recurse,$opt_title,$opt_verbose,$opt_hiddendirs);

    unshift @ARGV, split ' ', $Config{pod2html} if $Config{pod2html};
    my $result = GetOptions(
			    'backlink=s' => \$opt_backlink,
			    'cachedir=s' => \$opt_cachedir,
			    'css=s'      => \$opt_css,
			    'flush'      => \$opt_flush,
			    'header!'    => \$opt_header,
			    'help'       => \$opt_help,
			    'hiddendirs!'=> \$opt_hiddendirs,
			    'htmldir=s'  => \$opt_htmldir,
			    'htmlroot=s' => \$opt_htmlroot,
			    'index!'     => \$opt_index,
			    'infile=s'   => \$opt_infile,
			    'libpods=s'  => \$opt_libpods,
			    'netscape!'  => \$opt_netscape,
			    'outfile=s'  => \$opt_outfile,
			    'podpath=s'  => \$opt_podpath,
			    'podroot=s'  => \$opt_podroot,
			    'quiet!'     => \$opt_quiet,
			    'recurse!'   => \$opt_recurse,
			    'title=s'    => \$opt_title,
			    'verbose!'   => \$opt_verbose,
			   );
    usage("-", "invalid parameters") if not $result;

    usage("-") if defined $opt_help;	# see if the user asked for help
    $opt_help = "";			# just to make -w shut-up.

    @Podpath  = split(":", $opt_podpath) if defined $opt_podpath;
    @Libpods  = split(":", $opt_libpods) if defined $opt_libpods;

    $Backlink = $opt_backlink if defined $opt_backlink;
    $Cachedir = $opt_cachedir if defined $opt_cachedir;
    $Css      = $opt_css      if defined $opt_css;
    $Header   = $opt_header   if defined $opt_header;
    $Htmldir  = $opt_htmldir  if defined $opt_htmldir;
    $Htmlroot = $opt_htmlroot if defined $opt_htmlroot;
    $Doindex  = $opt_index    if defined $opt_index;
    $Podfile  = $opt_infile   if defined $opt_infile;
    $HiddenDirs = $opt_hiddendirs if defined $opt_hiddendirs;
    $Htmlfile = $opt_outfile  if defined $opt_outfile;
    $Podroot  = $opt_podroot  if defined $opt_podroot;
    $Quiet    = $opt_quiet    if defined $opt_quiet;
    $Recurse  = $opt_recurse  if defined $opt_recurse;
    $Title    = $opt_title    if defined $opt_title;
    $Verbose  = $opt_verbose  if defined $opt_verbose;

    warn "Flushing item and directory caches\n"
	if $opt_verbose && defined $opt_flush;
    $Dircache = "$Cachedir/pod2htmd.tmp";
    $Itemcache = "$Cachedir/pod2htmi.tmp";
    if (defined $opt_flush) {
	1 while unlink($Dircache, $Itemcache);
    }
}


my $Saved_Cache_Key;

sub get_cache {
    my($dircache, $itemcache, $podpath, $podroot, $recurse) = @_;
    my @cache_key_args = @_;

    # A first-level cache:
    # Don't bother reading the cache files if they still apply
    # and haven't changed since we last read them.

    my $this_cache_key = cache_key(@cache_key_args);

    return if $Saved_Cache_Key and $this_cache_key eq $Saved_Cache_Key;

    # load the cache of %Pages and %Items if possible.  $tests will be
    # non-zero if successful.
    my $tests = 0;
    if (-f $dircache && -f $itemcache) {
	warn "scanning for item cache\n" if $Verbose;
	$tests = load_cache($dircache, $itemcache, $podpath, $podroot);
    }

    # if we didn't succeed in loading the cache then we must (re)build
    #  %Pages and %Items.
    if (!$tests) {
	warn "scanning directories in pod-path\n" if $Verbose;
	scan_podpath($podroot, $recurse, 0);
    }
    $Saved_Cache_Key = cache_key(@cache_key_args);
}

sub cache_key {
    my($dircache, $itemcache, $podpath, $podroot, $recurse) = @_;
    return join('!', $dircache, $itemcache, $recurse,
	@$podpath, $podroot, stat($dircache), stat($itemcache));
}

#
# load_cache - tries to find if the caches stored in $dircache and $itemcache
#  are valid caches of %Pages and %Items.  if they are valid then it loads
#  them and returns a non-zero value.
#
sub load_cache {
    my($dircache, $itemcache, $podpath, $podroot) = @_;
    my($tests);
    local $_;

    $tests = 0;

    open(CACHE, "<$itemcache") ||
	die "$0: error opening $itemcache for reading: $!\n";
    $/ = "\n";

    # is it the same podpath?
    $_ = <CACHE>;
    chomp($_);
    $tests++ if (join(":", @$podpath) eq $_);

    # is it the same podroot?
    $_ = <CACHE>;
    chomp($_);
    $tests++ if ($podroot eq $_);

    # load the cache if its good
    if ($tests != 2) {
	close(CACHE);
	return 0;
    }

    warn "loading item cache\n" if $Verbose;
    while (<CACHE>) {
	/(.*?) (.*)$/;
	$Items{$1} = $2;
    }
    close(CACHE);

    warn "scanning for directory cache\n" if $Verbose;
    open(CACHE, "<$dircache") ||
	die "$0: error opening $dircache for reading: $!\n";
    $/ = "\n";
    $tests = 0;

    # is it the same podpath?
    $_ = <CACHE>;
    chomp($_);
    $tests++ if (join(":", @$podpath) eq $_);

    # is it the same podroot?
    $_ = <CACHE>;
    chomp($_);
    $tests++ if ($podroot eq $_);

    # load the cache if its good
    if ($tests != 2) {
	close(CACHE);
	return 0;
    }

    warn "loading directory cache\n" if $Verbose;
    while (<CACHE>) {
	/(.*?) (.*)$/;
	$Pages{$1} = $2;
    }

    close(CACHE);

    return 1;
}

#
# scan_podpath - scans the directories specified in @podpath for directories,
#  .pod files, and .pm files.  it also scans the pod files specified in
#  @Libpods for =item directives.
#
sub scan_podpath {
    my($podroot, $recurse, $append) = @_;
    my($pwd, $dir);
    my($libpod, $dirname, $pod, @files, @poddata);

    unless($append) {
	%Items = ();
	%Pages = ();
    }

    # scan each directory listed in @Podpath
    $pwd = getcwd();
    chdir($podroot)
	|| die "$0: error changing to directory $podroot: $!\n";
    foreach $dir (@Podpath) {
	scan_dir($dir, $recurse);
    }

    # scan the pods listed in @Libpods for =item directives
    foreach $libpod (@Libpods) {
	# if the page isn't defined then we won't know where to find it
	# on the system.
	next unless defined $Pages{$libpod} && $Pages{$libpod};

	# if there is a directory then use the .pod and .pm files within it.
	# NOTE: Only finds the first so-named directory in the tree.
#	if ($Pages{$libpod} =~ /([^:]*[^(\.pod|\.pm)]):/) {
	if ($Pages{$libpod} =~ /([^:]*(?<!\.pod)(?<!\.pm)):/) {
	    #  find all the .pod and .pm files within the directory
	    $dirname = $1;
	    opendir(DIR, $dirname) ||
		die "$0: error opening directory $dirname: $!\n";
	    @files = grep(/(\.pod|\.pm)\z/ && ! -d $_, readdir(DIR));
	    closedir(DIR);

	    # scan each .pod and .pm file for =item directives
	    foreach $pod (@files) {
		open my $fh, '<', "$dirname/$pod"
		    or die "$0: error opening $dirname/$pod for input: $!\n";
		@poddata = <$fh>;
		close $fh;
		clean_data( \@poddata );

		scan_items( \%Items, "$dirname/$pod", @poddata);
	    }

	    # use the names of files as =item directives too.
### Don't think this should be done this way - confuses issues.(WL)
###	    foreach $pod (@files) {
###		$pod =~ /^(.*)(\.pod|\.pm)$/;
###		$Items{$1} = "$dirname/$1.html" if $1;
###	    }
	} elsif ($Pages{$libpod} =~ /([^:]*\.pod):/ ||
		 $Pages{$libpod} =~ /([^:]*\.pm):/) {
	    # scan the .pod or .pm file for =item directives
	    $pod = $1;
	    open my $fh, '<', $pod
		or die "$0: error opening $pod for input: $!\n";
	    @poddata = <$fh>;
	    close $fh;
	    clean_data( \@poddata );

	    scan_items( \%Items, "$pod", @poddata);
	} else {
	    warn "$0: shouldn't be here (line ".__LINE__."\n" unless $Quiet;
	}
    }
    @poddata = ();	# clean-up a bit

    chdir($pwd)
	|| die "$0: error changing to directory $pwd: $!\n";

    # cache the item list for later use
    warn "caching items for later use\n" if $Verbose;
    open my $cache, '>', $Itemcache
	or die "$0: error open $Itemcache for writing: $!\n";

    print $cache join(":", @Podpath) . "\n$podroot\n";
    foreach my $key (keys %Items) {
	print $cache "$key $Items{$key}\n";
    }

    close $cache or die "error closing $Itemcache: $!";

    # cache the directory list for later use
    warn "caching directories for later use\n" if $Verbose;
    open $cache, '>', $Dircache
	or die "$0: error open $Dircache for writing: $!\n";

    print $cache join(":", @Podpath) . "\n$podroot\n";
    foreach my $key (keys %Pages) {
	print $cache "$key $Pages{$key}\n";
    }

    close $cache or die "error closing $Dircache: $!";
}

#
# scan_dir - scans the directory specified in $dir for subdirectories, .pod
#  files, and .pm files.  notes those that it finds.  this information will
#  be used later in order to figure out where the pages specified in L<>
#  links are on the filesystem.
#
sub scan_dir {
    my($dir, $recurse) = @_;
    my($t, @subdirs, @pods, $pod, $dirname, @dirs);
    local $_;

    @subdirs = ();
    @pods = ();

    opendir(DIR, $dir) ||
	die "$0: error opening directory $dir: $!\n";
    while (defined($_ = readdir(DIR))) {
	if (-d "$dir/$_" && $_ ne "." && $_ ne ".."
	    && ($HiddenDirs || !/^\./)
	) {         # directory
	    $Pages{$_}  = "" unless defined $Pages{$_};
	    $Pages{$_} .= "$dir/$_:";
	    push(@subdirs, $_);
	} elsif (/\.pod\z/) {	    	    	    	    # .pod
	    s/\.pod\z//;
	    $Pages{$_}  = "" unless defined $Pages{$_};
	    $Pages{$_} .= "$dir/$_.pod:";
	    push(@pods, "$dir/$_.pod");
	} elsif (/\.html\z/) { 	    	    	    	    # .html
	    s/\.html\z//;
	    $Pages{$_}  = "" unless defined $Pages{$_};
	    $Pages{$_} .= "$dir/$_.pod:";
	} elsif (/\.pm\z/) { 	    	    	    	    # .pm
	    s/\.pm\z//;
	    $Pages{$_}  = "" unless defined $Pages{$_};
	    $Pages{$_} .= "$dir/$_.pm:";
	    push(@pods, "$dir/$_.pm");
	} elsif (-T "$dir/$_") {			    # script(?)
	    local *F;
	    if (open(F, "$dir/$_")) {
		my $line;
		while (defined($line = <F>)) {
		    if ($line =~ /^=(?:pod|head1)/) {
			$Pages{$_}  = "" unless defined $Pages{$_};
			$Pages{$_} .= "$dir/$_.pod:";
			last;
		    }
		}
		close(F);
	    }
	}
    }
    closedir(DIR);

    # recurse on the subdirectories if necessary
    if ($recurse) {
	foreach my $subdir (@subdirs) {
	    scan_dir("$dir/$subdir", $recurse);
	}
    }
}

#
# scan_headings - scan a pod file for head[1-6] tags, note the tags, and
#  build an index.
#
sub scan_headings {
    my($sections, @data) = @_;
    my($tag, $which_head, $otitle, $listdepth, $index);

    local $Ignore = 0;

    $listdepth = 0;
    $index = "";

    # scan for =head directives, note their name, and build an index
    #  pointing to each of them.
    foreach my $line (@data) {
      if ($line =~ /^=(head)([1-6])\s+(.*)/) {
        ($tag, $which_head, $otitle) = ($1,$2,$3);

        my $title = depod( $otitle );
        my $name = anchorify( $title );
        $$sections{$name} = 1;
        $title = process_text( \$otitle );

	    while ($which_head != $listdepth) {
		if ($which_head > $listdepth) {
		    $index .= "\n" . ("\t" x $listdepth) . "<ul>\n";
		    $listdepth++;
		} elsif ($which_head < $listdepth) {
		    $listdepth--;
		    $index .= "\n" . ("\t" x $listdepth) . "</ul>\n";
		}
	    }

	    $index .= "\n" . ("\t" x $listdepth) . "<li>" .
	              "<a href=\"#" . $name . "\">" .
		      $title . "</a></li>";
	}
    }

    # finish off the lists
    while ($listdepth--) {
	$index .= "\n" . ("\t" x $listdepth) . "</ul>\n";
    }

    # get rid of bogus lists
    $index =~ s,\t*<ul>\s*</ul>\n,,g;

    return $index;
}

#
# scan_items - scans the pod specified by $pod for =item directives.  we
#  will use this information later on in resolving C<> links.
#
sub scan_items {
    my( $itemref, $pod, @poddata ) = @_;
    my($i, $item);
    local $_;

    $pod =~ s/\.pod\z//;
    $pod .= ".html" if $pod;

    foreach $i (0..$#poddata) {
	my $txt = depod( $poddata[$i] );

	# figure out what kind of item it is.
	# Build string for referencing this item.
	if ( $txt =~ /\A=item\s+\*\s*(.*)\Z/s ) { # bulleted list
	    next unless $1;
	    $item = $1;
        } elsif( $txt =~ /\A=item\s+(?>\d+\.?)\s*(.*)\Z/s ) { # numbered list
	    $item = $1;
	} elsif( $txt =~ /\A=item\s+(.*)\Z/s ) { # definition list
	    $item = $1;
	} else {
	    next;
	}
	my $fid = fragment_id( $item );
	$$itemref{$fid} = "$pod" if $fid;
    }
}

#
# process_head - convert a pod head[1-6] tag and convert it to HTML format.
#
sub process_head {
    my($fh, $tag, $heading, $hasindex) = @_;

    # figure out the level of the =head
    $tag =~ /head([1-6])/;
    my $level = $1;

    finish_list( $fh );

    print $fh "<p>\n";
    if( $level == 1 && ! $Top ){
      print $fh "<a href=\"#__index__\"><small>$Backlink</small></a>\n"
        if $hasindex and $Backlink;
      print $fh "</p>\n<hr />\n"
    } else {
      print $fh "</p>\n";
    }

    my $name = anchorify( depod( $heading ) );
    my $convert = process_text( \$heading );
    print $fh "<h$level><a name=\"$name\">$convert</a></h$level>\n";
}


#
# emit_item_tag - print an =item's text
# Note: The global $EmittedItem is used for inhibiting self-references.
#
my $EmittedItem;

sub emit_item_tag {
    my( $fh, $otext, $text, $compact ) = @_;
    my $item = fragment_id( depod($text) , -generate);
    Carp::confess("Undefined fragment '$text' (".depod($text).") from fragment_id() in emit_item_tag() in $Podfile")
        if !defined $item;
    $EmittedItem = $item;
    ### print STDERR "emit_item_tag=$item ($text)\n";

    print $fh '<strong>';
    if ($Items_Named{$item}++) {
	print $fh &process_text( \$otext );
    } else {
        my $name = $item;
        $name = anchorify($name);
	print $fh qq{<a name="$name" class="item">}, process_text( \$otext ), '</a>';
    }
    print $fh "</strong>";
    undef( $EmittedItem );
}

sub new_listitem {
    my ($fh, $tag) = @_;
    # Open tag for definition list as we have something to put in it
    if( ($tag ne 'dl') && ($ListNewTerm) ){
	print $fh "<dd>\n";
	$ListNewTerm = 0;
    }

    if( $Items_Seen[$Listlevel]++ == 0 ){
	# start of new list
	push( @Listtype, "$tag" );
	print $fh "<$tag>\n";
    } else {
	# if this is not the first item, close the previous one
	if ( $tag eq 'dl' ){
	    print $fh "</dd>\n" unless $ListNewTerm;
	} else {
	    print $fh "</li>\n";
	}
    }
    my $opentag = $tag eq 'dl' ? 'dt' : 'li';
    print $fh "<$opentag>";
}

#
# process_item - convert a pod item tag and convert it to HTML format.
#
sub process_item {
    my ($fh, $otext) = @_;

    # lots of documents start a list without doing an =over.  this is
    # bad!  but, the proper thing to do seems to be to just assume
    # they did do an =over.  so warn them once and then continue.
    if( $Listlevel == 0 ){
	warn "$0: $Podfile: unexpected =item directive in paragraph $Paragraph.  ignoring.\n" unless $Quiet;
	process_over();
    }

    # remove formatting instructions from the text
    my $text = depod( $otext );

    # all the list variants:
    if( $text =~ /\A\*/ ){ # bullet
        new_listitem( $fh, 'ul' );
        if ($text =~ /\A\*\s+(\S.*)\Z/s ) { # with additional text
            my $tag = $1;
            $otext =~ s/\A\*\s+//;
            emit_item_tag( $fh, $otext, $tag, 1 );
            print $fh "\n";
        }

    } elsif( $text =~ /\A\d+/ ){ # numbered list
        new_listitem( $fh, 'ol' );
        if ($text =~ /\A(?>\d+\.?)\s*(\S.*)\Z/s ) { # with additional text
            my $tag = $1;
            $otext =~ s/\A\d+\.?\s*//;
            emit_item_tag( $fh, $otext, $tag, 1 );
            print $fh "\n";
        }

    } else {			# definition list
        # new_listitem takes care of opening the <dt> tag
        new_listitem( $fh, 'dl' );
        if ($text =~ /\A(.+)\Z/s ){ # should have text
            emit_item_tag( $fh, $otext, $text, 1 );
	    # write the definition term and close <dt> tag
	    print $fh "</dt>\n";
        }
        # trigger opening a <dd> tag for the actual definition; will not
        # happen if next paragraph is also a definition term (=item)
        $ListNewTerm = 1;
    }
    print $fh "\n";
}

#
# process_over - process a pod over tag and start a corresponding HTML list.
#
sub process_over {
    # start a new list
    $Listlevel++;
    push( @Items_Seen, 0 );
}

#
# process_back - process a pod back tag and convert it to HTML format.
#
sub process_back {
    my $fh = shift;
    if( $Listlevel == 0 ){
	warn "$0: $Podfile: unexpected =back directive in paragraph $Paragraph.  ignoring.\n" unless $Quiet;
	return;
    }

    # close off the list.  note, I check to see if $Listtype[$Listlevel] is
    # defined because an =item directive may have never appeared and thus
    # $Listtype[$Listlevel] may have never been initialized.
    $Listlevel--;
    if( defined $Listtype[$Listlevel] ){
        if ( $Listtype[$Listlevel] eq 'dl' ){
            print $fh "</dd>\n" unless $ListNewTerm;
        } else {
            print $fh "</li>\n";
        }
        print $fh "</$Listtype[$Listlevel]>\n";
        pop( @Listtype );
        $ListNewTerm = 0;
    }

    # clean up item count
    pop( @Items_Seen );
}

#
# process_cut - process a pod cut tag, thus start ignoring pod directives.
#
sub process_cut {
    $Ignore = 1;
}

#
# process_pod - process a pod tag, thus stop ignoring pod directives
# until we see a corresponding cut.
#
sub process_pod {
    # no need to set $Ignore to 0 cause the main loop did it
}

#
# process_for - process a =for pod tag.  if it's for html, spit
# it out verbatim, if illustration, center it, otherwise ignore it.
#
sub process_for {
    my ($fh, $whom, $text) = @_;
    if ( $whom =~ /^(pod2)?html$/i) {
	print $fh $text;
    } elsif ($whom =~ /^illustration$/i) {
        1 while chomp $text;
	for my $ext (qw[.png .gif .jpeg .jpg .tga .pcl .bmp]) {
	  $text .= $ext, last if -r "$text$ext";
	}
        print $fh qq{<p align="center"><img src="$text" alt="$text illustration" /></p>};
    }
}

#
# process_begin - process a =begin pod tag.  this pushes
# whom we're beginning on the begin stack.  if there's a
# begin stack, we only print if it us.
#
sub process_begin {
    my ($fh, $whom, $text) = @_;
    $whom = lc($whom);
    push (@Begin_Stack, $whom);
    if ( $whom =~ /^(pod2)?html$/) {
	print $fh $text if $text;
    }
}

#
# process_end - process a =end pod tag.  pop the
# begin stack.  die if we're mismatched.
#
sub process_end {
    my($whom, $text) = @_;
    $whom = lc($whom);
    if (!defined $Begin_Stack[-1] or $Begin_Stack[-1] ne $whom ) {
	Carp::confess("Unmatched begin/end at chunk $Paragraph in pod $Podfile\n")
    }
    pop( @Begin_Stack );
}

#
# process_pre - indented paragraph, made into <pre></pre>
#
sub process_pre {
    my( $text ) = @_;
    my( $rest );
    return if $Ignore;

    $rest = $$text;

    # insert spaces in place of tabs
    $rest =~ s#(.+)#
	    my $line = $1;
            1 while $line =~ s/(\t+)/' ' x ((length($1) * 8) - $-[0] % 8)/e;
	    $line;
	#eg;

    # convert some special chars to HTML escapes
    $rest = html_escape($rest);

    # try and create links for all occurrences of perl.* within
    # the preformatted text.
    $rest =~ s{
	         (\s*)(perl\w+)
	      }{
		 if ( defined $Pages{$2} ){	# is a link
		     qq($1<a href="$Htmlroot/$Pages{$2}">$2</a>);
		 } elsif (defined $Pages{dosify($2)}) {	# is a link
		     qq($1<a href="$Htmlroot/$Pages{dosify($2)}">$2</a>);
		 } else {
		     "$1$2";
		 }
	      }xeg;
     $rest =~ s{
		 (<a\ href="?) ([^>:]*:)? ([^>:]*) \.pod: ([^>:]*:)?
               }{
                  my $url ;
                  if ( $Htmlfileurl ne '' ){
		     # Here, we take advantage of the knowledge
		     # that $Htmlfileurl ne '' implies $Htmlroot eq ''.
		     # Since $Htmlroot eq '', we need to prepend $Htmldir
		     # on the fron of the link to get the absolute path
		     # of the link's target. We check for a leading '/'
		     # to avoid corrupting links that are #, file:, etc.
		     my $old_url = $3 ;
		     $old_url = "$Htmldir$old_url" if $old_url =~ m{^\/};
 		     $url = relativize_url( "$old_url.html", $Htmlfileurl );
	          } else {
		     $url = "$3.html" ;
		  }
		  "$1$url" ;
	       }xeg;

    # Look for embedded URLs and make them into links.  We don't
    # relativize them since they are best left as the author intended.

    my $urls = '(' . join ('|', qw{
                http
                telnet
		mailto
		news
                gopher
                file
                wais
                ftp
            } )
        . ')';

    my $ltrs = '\w';
    my $gunk = '/#~:.?+=&%@!\-';
    my $punc = '.:!?\-;';
    my $any  = "${ltrs}${gunk}${punc}";

    $rest =~ s{
	\b			# start at word boundary
	(			# begin $1  {
	    $urls :		# need resource and a colon
	    (?!:)		# Ignore File::, among others.
	    [$any] +?		# followed by one or more of any valid
				#   character, but be conservative and
				#   take only what you need to....
	)			# end   $1  }
	(?=
	    &quot; &gt;		# maybe pre-quoted '<a href="...">'
	|			# or:
	    [$punc]*		# 0 or more punctuation
	    (?:			#   followed
		[^$any]		#   by a non-url char
	    |			#   or
		$		#   end of the string
	    )			#
	|			# or else
	    $			#   then end of the string
        )
      }{<a href="$1">$1</a>}igox;

    # text should be as it is (verbatim)
    $$text = $rest;
}


#
# pure text processing
#
# pure_text/inIS_text: differ with respect to automatic C<> recognition.
# we don't want this to happen within IS
#
sub pure_text($){
    my $text = shift();
    process_puretext( $text, 1 );
}

sub inIS_text($){
    my $text = shift();
    process_puretext( $text, 0 );
}

#
# process_puretext - process pure text (without pod-escapes) converting
#  double-quotes and handling implicit C<> links.
#
sub process_puretext {
    my($text, $notinIS) = @_;

    ## Guessing at func() or [\$\@%&]*var references in plain text is destined
    ## to produce some strange looking ref's. uncomment to disable:
    ## $notinIS = 0;

    my(@words, $lead, $trail);

    # keep track of leading and trailing white-space
    $lead  = ($text =~ s/\A(\s+)//s ? $1 : "");
    $trail = ($text =~ s/(\s+)\Z//s ? $1 : "");

    # split at space/non-space boundaries
    @words = split( /(?<=\s)(?=\S)|(?<=\S)(?=\s)/, $text );

    # process each word individually
    foreach my $word (@words) {
	# skip space runs
 	next if $word =~ /^\s*$/;
	# see if we can infer a link or a function call
	#
	# NOTE: This is a word based search, it won't automatically
	# mark "substr($var, 1, 2)" because the 1st word would be "substr($var"
	# User has to enclose those with proper C<>

	if( $notinIS && $word =~
	    m/
		^([a-z_]{2,})                 # The function name
		\(
		    ([0-9][a-z]*              # Manual page(1) or page(1M)
		    |[^)]*[\$\@\%][^)]+       # ($foo), (1, @foo), (%hash)
		    |                         # ()
		    )
		\)
		([.,;]?)$                     # a possible punctuation follows
	    /xi
	) {
	    # has parenthesis so should have been a C<> ref
            ## try for a pagename (perlXXX(1))?
            my( $func, $args, $rest ) = ( $1, $2, $3 || '' );
            if( $args =~ /^\d+$/ ){
                my $url = page_sect( $word, '' );
                if( defined $url ){
                    $word = qq(<a href="$url" class="man">the $word manpage</a>$rest);
                    next;
                }
            }
            ## try function name for a link, append tt'ed argument list
            $word = emit_C( $func, '', "($args)") . $rest;

#### disabled. either all (including $\W, $\w+{.*} etc.) or nothing.
##      } elsif( $notinIS && $word =~ /^[\$\@%&*]+\w+$/) {
##	    # perl variables, should be a C<> ref
##	    $word = emit_C( $word );

	} elsif ($word =~ m,^\w+://\w,) {
	    # looks like a URL
            # Don't relativize it: leave it as the author intended
	    $word = qq(<a href="$word">$word</a>);
	} elsif ($word =~ /[\w.-]+\@[\w-]+\.\w/) {
	    # looks like an e-mail address
	    my ($w1, $w2, $w3) = ("", $word, "");
	    ($w1, $w2, $w3) = ("(", $1, ")$2") if $word =~ /^\((.*?)\)(,?)/;
	    ($w1, $w2, $w3) = ("&lt;", $1, "&gt;$2") if $word =~ /^<(.*?)>(,?)/;
	    $word = qq($w1<a href="mailto:$w2">$w2</a>$w3);
	} else {
	    $word = html_escape($word) if $word =~ /["&<>]/;
	}
    }

    # put everything back together
    return $lead . join( '', @words ) . $trail;
}


#
# process_text - handles plaintext that appears in the input pod file.
# there may be pod commands embedded within the text so those must be
# converted to html commands.
#

sub process_text1($$;$$);
sub pattern ($) { $_[0] ? '\s+'.('>' x ($_[0] + 1)) : '>' }
sub closing ($) { local($_) = shift; (defined && s/\s+\z//) ? length : 0 }

sub process_text {
    return if $Ignore;
    my( $tref ) = @_;
    my $res = process_text1( 0, $tref );
    $res =~ s/\s+$//s;
    $$tref = $res;
}

sub process_text_rfc_links {
    my $text = shift;

    # For every "RFCnnnn" or "RFC nnn", link it to the authoritative
    # ource. Do not use the /i modifier here. Require "RFC" to be written in
    #  in capital letters.

    $text =~ s{
	(?<=[^<>[:alpha:]])           # Make sure this is not an URL already
	(RFC\s*([0-9]{1,5}))(?![0-9]) # max 5 digits
    }
    {<a href="http://www.ietf.org/rfc/rfc$2.txt" class="rfc">$1</a>}gx;

    $text;
}

sub process_text1($$;$$){
    my( $lev, $rstr, $func, $closing ) = @_;
    my $res = '';

    unless (defined $func) {
	$func = '';
	$lev++;
    }

    if( $func eq 'B' ){
	# B<text> - boldface
	$res = '<strong>' . process_text1( $lev, $rstr ) . '</strong>';

    } elsif( $func eq 'C' ){
	# C<code> - can be a ref or <code></code>
	# need to extract text
	my $par = go_ahead( $rstr, 'C', $closing );

	## clean-up of the link target
        my $text = depod( $par );

	### my $x = $par =~ /[BI]</ ? 'yes' : 'no' ;
        ### print STDERR "-->call emit_C($par) lev=$lev, par with BI=$x\n";

	$res = emit_C( $text, $lev > 1 || ($par =~ /[BI]</) );

    } elsif( $func eq 'E' ){
	# E<x> - convert to character
	$$rstr =~ s/^([^>]*)>//;
	my $escape = $1;
	$escape =~ s/^0?x([\dA-F]+)$/#x$1/i
	or $escape =~ s/^0([0-7]+)$/'#'.oct($1)/ei
	or $escape =~ s/^(\d+)$/#$1/;
	$res = "&$escape;";

    } elsif( $func eq 'F' ){
	# F<filename> - italicize
	$res = '<em class="file">' . process_text1( $lev, $rstr ) . '</em>';

    } elsif( $func eq 'I' ){
	# I<text> - italicize
	$res = '<em>' . process_text1( $lev, $rstr ) . '</em>';

    } elsif( $func eq 'L' ){
	# L<link> - link
	## L<text|cross-ref> => produce text, use cross-ref for linking
	## L<cross-ref> => make text from cross-ref
	## need to extract text
	my $par = go_ahead( $rstr, 'L', $closing );

        # some L<>'s that shouldn't be:
	# a) full-blown URL's are emitted as-is
        if( $par =~ m{^\w+://}s ){
	    return make_URL_href( $par );
	}
        # b) C<...> is stripped and treated as C<>
        if( $par =~ /^C<(.*)>$/ ){
	    my $text = depod( $1 );
 	    return emit_C( $text, $lev > 1 || ($par =~ /[BI]</) );
	}

	# analyze the contents
	$par =~ s/\n/ /g;   # undo word-wrapped tags
        my $opar = $par;
	my $linktext;
	if( $par =~ s{^([^|]+)\|}{} ){
	    $linktext = $1;
	}

	# make sure sections start with a /
	$par =~ s{^"}{/"};

	my( $page, $section, $ident );

	# check for link patterns
	if( $par =~ m{^([^/]+?)/(?!")(.*?)$} ){     # name/ident
            # we've got a name/ident (no quotes)
            if (length $2) {
                ( $page, $ident ) = ( $1, $2 );
            } else {
                ( $page, $section ) = ( $1, $2 );
            }
            ### print STDERR "--> L<$par> to page $page, ident $ident\n";

	} elsif( $par =~ m{^(.*?)/"?(.*?)"?$} ){ # [name]/"section"
            # even though this should be a "section", we go for ident first
	    ( $page, $ident ) = ( $1, $2 );
            ### print STDERR "--> L<$par> to page $page, section $section\n";

	} elsif( $par =~ /\s/ ){  # this must be a section with missing quotes
	    ( $page, $section ) = ( '', $par );
            ### print STDERR "--> L<$par> to void page, section $section\n";

        } else {
	    ( $page, $section ) = ( $par, '' );
            ### print STDERR "--> L<$par> to page $par, void section\n";
	}

        # now, either $section or $ident is defined. the convoluted logic
        # below tries to resolve L<> according to what the user specified.
        # failing this, we try to find the next best thing...
        my( $url, $ltext, $fid );

        RESOLVE: {
            if( defined $ident ){
                ## try to resolve $ident as an item
	        ( $url, $fid ) = coderef( $page, $ident );
                if( $url ){
                    if( ! defined( $linktext ) ){
                        $linktext = $ident;
                        $linktext .= " in " if $ident && $page;
                        $linktext .= "the $page manpage" if $page;
                    }
                    ###  print STDERR "got coderef url=$url\n";
                    last RESOLVE;
                }
                ## no luck: go for a section (auto-quoting!)
                $section = $ident;
            }
            ## now go for a section
            my $htmlsection = htmlify( $section );
 	    $url = page_sect( $page, $htmlsection );
            if( $url ){
                if( ! defined( $linktext ) ){
                    $linktext = $section;
                    $linktext .= " in " if $section && $page;
                    $linktext .= "the $page manpage" if $page;
                }
                ### print STDERR "got page/section url=$url\n";
                last RESOLVE;
            }
            ## no luck: go for an ident
            if( $section ){
                $ident = $section;
            } else {
                $ident = $page;
                $page  = undef();
            }
            ( $url, $fid ) = coderef( $page, $ident );
            if( $url ){
                if( ! defined( $linktext ) ){
                    $linktext = $ident;
                    $linktext .= " in " if $ident && $page;
                    $linktext .= "the $page manpage" if $page;
                }
                ### print STDERR "got section=>coderef url=$url\n";
                last RESOLVE;
            }

            # warning; show some text.
            $linktext = $opar unless defined $linktext;
            warn "$0: $Podfile: cannot resolve L<$opar> in paragraph $Paragraph.\n" unless $Quiet;
        }

        # now we have a URL or just plain code
        $$rstr = $linktext . '>' . $$rstr;
        if( defined( $url ) ){
            $res = "<a href=\"$url\">" . process_text1( $lev, $rstr ) . '</a>';
        } else {
	    $res = '<em>' . process_text1( $lev, $rstr ) . '</em>';
        }

    } elsif( $func eq 'S' ){
	# S<text> - non-breaking spaces
	$res = process_text1( $lev, $rstr );
	$res =~ s/ /&nbsp;/g;

    } elsif( $func eq 'X' ){
	# X<> - ignore
	warn "$0: $Podfile: invalid X<> in paragraph $Paragraph.\n"
	    unless $$rstr =~ s/^[^>]*>// or $Quiet;
    } elsif( $func eq 'Z' ){
	# Z<> - empty
	warn "$0: $Podfile: invalid Z<> in paragraph $Paragraph.\n"
	    unless $$rstr =~ s/^>// or $Quiet;

    } else {
        my $term = pattern $closing;
	while( $$rstr =~ s/\A(.*?)(([BCEFILSXZ])<(<+[^\S\n]+)?|$term)//s ){
	    # all others: either recurse into new function or
	    # terminate at closing angle bracket(s)
	    my $pt = $1;
            $pt .= $2 if !$3 &&  $lev == 1;
	    $res .= $lev == 1 ? pure_text( $pt ) : inIS_text( $pt );
	    return $res if !$3 && $lev > 1;
            if( $3 ){
		$res .= process_text1( $lev, $rstr, $3, closing $4 );
 	    }
	}
	if( $lev == 1 ){
	    $res .= pure_text( $$rstr );
	} elsif( ! $Quiet ) {
            my $snippet = substr($$rstr,0,60);
            warn "$0: $Podfile: undelimited $func<> in paragraph $Paragraph: '$snippet'.\n"

	}
	$res = process_text_rfc_links($res);
    }
    return $res;
}

#
# go_ahead: extract text of an IS (can be nested)
#
sub go_ahead($$$){
    my( $rstr, $func, $closing ) = @_;
    my $res = '';
    my @closing = ($closing);
    while( $$rstr =~
      s/\A(.*?)(([BCEFILSXZ])<(<+\s+)?|@{[pattern $closing[0]]})//s ){
	$res .= $1;
	unless( $3 ){
	    shift @closing;
	    return $res unless @closing;
	} else {
	    unshift @closing, closing $4;
	}
	$res .= $2;
    }
    unless ($Quiet) {
        my $snippet = substr($$rstr,0,60);
        warn "$0: $Podfile: undelimited $func<> in paragraph $Paragraph (go_ahead): '$snippet'.\n"
    }
    return $res;
}

#
# emit_C - output result of C<text>
#    $text is the depod-ed text
#
sub emit_C($;$$){
    my( $text, $nocode, $args ) = @_;
    $args = '' unless defined $args;
    my $res;
    my( $url, $fid ) = coderef( undef(), $text );

    # need HTML-safe text
    my $linktext = html_escape( "$text$args" );

    if( defined( $url ) &&
        (!defined( $EmittedItem ) || $EmittedItem ne $fid ) ){
	$res = "<a href=\"$url\"><code>$linktext</code></a>";
    } elsif( 0 && $nocode ){
	$res = $linktext;
    } else {
	$res = "<code>$linktext</code>";
    }
    return $res;
}

#
# html_escape: make text safe for HTML
#
sub html_escape {
    my $rest = $_[0];
    $rest   =~ s/&/&amp;/g;
    $rest   =~ s/</&lt;/g;
    $rest   =~ s/>/&gt;/g;
    $rest   =~ s/"/&quot;/g;
    # &apos; is only in XHTML, not HTML4.  Be conservative
    #$rest   =~ s/'/&apos;/g;
    return $rest;
}


#
# dosify - convert filenames to 8.3
#
sub dosify {
    my($str) = @_;
    return lc($str) if $^O eq 'VMS';     # VMS just needs casing
    if ($Is83) {
        $str = lc $str;
        $str =~ s/(\.\w+)/substr ($1,0,4)/ge;
        $str =~ s/(\w+)/substr ($1,0,8)/ge;
    }
    return $str;
}

#
# page_sect - make a URL from the text of a L<>
#
sub page_sect($$) {
    my( $page, $section ) = @_;
    my( $linktext, $page83, $link);	# work strings

    # check if we know that this is a section in this page
    if (!defined $Pages{$page} && defined $Sections{$page}) {
	$section = $page;
	$page = "";
        ### print STDERR "reset page='', section=$section\n";
    }

    $page83=dosify($page);
    $page=$page83 if (defined $Pages{$page83});
    if ($page eq "") {
        $link = "#" . anchorify( $section );
    } elsif ( $page =~ /::/ ) {
	$page =~ s,::,/,g;
	# Search page cache for an entry keyed under the html page name,
	# then look to see what directory that page might be in.  NOTE:
	# this will only find one page. A better solution might be to produce
	# an intermediate page that is an index to all such pages.
	my $page_name = $page ;
	$page_name =~ s,^.*/,,s ;
	if ( defined( $Pages{ $page_name } ) &&
	     $Pages{ $page_name } =~ /([^:]*$page)\.(?:pod|pm):/
	   ) {
	    $page = $1 ;
	}
	else {
	    # NOTE: This branch assumes that all A::B pages are located in
	    # $Htmlroot/A/B.html . This is often incorrect, since they are
	    # often in $Htmlroot/lib/A/B.html or such like. Perhaps we could
	    # analyze the contents of %Pages and figure out where any
	    # cousins of A::B are, then assume that.  So, if A::B isn't found,
	    # but A::C is found in lib/A/C.pm, then A::B is assumed to be in
	    # lib/A/B.pm. This is also limited, but it's an improvement.
	    # Maybe a hints file so that the links point to the correct places
	    # nonetheless?

	}
	$link = "$Htmlroot/$page.html";
	$link .= "#" . anchorify( $section ) if ($section);
    } elsif (!defined $Pages{$page}) {
	$link = "";
    } else {
	$section = anchorify( $section ) if $section ne "";
        ### print STDERR "...section=$section\n";

	# if there is a directory by the name of the page, then assume that an
	# appropriate section will exist in the subdirectory
#	if ($section ne "" && $Pages{$page} =~ /([^:]*[^(\.pod|\.pm)]):/) {
	if ($section ne "" && $Pages{$page} =~ /([^:]*(?<!\.pod)(?<!\.pm)):/) {
	    $link = "$Htmlroot/$1/$section.html";
            ### print STDERR "...link=$link\n";

	# since there is no directory by the name of the page, the section will
	# have to exist within a .html of the same name.  thus, make sure there
	# is a .pod or .pm that might become that .html
	} else {
	    $section = "#$section" if $section;
            ### print STDERR "...section=$section\n";

	    # check if there is a .pod with the page name.
	    # for L<Foo>, Foo.(pod|pm) is preferred to A/Foo.(pod|pm)
	    if ($Pages{$page} =~ /([^:]*)\.(?:pod|pm):/) {
		$link = "$Htmlroot/$1.html$section";
	    } else {
		$link = "";
	    }
	}
    }

    if ($link) {
	# Here, we take advantage of the knowledge that $Htmlfileurl ne ''
	# implies $Htmlroot eq ''. This means that the link in question
	# needs a prefix of $Htmldir if it begins with '/'. The test for
	# the initial '/' is done to avoid '#'-only links, and to allow
	# for other kinds of links, like file:, ftp:, etc.
        my $url ;
        if (  $Htmlfileurl ne '' ) {
            $link = "$Htmldir$link" if $link =~ m{^/}s;
            $url = relativize_url( $link, $Htmlfileurl );
# print( "  b: [$link,$Htmlfileurl,$url]\n" );
	}
	else {
            $url = $link ;
	}
	return $url;

    } else {
	return undef();
    }
}

#
# relativize_url - convert an absolute URL to one relative to a base URL.
# Assumes both end in a filename.
#
sub relativize_url {
    my ($dest,$source) = @_ ;

    my ($dest_volume,$dest_directory,$dest_file) =
        File::Spec::Unix->splitpath( $dest ) ;
    $dest = File::Spec::Unix->catpath( $dest_volume, $dest_directory, '' ) ;

    my ($source_volume,$source_directory,$source_file) =
        File::Spec::Unix->splitpath( $source ) ;
    $source = File::Spec::Unix->catpath( $source_volume, $source_directory, '' ) ;

    my $rel_path = '' ;
    if ( $dest ne '' ) {
       $rel_path = File::Spec::Unix->abs2rel( $dest, $source ) ;
    }

    if ( $rel_path ne ''                &&
         substr( $rel_path, -1 ) ne '/' &&
         substr( $dest_file, 0, 1 ) ne '#'
        ) {
        $rel_path .= "/$dest_file" ;
    }
    else {
        $rel_path .= "$dest_file" ;
    }

    return $rel_path ;
}


#
# coderef - make URL from the text of a C<>
#
sub coderef($$){
    my( $page, $item ) = @_;
    my( $url );

    my $fid = fragment_id( $item );

    if( defined( $page ) && $page ne "" ){
	# we have been given a $page...
	$page =~ s{::}{/}g;

        Carp::confess("Undefined fragment '$item' from fragment_id() in coderef() in $Podfile")
            if !defined $fid;
	# Do we take it? Item could be a section!
	my $base = $Items{$fid} || "";
	$base =~ s{[^/]*/}{};
	if( $base ne "$page.html" ){
            ###   print STDERR "coderef( $page, $item ): items{$fid} = $Items{$fid} = $base => discard page!\n";
	    $page = undef();
	}

    } else {
        # no page - local items precede cached items
	if( defined( $fid ) ){
	    if(  exists $Local_Items{$fid} ){
		$page = $Local_Items{$fid};
	    } else {
		$page = $Items{$fid};
	    }
	}
    }

    # if there was a pod file that we found earlier with an appropriate
    # =item directive, then create a link to that page.
    if( defined $page ){
	if( $page ){
            if( exists $Pages{$page} and $Pages{$page} =~ /([^:]*)\.[^:.]*:/){
		$page = $1 . '.html';
	    }
	    my $link = "$Htmlroot/$page#" . anchorify($fid);

	    # Here, we take advantage of the knowledge that $Htmlfileurl
	    # ne '' implies $Htmlroot eq ''.
	    if (  $Htmlfileurl ne '' ) {
		$link = "$Htmldir$link" ;
		$url = relativize_url( $link, $Htmlfileurl ) ;
	    } else {
		$url = $link ;
	    }
	} else {
	    $url = "#" . anchorify($fid);
	}

	confess "url has space: $url" if $url =~ /"[^"]*\s[^"]*"/;
    }
    return( $url, $fid );
}



#
# Adapted from Nick Ing-Simmons' PodToHtml package.
sub relative_url {
    my $source_file = shift ;
    my $destination_file = shift;

    my $source = URI::file->new_abs($source_file);
    my $uo = URI::file->new($destination_file,$source)->abs;
    return $uo->rel->as_string;
}


#
# finish_list - finish off any pending HTML lists.  this should be called
# after the entire pod file has been read and converted.
#
sub finish_list {
    my $fh = shift;
    if( $Listlevel ){
	warn "$0: $Podfile: unterminated list(s) at =head in paragraph $Paragraph.  ignoring.\n" unless $Quiet;
	while( $Listlevel ){
            process_back( $fh );
        }
    }
}

#
# htmlify - converts a pod section specification to a suitable section
# specification for HTML. Note that we keep spaces and special characters
# except ", ? (Netscape problem) and the hyphen (writer's problem...).
#
sub htmlify {
    my( $heading) = @_;
    $heading =~ s/(\s+)/ /g;
    $heading =~ s/\s+\Z//;
    $heading =~ s/\A\s+//;
    # The hyphen is a disgrace to the English language.
    # $heading =~ s/[-"?]//g;
    $heading =~ s/["?]//g;
    $heading = lc( $heading );
    return $heading;
}

#
# similar to htmlify, but turns non-alphanumerics into underscores
#
sub anchorify {
    my ($anchor) = @_;
    $anchor = htmlify($anchor);
    $anchor =~ s/\W/_/g;
    return $anchor;
}

#
# depod - convert text by eliminating all interior sequences
# Note: can be called with copy or modify semantics
#
my %E2c;
$E2c{lt}     = '<';
$E2c{gt}     = '>';
$E2c{sol}    = '/';
$E2c{verbar} = '|';
$E2c{amp}    = '&'; # in Tk's pods

sub depod1($;$$);

sub depod($){
    my $string;
    if( ref( $_[0] ) ){
	$string =  ${$_[0]};
        ${$_[0]} = depod1( \$string );
    } else {
	$string =  $_[0];
        depod1( \$string );
    }
}

sub depod1($;$$){
  my( $rstr, $func, $closing ) = @_;
  my $res = '';
  return $res unless defined $$rstr;
  if( ! defined( $func ) ){
      # skip to next begin of an interior sequence
      while( $$rstr =~ s/\A(.*?)([BCEFILSXZ])<(<+[^\S\n]+)?//s ){
         # recurse into its text
	  $res .= $1 . depod1( $rstr, $2, closing $3);
      }
      $res .= $$rstr;
  } elsif( $func eq 'E' ){
      # E<x> - convert to character
      $$rstr =~ s/^([^>]*)>//;
      $res .= $E2c{$1} || "";
  } elsif( $func eq 'X' ){
      # X<> - ignore
      $$rstr =~ s/^[^>]*>//;
  } elsif( $func eq 'Z' ){
      # Z<> - empty
      $$rstr =~ s/^>//;
  } else {
      # all others: either recurse into new function or
      # terminate at closing angle bracket
      my $term = pattern $closing;
      while( $$rstr =~ s/\A(.*?)(([BCEFILSXZ])<(<+[^\S\n]+)?|$term)//s ){
	  $res .= $1;
	  last unless $3;
          $res .= depod1( $rstr, $3, closing $4 );
      }
      ## If we're here and $2 ne '>': undelimited interior sequence.
      ## Ignored, as this is called without proper indication of where we are.
      ## Rely on process_text to produce diagnostics.
  }
  return $res;
}

{
    my %seen;   # static fragment record hash

sub fragment_id_readable {
    my $text     = shift;
    my $generate = shift;   # optional flag

    my $orig = $text;

    # leave the words for the fragment identifier,
    # change everything else to underbars.
    $text =~ s/[^A-Za-z0-9_]+/_/g; # do not use \W to avoid locale dependency.
    $text =~ s/_{2,}/_/g;
    $text =~ s/\A_//;
    $text =~ s/_\Z//;

    unless ($text)
    {
        # Nothing left after removing punctuation, so leave it as is
        # E.g. if option is named: "=item -#"

        $text = $orig;
    }

    if ($generate) {
        if ( exists $seen{$text} ) {
            # This already exists, make it unique
            $seen{$text}++;
            $text = $text . $seen{$text};
        } else {
            $seen{$text} = 1;  # first time seen this fragment
        }
    }

    $text;
}}

my @HC;
sub fragment_id_obfuscated {  # This was the old "_2d_2d__"
    my $text     = shift;
    my $generate = shift;   # optional flag

    # text? Normalize by obfuscating the fragment id to make it unique
    $text =~ s/\s+/_/sg;

    $text =~ s{(\W)}{
        defined( $HC[ord($1)] ) ? $HC[ord($1)]
        : ( $HC[ord($1)] = sprintf( "%%%02X", ord($1) ) ) }gxe;
    $text = substr( $text, 0, 50 );

    $text;
}

#
# fragment_id - construct a fragment identifier from:
#   a) =item text
#   b) contents of C<...>
#

sub fragment_id {
    my $text     = shift;
    my $generate = shift;   # optional flag

    $text =~ s/\s+\Z//s;
    if( $text ){
	# a method or function?
	return $1 if $text =~ /(\w+)\s*\(/;
	return $1 if $text =~ /->\s*(\w+)\s*\(?/;

	# a variable name?
	return $1 if $text =~ /^([\$\@%*]\S+)/;

	# some pattern matching operator?
	return $1 if $text =~ m|^(\w+/).*/\w*$|;

	# fancy stuff... like "do { }"
	return $1 if $text =~ m|^(\w+)\s*{.*}$|;

	# honour the perlfunc manpage: func [PAR[,[ ]PAR]...]
	# and some funnies with ... Module ...
	return $1 if $text =~ m{^([a-z\d_]+)(\s+[A-Z,/& ][A-Z\d,/& ]*)?$};
	return $1 if $text =~ m{^([a-z\d]+)\s+Module(\s+[A-Z\d,/& ]+)?$};

	return fragment_id_readable($text, $generate);
    } else {
	return;
    }
}

#
# make_URL_href - generate HTML href from URL
# Special treatment for CGI queries.
#
sub make_URL_href($){
    my( $url ) = @_;
    if( $url !~
        s{^(http:[-\w/#~:.+=&%@!]+)(\?.*)$}{<a href="$1$2">$1</a>}i ){
        $url = "<a href=\"$url\">$url</a>";
    }
    return $url;
}
}



package Mpp::html;

use strict;

use File::Copy;
use File::Path;

our $podroot;

BEGIN { unshift @INC, 'html' }	# For now bundle 1.11, the last working version

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
      makepp_compatibility
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


our $sitesearch = '/2.1';
sub frame($$$) {
  my( $file, $tab ) = @_;
  my( $head, $body ) = split "\f", $_[2];
  die unless $body;

  my $tabs = '';
  for my $meta ( @tabmeta ) {
    $tab eq $meta->[1] or
    $tabs .=
      "<a href='$meta->[2]'><img title='$meta->[0]' id='_$meta->[3]' src='${docroot}tabs.png' alt='$meta->[1]'/></a>";
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
<!DOCTYPE html>
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
      <input type='hidden' name='as_sitesearch' value='makepp.sourceforge.net$sitesearch'/>
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
  s((\$[{(]{1,2})([-\w]+)([})]{0,2})){
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
  open my( $tmpfile ), $_[0] or die;
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

	if( s/^  $// ) {	# special case for signature, keep exactly the empty lines
	} elsif( /^ $/ ) {
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
