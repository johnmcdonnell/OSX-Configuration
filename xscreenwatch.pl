#!/usr/bin/perl

my $blanked = 0; 
open (IN, "xscreensaver-command -watch |"); 
while (<IN>) { 
    if (m/^(BLANK|LOCK)/) { 
        if (!$blanked) { 
            system "amixer set Master mute"; 
            $blanked = 1; 
        } 
    } 
    elsif (m/^UNBLANK/) { 
        # system "amixer set Master unmute"; 
        $blanked = 0; 
    } 
}

