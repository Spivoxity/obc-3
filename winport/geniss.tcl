fconfigure stdout -translation lf

set gtk 0

if {[lindex $argv 0] == "-gtk"} {
    set gtk 1
    set gtkdir [lindex $argv 1]
    set argv [lrange $argv 2 end]
}

foreach f $argv {
    set d ""; set p ""; set q "{app}"
    regexp {(.*)/(.*)} $f _ d f
    if {$gtk} {
	if {$d == ""} {
	    set p "$gtkdir/bin/"
	} else {
	    set p "$gtkdir/$d/"
	    set q "$q/$d"
	}
    } elseif {$d != ""} {
	set p "$p$d/"
    }
    regsub -all {/} $p {\\} p
    regsub -all {/} $q {\\} q
    puts "Source: \"$p$f\"; DestDir: \"$q\"; Flags: ignoreversion"
}
