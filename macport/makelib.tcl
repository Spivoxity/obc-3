set source [lindex $argv 0]
set target [lindex $argv 1]
set libs [lrange $argv 2 end]

set relpath "@executable_path/../Resources/lib"

file mkdir $target

proc find_deps {prog prefix} {
    set raw [exec otool -L $prog]
    set deps {}
    set re "$prefix/(\[^ \]*\\.dylib)"
    foreach {_ lib} [regexp -all -inline $re $raw] {
        if {[lsearch $deps $lib] < 0} {
            lappend deps $lib
        }
    }
    return $deps
}

proc union {s t} {
    set r $s
    foreach x $t {
        if {[lsearch $s $x] < 0} {
            lappend r $x
        }
    }
    return $r
}

proc diff {s t} {
    set r {}
    foreach x $s {
        if {[lsearch $t $x] < 0} {
            lappend r $x
        }
    }
    return $r
}

proc change_deps {file deps} {
    global relpath source
    
    set changes {}
    foreach d $deps {
        lappend changes -change $source/$d $relpath/$d
    }

    if {[file extension $file] == ".dylib"} {
        set raw [exec otool -D $file]
	regexp {/([^/]*)$} $raw _ name
	eval exec install_name_tool -id $relpath/$name $changes $file
    } elseif {[llength $deps] > 0} {
        eval exec install_name_tool $changes $file
    }
}

set agenda {}

foreach x $libs {
    if {[regexp {^-l(.*)} $x _ y]} {
        lappend agenda "lib$y.dylib"
    } else {
        puts "$x"
	set src [exec find $source -name $x]
	if {$src == ""} {
	    puts "$x not found"; exit 1
	}
        file copy $src $target/$x
        set deps [find_deps $target/$x $source]
        change_deps $target/$x $deps
        set agenda [union $agenda $deps]
    }
}

set known $agenda

proc schedule {events} {
    global pending known

    set pending [union $pending [diff $events $known]]
    set known [union $known $events]
}

while {[llength $agenda] > 0} {
    set pending {}

    foreach x $agenda {
        if {[file type $source/$x] == "link"} {
            set y [file readlink $source/$x]
            puts "Link $x --> $y"
            # TCL's file link assumes target exists
	    exec ln -s $y $target/$x
	    set deps $y
	} else {
	    puts $x
	    file copy $source/$x $target/$x
	    set deps [lrange [find_deps $target/$x $source] 1 end]
	    change_deps $target/$x $deps
	}
	
	set pending [union $pending [diff $deps $known]]
	set known [union $known $deps]
    }

    set agenda $pending
}
