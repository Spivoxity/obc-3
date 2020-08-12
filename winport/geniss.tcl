fconfigure stdout -translation lf

set dest [lindex $argv 0]
set sources [lrange $argv 1 end]

if {$dest == "."} {
    set destdir "{app}"
} else {
    set destdir "{app}/$dest"
}

regsub -all {/} $destdir {\\} destdir

foreach f $sources {
    regsub -all {/} $f {\\} f
    puts "Source: \"$f\"; DestDir: \"$destdir\"; Flags: ignoreversion"    
}
