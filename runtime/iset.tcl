#
# iset.tcl
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006--2016 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# This workaround is needed with TCL 8.4.2 if output goes to an emacs
# compilation buffer.
fconfigure stdout -translation lf
fconfigure stderr -translation lf

if {[llength $argv] != 4} {
    puts stderr "usage: iset input.iset header template interp"
    exit 1
}

set srcdir [file dirname $argv0]
source "$srcdir/util.tcl"
source "$srcdir/iparse.tcl"

if {[file exists "config.tcl"]} {source "config.tcl"}

lsplit $argv infile hfile tfile ifile

# BUILD THE TRIE

# make_trie -- recursively build a trie for a set of strings
proc make_trie {n strings} {
    global charcode first trie taken check ntrie

    # Assume the strings agree on the first n characters

    if {[llength $strings] == 0} {
	puts stderr "Empty trie!!!"
	return -9999
    }

    # Set chars to the set of n'th characters of the strings
    set chars [remdups [lsort [map {nth_char $n} $strings]]]
    set c1 $charcode([lindex $chars 0])

    # Find a place where a node for $chars will fit -- 0 for the root
    for {set q 0} {1} {incr q} {
	if {[info exists taken($q)]} continue

        while {$ntrie <= $q+128} {
            set trie($ntrie) 0
            set check($ntrie) 128
            incr ntrie
        }

	set ok 1
	foreach c $chars {
	    set ix [expr {$q+$charcode($c)}]
	    if {$check($ix) != 128} {
		set ok 0; break
	    }
	}

	if {$ok} break
    }

    # Reserve the locations we will use by filling in check
    # (actual values in trie get filled in later)
    set taken($q) 1

    foreach c $chars {
	set ix [expr {$q+$charcode($c)}]
	set check($ix) $charcode($c)
    }

    # Recursively build sub-tries
    foreach c $chars {
	if {$c == ""} {
	    set t [string range [lindex $strings 0] 0 [expr {$n-1}]]
	    set trie($q) $first($t)
	    set check($q) 0
	} else {
	    set ix [expr {$q+$charcode($c)}]
	    set subset [filter {nth_char_is $n $c} $strings]
	    set trie($ix) [make_trie [expr {$n+1}] $subset]
	    set check($ix) $charcode($c)
	}
    }

    return $q
}

proc has_templates {i} {
    global templates
    return [expr {[llength $templates($i)] > 0}]
}

# Build a packed trie for the instructions
proc build_trie {} {
    global ntrie instrs dirs
    set ntrie 0
    make_trie 0 [filter has_templates [concat $instrs $dirs]]
}

proc dmp_trie {f q pfx} {
    global trie check ntrie

    set sep ""
    if {$check($q) == 0} {
        puts $f [format "--> %d" $trie($q)]
        set sep $pfx
    }
    set nchild 0
    for {set c 1} {$c < 128} {incr c} {
        set i [expr {$q+$c}]
        if {$check($i) == $c} {
            set char($nchild) $c
            set son($nchild) $trie($i)
            incr nchild
        }
    }

    for {set j 0} {$j < $nchild} {incr j} {
        puts -nonewline $f [format "%s\[%c\] %3d " $sep $char($j) $son($j)]
        if {$j+1 < $nchild} {
            set slug " |      "
        } else {
            set slug "        "
        }
        dmp_trie $f $son($j) "$pfx$slug"
        set sep $pfx
    }
}
        

# GENERATE HEADER FILE

proc gen_header {f} {
    global ntempl maxargs instrs instrno dirs dirno ops action ntrie \
        expand opcode ncodes

    puts $f "#define NTEMPLATES $ntempl"
    puts $f "#define NTRIE $ntrie"
    puts $f "#define MAXARGS $maxargs"
    puts $f ""

    puts $f "#define __INSTRS__(i) \\"
    puts -nonewline $f "     i(ILLEGAL)"
    foreach i $instrs {
	set m [list [csym "" $i]]
	if {[info exists expand($i)]} {
	    foreach x $expand($i) {
		if {[regexp {^(.*) \$a$} $x _ y]} {
		    lappend m "[csym I $y]|IARG"
		} elseif {[regexp {^(.*) (-?[0-9]*)$} $x _ y z]} {
		    lappend m "[csym I $y]|ICON" $z
		} else {
		    lappend m "[csym I $x]"
		}
	    }
	}
        puts -nonewline $f \
            " \\\n     i([join $m ", "])"
    }
    puts $f ""
    puts $f ""
    puts $f "#define __i1__(sym, ...) I_##sym,"
    puts $f "enum { __INSTRS__(__i1__) };"
    puts $f ""

    puts $f "#define __DIRS__(d) \\"
    puts -nonewline $f "     d(ILLEGAL)"
    foreach d $dirs {
        puts -nonewline $f " \\\n     d([csym "" $d])"
    }
    puts $f ""
    puts $f ""
    puts $f "#define __d1__(sym) D_##sym,"
    puts $f "enum { __DIRS__(__d1__) };"
    puts $f ""

    # Not an enum, because bases are not contiguous!
    puts $f "#define K_ILLEGAL 0"
    foreach op $ops {
	with $action($op) {base count length inst key act args} {
	    puts $f "#define [csym K $op] $base"
	}
    }
    puts $f ""

    puts -nonewline $f "#define __OPCODES__(o)"
    for {set i 0} {$i < 256} {incr i} {
        if {$i < $ncodes} {
            with $opcode($i) {op inst patt arg len} {
                puts -nonewline $f \
                    " \\\n     o($op, $inst, \"$patt\", $arg, $len)"
            }
	} else {
            puts -nonewline $f " \\\n     o(ILLEGAL, ILLEGAL, \"\", 0, 1)"
        }
    }

    puts $f ""
}

# GENERATE TEMPLATE FILE

# make_code -- assemble equivalent code
proc make_code {op} {
    global ops dirs status

    if {$op == "NOP"} {
	return {}
    } elseif {[lmember $op $ops]} {
	return [csym K $op]
    } elseif {[lmember $op $dirs]} {
	return [csym D $op]
    } else {
	puts stderr "Code $op does not exist"
	set status 1
    }
}

proc quote {s} {return "\"$s\""}

proc gen_template {f} {
    global templates instrs dirs first ntrie trie check macro

    puts $f "#include \"oblink.h\""
    puts $f "#include \"keiko.h\""
    puts $f ""

    set nt 0
    set fmt "{%-12s %-7s%3d, %2d, %2d, %2d, %2d, %s, {%s}},"
    puts $f "struct _template templates\[NTEMPLATES\] = {"
    foreach inst [concat $instrs $dirs] {
	set first($inst) $nt
	foreach templ $templates($inst) {
	    with $templ {patt bounds op argsz} {
		with $bounds {lo hi step} {
		    if {$nt == $first($inst)} {
			set icode "\"$inst\""
		    } else {
			set icode "   NULL"
		    }
		    if {[info exists macro($op)]} {
			set maclines [map quote $macro($op)]
			puts $f \
			    [format $fmt "$icode," "\"$patt\"," \
				 $lo $hi $step 0 0 0 [join $maclines ", "]]
		    } else {
			if {$op == "NOP"} {
			    set n 0; set c 0
			} else {
			    set n 1; set c [make_code $op]
			}
			set len [expr {$argsz >= 0 ? $n + $argsz : $argsz}]
			puts $f \
			    [format $fmt "$icode," "\"$patt\"," \
				 $lo $hi $step  $len $n $c ""]
		    }
		}
	    }
	    incr nt
	}
    }
    puts $f "};";
    puts $f "";

    build_trie

    puts $f "/*"
    dmp_trie $f 0 ""
    puts $f "*/"
    puts $f ""

    puts $f "short templ_trie\[NTRIE\] = {"
    for {set i 0} {$i < $ntrie} {incr i} {
	if {$i > 0 && $i % 10 == 0} {puts $f ""}
	puts -nonewline $f [format "%4d, " $trie($i)]
    }
    puts $f "\n};"    
    puts $f "";
    puts $f "uchar templ_check\[NTRIE\] = {"
    for {set i 0} {$i < $ntrie} {incr i} {
	if {$i > 0 && $i % 10 == 0} {puts $f ""}
        if {$check($i) >= 32 && $check($i) < 128} {
            puts -nonewline $f [format " '%c', " $check($i)]
        } else {
            puts -nonewline $f [format "%4d, " $check($i)]
        }
    }
    puts $f "\n};"    
}

# GENERATE INTERPRETER

proc make_body {key action argv} {
    global err_op

    set body $action

    for {set i 0} {$i < [llength $argv]} {incr i} {
	set formal [string index "abcd" $i]
	regsub -all "\\\$$formal" $body [lindex $argv $i] body
    }

    regsub -all {\$s} $body "sp" body
    regexp {\.(.)} $key _ suffix

    switch -glob -- $key {
	B.d {
	    # Double from two doubles
	    regsub -all {\$1\.d} $body {getdbl(\&sp[2])} body
	    regsub -all {\$2\.d} $body {getdbl(\&sp[0])} body
	    return "putdbl(&sp\[2\], $body); sp += 2;"
	}
	B.?dd {
	    # Value from two doubles
	    regsub -all {\$1\.d} $body {getdbl(\&sp[2])} body
	    regsub -all {\$2\.d} $body {getdbl(\&sp[0])} body
	    return "sp\[3\].$suffix = $body; sp += 3;"
	}	    
	B.d?? {
	    # Double from two values
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {sp[0]} body
    	    return "putdbl(&sp\[0\], $body);"
	}
	B.q {
	    # Long from two longs
	    regsub -all {\$1\.q} $body {getlong(\&sp[2])} body
	    regsub -all {\$2\.q} $body {getlong(\&sp[0])} body
	    return "putlong(&sp\[2\], $body); sp += 2;"
	}
	B.?qq {
	    # Value from two longs
	    regsub -all {\$1\.q} $body {getlong(\&sp[2])} body
	    regsub -all {\$2\.q} $body {getlong(\&sp[0])} body
	    return "sp\[3\].$suffix = $body; sp += 3;"
	}	    
	B.q?? {
	    # Long from two values
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {sp[0]} body
    	    return "putlong(&sp\[0\], $body);"
	}
        B.x {
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {sp[0]} body
 	    return "sp\[1\].a = address($body); sp++;"
        }
	B.? {
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {sp[0]} body
 	    return "sp\[1\].$suffix = $body; sp++;"
	}
	M.dq {
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "putdbl(&sp\[0\], $body);"
	}
	M.qd {
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "putlong(&sp\[0\], $body);"
	}
	M.d {
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "putdbl(&sp\[0\], $body);"
	}
	M.d? {
	    # Double from value
	    regsub -all {\$1} $body {sp[1]} body
	    return "sp--; putdbl(&sp\[0\], $body);"
	}
	M.?d {
	    # Value from double
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "sp\[1\].$suffix = $body; sp++;"
	}	    
	M.q {
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "putlong(&sp\[0\], $body);"
	}
	M.q? {
	    # Long from value
	    regsub -all {\$1} $body {sp[1]} body
	    return "sp--; putlong(&sp\[0\], $body);"
	}
	M.?q {
	    # Value from long
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "sp\[1\].$suffix = $body; sp++;"
	}	    
        M.x {
	    regsub -all {\$1} $body {sp[0]} body
	    return "sp\[0\].a = address($body);"
        }            
	M.? {
	    regsub -all {\$1} $body {sp[0]} body
	    return "sp\[0\].$suffix = $body;"
	}
	V.d {
	    return "sp -= 2; putdbl(&sp\[0\], $body);"
	}
	V.q {
	    return "sp -= 2; putlong(&sp\[0\], $body);"
	}
        V.x {
            return "sp--; sp\[0\].a = address($body);"
        }
	V.? {
	    return "sp--; sp\[0\].$suffix = $body;"
	}	    
	S0 {
	    return "{ $body }"
	}
	S[123] {
	    regexp {S(.)} $key _ x
	    for {set i 1} {$i <= $x} {incr i} {
                set j [expr {$x-$i}]
		regsub -all "\\\$$i" $body "sp\[$j\]" body
	    }
	    return "{ $body } sp += $x;"
	}
	S1d {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "{ $body } sp += 2;"
	}
	S2d? {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[1])} body
	    regsub -all {\$2} $body {sp[0]} body
	    return "{ $body } sp += 3;"
	}
	S3d?? {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[2])} body
	    regsub -all {\$2} $body {sp[1]} body
	    regsub -all {\$3} $body {sp[0]} body
	    return "{ $body } sp += 4;"
	}
	S1q {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "{ $body } sp += 2;"
	}
	S2q? {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[1])} body
	    regsub -all {\$2} $body {sp[0]} body
	    return "{ $body } sp += 3;"
	}
	S3q?? {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[2])} body
	    regsub -all {\$2} $body {sp[1]} body
	    regsub -all {\$3} $body {sp[0]} body
	    return "{ $body } sp += 4;"
	}
	T1 {
 	    regsub -all {\$1} $body {sp[0]} body
	    return "{ $body }"
 	}
        T1d {
            regsub -all {\$1\.d} $body {get_double(\&sp[0])} body
            return "{ $body }"
        }
        T1q {
            regsub -all {\$1\.q} $body {get_long(\&sp[0])} body
            return "{ $body }"
        }
	T2 {
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {sp[0]} body
	    return "{ $body } sp++;"
	}
        T2q {
            regsub -all {\$1\.q} $body {get_long(\&sp[0])} body
            regsub -all {\$2\.q} $body {get_long(\&sp[2])} body
            return "{ $body } sp += 2;"
        }
	default {
	    error "Bad key $key for $err_op"
	}
    }
}

proc gen_actions {f} {
    global ncodes opcode defs copy ops action input err_op
    
    # macros used in action code
    puts $f $defs

    # action code
    foreach op $ops {
	set err_op $op
	with $action($op) {base count length inst key act argv} {
	    set act [make_body $key $act $argv]
	    puts $f "          ACTION($op)"
	    for {set j 1} {$j < $count} {incr j} {
		puts $f "          ALSO($op+$j)"
	    }
	    puts $f "               pc = pc0 + $length;"
	    puts $f "               $act"
	    puts $f "               NEXT;"
	    puts $f ""
	}
    }
}


# MAIN PROGRAM

readfile $infile

if {$status != 0} {exit $status}

proc gen_file {msg fname gen} {
    set f [open $fname "w"]

    puts $f "/* $msg -- generated by iset.tcl */"
    puts $f ""
    $gen $f
    close $f
}

gen_file "Template file" $tfile gen_template
gen_file "Action code" $ifile gen_actions
gen_file "Header file" $hfile gen_header

# Print statistics
puts "Instr     Count  Opcodes"
set fmt "%-10s %3d    %3d"
set count(0) 0;
set count(1) 1; # Allow for ILLEGAL
set count(2) 0
foreach inst $instrs {
    if {$opcount($inst) <= 2} {
	incr count($opcount($inst))
    } else {
	puts [format $fmt $inst 1 $opcount($inst)]
    }
}
puts [format $fmt "singles" $count(1) $count(1)]
puts [format $fmt "doubles" $count(2) [expr {2 * $count(2)}]]
puts [format $fmt "Total" $ninstr $ncodes]

if {$ncodes > 256} {set status 1}

exit $status
