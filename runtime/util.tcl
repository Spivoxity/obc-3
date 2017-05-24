#
# util.tcl
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

# max -- maximum of any number of args
proc max {x args} {
    set max $x
    foreach y $args {
	if {$y > $max} {set max $y}
    }
    return $max
}

# count -- number of occurrences of a char in a string
proc count {ch s} {return [regsub -all $ch $s "" dummy]}

# lmember -- test for list membership
proc lmember {x xs} {
    return [expr {[lsearch -exact $xs $x] >= 0}]
}

# ladd -- add values to a set
proc ladd {sv args} {
    upvar $sv s

    foreach x $args {
	if {! [lmember $x $s]} {lappend s $x}
    }
}

# lsplit -- split a list into components
proc lsplit {xs args} {
    set n [llength $args]

    if {[llength $xs] != $n} {
	error "expected $n fields, got [llength $xs]"
    }

    for {set i 0} {$i < $n} {incr i} {
	uplevel [list set [lindex $args $i] [lindex $xs $i]]
    }
}

# with -- bind names to record fields in body
proc with {record fields body} {
    uplevel [concat [list lsplit $record] $fields]
    uplevel $body
}

# filter -- filter a list by a predicate
proc filter {p xs} {
    set ys {}
    foreach x $xs {if {[uplevel $p $x]} {lappend ys $x}}
    return $ys
}

# exists -- test is a list has any member that satisfies a predicate
proc exists {p xs} {
    foreach x $xs {if {[uplevel $p $x]} {return 1}}
    return 0
}

# map -- apply a function to all elements of a list
proc map {f xs} {
    set ys {}
    foreach x $xs {lappend ys [uplevel [concat $f [list $x]]]}
    return $ys
}

# flatmap -- apply a function to all elements of a list, concatenate results
proc flatmap {f xs} {
    set ys {}
    foreach x $xs {set ys [concat $ys [uplevel $f $x]]}
    return $ys
}

# remdups -- remove adjecent duplicates from a list
proc remdups {xs} {
    if {$xs == ""} {
	return {}
    } else {
	set y [lindex $xs 0]
	set ys [list $y]
	foreach x $xs {
	    if {$x != $y} {lappend ys $x}
	    set y $x
	}
	return $ys
    }
}

# nth_char -- return n'th character of string
proc nth_char {n string} {
    return [string index $string $n]
}

# nth_char_is -- test if n'th character of string is a given char
proc nth_char_is {n ch string} {
    return [expr {[string index $string $n] == $ch}]
}

# trycatch -- handle exceptions by evaluating alternative script
proc trycatch {s1 s2} {
    if {[catch {uplevel $s1}]} {uplevel $s2}
}

set charcode() 0
for {set i 1} {$i < 128} {incr i} {
    set ch [format "%c" $i]
    set charcode($ch) $i
}

# trim -- trim leading and trailing spaces
proc trim {s} { regexp {^ *(.*[^ ]) *$} $s dummy s; return $s }

# csym -- make C symbol
proc csym {k s} {
    if {$k == ""} {
        return [regsub "\\." $s ""]
    } else {
        return ${k}_[regsub "\\." $s ""]
    }
}
