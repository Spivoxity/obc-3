#
# iparse.tcl
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

# PARSER FOR INSTRUCTION SET DEFINITIONS

# The input is a sequence of calls to TCL routines that are
# defined here.  Instructions for the abstract machine are created by
# calling the routine 'inst' or one of its variants.  Each rule has:
#
#  * A mnemonic, used in the assembly code output by the compiler.
#  * One or more patterns that match arguments of the instruction.
#  * A key that specifies the number and types is the operands and
#    the result of the instruction.
#  * An action, written as a fragment of C code.
#
# Patterns:
# 	1/2		Integer (1/2 bytes)
#       [lo,hi,step]	Integer encoded in opcode
#	R/S		Branch displacement (1/2 bytes)
# 
# A rule may contain a list of patterns, and produces one template for
# each pattern.  A template corresponds to one opcode or (with the
# form [lo,hi,step]) several opcodes. It's allowed to have more than
# one rule for the same instruction, with disjoint patterns.
#
# A pattern can describe multiple arguments (so the pattern "1R"
# describes two operands, a 1-byte integer and a 1-byte displacement),
# but this may not be combined with range patterns [lo,hi,step], which
# must always appear alone.

# INPUT PROCESSING

# syntax -- syntax error message
proc syntax {msg {n -1}} {
    error $msg

    global input lcount status
    if {$n < 0} {set n $lcount}
    puts stderr "$input:$n: $msg"
    set status 1
}

set ncodes 0;			# No. of opcodes used
set ntempl 0;			# No. of templates
set ndir 0;			# No. of directives
set ninstr 0;			# No. of instructions
set maxargs 0;			# Max args of any template
set status 0;			# Exit status
set instrs {}; set dirs {}; set ops {}
set defs {}

proc make_inst {inst} {
    global ninstr instrs instrno templates opcount

    if {[info exists instrno($inst)]} return
    set n [incr ninstr]
    lappend instrs $inst
    set instrno($inst) $n
    set templates($inst) {}
    set opcount($inst) 0
}

proc make_dir {dir} {
    global dirs dirno ndir
    set n [incr ndir]
    lappend dirs $dir
    set dirno($dir) $n
    set templates($dir) {}
}

proc make_template {inst patt bounds op argbytes} {
    global templates ntempl maxargs

    if {$argbytes > 0} {set maxargs [max $maxargs [string length $patt]]}
    lappend templates($inst) [list $patt $bounds $op $argbytes] 
    incr ntempl
}

proc make_macro {op codes} {
    global macro
    set macro($op) $codes
}

proc make_action {op base count inst key act argv length} {
    global ops action

    lappend ops $op
    set action($op) [list $base $count $length $inst $key $act $argv]
}

proc make_opcode {op inst patt arg len} {
    global ncodes 
    global opcode
    global opcount

    incr opcount($inst)
    set opcode($ncodes) [list $op $inst $patt $arg $len]
    incr ncodes
}

# range_arg -- form expression for argument of action code
proc range_arg {bounds base} {
    # The expression is "ir * step + off", where lo = base * step + off.
    with $bounds {lo hi step} {
	set off [expr {$lo - $base * $step}]
	set exp "ir"
	if {$step != 1} {set exp "$exp*$step"}
	if {$off > 0} {
	    set exp "$exp+$off"
	} elseif {$off < 0} {
	    set exp "$exp-[expr {-$off}]"
	}
    }
    return $exp
}
    
proc map_args {patt bounds base vargs} {
    upvar $vargs args

    set off 1; set args {}
    foreach p [split $patt {}] {
	switch -glob $p {
	    N       {lappend args [range_arg $bounds $base]}
	    [1SK]   {lappend args "get1(pc0+$off)"; incr off 1}
	    [2RTL]  {lappend args "get2(pc0+$off)"; incr off 2}
	    default {syntax "Bad pattern code $p"}
	}
    }
    return [expr {$off - 1}]
}

# process -- process an instruction definition
proc process {inst patts key action} {
    global ncodes status

    if {$patts == "0"} {set patts [list ""]}
    make_inst $inst

    set j 0
    foreach patt $patts {
	if {[regexp {^\[(.*),(.*),(.*)\]} $patt _ lo hi step]} {
	    regsub {^\[.*\]} $patt "N" patt
	    if {[llength $patts] == 1} {
		set op $inst
	    } else {
		set op "${inst}_x[incr j]"
	    }
	} else {
	    set lo 0; set hi 0; set step 0
	    if {$patt == ""} {
		set op $inst
	    } else {
		set op "${inst}_$patt"
	    }
	}	

	if {![regexp {^[12RSTNKL]*$} $patt]} {
	    syntax "Bad pattern $patt"
	}

	# Compute offsets for the arguments
	set base $ncodes; set n 0
	set bounds [list $lo $hi $step]
	set arglen [map_args $patt $bounds $base args]
	set totlen [expr {$arglen+1}]

	for {set arg $lo} {$arg <= $hi} {incr arg [expr {$step>0?$step:1}]} {
	    make_opcode $op $inst $patt $arg $totlen
	    incr n
	}

	make_template $inst $patt $bounds $op $arglen
	make_action $op $base $n $inst $key $action $args $totlen
    }
}

proc defs {text} {
    global defs
    append defs "$text\n"
}

# Create an instruction
proc inst {inst patts key act} {
    process $inst $patts $key $act
}

# Create a dummy instruction (used for CASE labels)
proc zinst {inst template} {
    make_inst $inst
    if {$template != "0"} {
	set arglen [map_args $template none 0 args]
	make_template $inst $template {0 0 0} NOP $arglen
    }
}

# Create an assembler directive
proc dir {inst patt} {
    # A directive
    if {$patt == "0"} {set patt ""}
    make_dir $inst
    make_template $inst $patt {0 0 0} $inst -1
}

# Make an instruction equivalent to a sequence of others
proc equiv {inst patt equiv} {
    global ncodes status

    if {$patt == "0"} {
	set patt ""; set op $inst
    } else {
	set op "${inst}_$patt"
    }

    set arglen [map_args $patt none 0 args]
    set codes [map trim [split $equiv ","]]

    make_inst $inst
    make_template $inst $patt {0 0 0} $op $arglen
    make_macro $op $codes
}

# Provide expansion for use by JIT translator
proc expand {inst patt equiv} {
    global expand
    set expand($inst) [map trim [split $equiv ","]]
}

proc readfile {name} {
    global input opcount

    set input $name

    set opcount(ILLEGAL) 0
    make_opcode ILLEGAL ILLEGAL "" 0 1

    uplevel #0 source $name
}

