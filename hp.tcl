# Interface to the hp calculator exe primarily for Android
namespace eval hp {
    variable exe {./hp}

    variable prompt {> }

    # Child produced output: drain whatever is ready, then re-show the prompt.
    proc read {ch} {
        variable prompt
        while {[gets $ch line] >= 0} {
            puts $line
        }
        if {[eof $ch]} {
            close $ch
            incr ::forever
            return
        }
        # gets returned -1 with no eof: no complete line buffered yet.
        puts -nonewline $prompt; flush stdout
    }

    # User typed a line on stdin: forward it to the child.
    proc write {ch} {
        if {[gets stdin line] < 0} {
            # stdin closed (e.g. Ctrl-D): shut the child's input down.
            if {[eof stdin]} {
                close $ch write
            }
            return
        }
        puts $ch $line
    }

    proc run {} {
        variable exe
        variable prompt
        set fh [open "|$exe" r+]
        fconfigure $fh -blocking 0 -buffering line
        fconfigure stdin -blocking 0 -buffering line
        fileevent $fh readable [list hp::read $fh]
        fileevent stdin readable [list hp::write $fh]
        puts -nonewline $prompt; flush stdout
        vwait ::forever
    }
}

hp::run

