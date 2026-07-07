# Tk front-end for the hp calculator exe, for desktop wish and AndroWish on
# Android - drive the calculator without a terminal.
#
#   wish hp_tk.tcl            (desktop)
#
# On Android (AndroWish) the calculator binary is shipped inside the APK as
# lib/<abi>/libhp.so, which the installer extracts into the app's
# nativeLibraryDir - the only location an app is permitted to exec from. That
# directory is on LD_LIBRARY_PATH, so find_exe locates libhp.so there. On the
# desktop nothing matches and it falls back to ./hp.
#
# hp is launched as 'hp -v -np': verbose (dump the whole stack after every
# command) with no prompt. Each dump is a run of newline-terminated register
# lines 't:'/'s:'/'z:'/'y:'/'x:', top-to-bottom, always ending with 'x:'. The
# GUI is therefore purely line-based: it accumulates register lines and, on the
# 'x:' line, renders the stack pane and logs the result. The 'ss' command asks
# hp to re-dump the stack and drives the Refresh button.
#
# Layout (portrait): the live Stack (top), a scrolling History transcript
# (middle, expands), and a single-line entry with a Send button (bottom).

package require Tk

namespace eval hp {
    variable exe    {./hp}      ;# path to the calculator (override for Android)
    variable chan   {}          ;# pipe channel to the child
    variable regbuf {}          ;# stack register lines seen since the last 'x:'
    variable w                  ;# array of widget paths
    variable stack_size 5       ;# The stack size of the compiler, fixed at compile time

    # Locate the calculator executable. On AndroWish it is shipped as libhp.so
    # in the app's nativeLibraryDir, which Android places on LD_LIBRARY_PATH;
    # on the desktop nothing matches and we keep ./hp.
    proc find_exe {} {
        variable exe
        if {[info exists ::env(LD_LIBRARY_PATH)]} {
            foreach dir [split $::env(LD_LIBRARY_PATH) :] {
                set cand [file join $dir libhp.so]
                if {[file exists $cand]} { return $cand }
            }
        }
        return $exe
    }

    # --- output rendering -------------------------------------------------

    proc to_history {text} {
        variable w
        $w(hist) configure -state normal
        if {[string length $text] > 3 && ([string range "$text" end-2 end ] eq {???})} {
            $w(hist) insert end "$text\n" unknown
        } else {
            $w(hist) insert end "$text\n"
        }
        $w(hist) see end
        $w(hist) configure -state disabled
    }

    proc set_stack {regs} {
        variable w
        variable stack_size
        clear_stack
        $w(stack) configure -state normal
        # The stack should always display from the bottom up
        set nlines [expr {$stack_size - [llength $regs]}]
        $w(stack) insert end [string repeat "\n" $nlines]
        $w(stack) insert end [join $regs "\n"]
        $w(stack) configure -state disabled
    }

    proc clear_stack {} {
        variable w
        $w(stack) configure -state normal
        $w(stack) delete 1.0 end
        $w(stack) configure -state disabled
    }

    # --- child I/O --------------------------------------------------------

    proc on_readable {} {
        variable chan
        variable regbuf
        while {[gets $chan line] >= 0} {
            set line [string trimright $line "\r"]
            if {[regexp {^[a-z]: } $line]} {
                lappend regbuf $line
                if {[string match {x:*} $line]} {
                    set_stack $regbuf
                    to_history "  → [string trim [string range $line 2 end]]"
                    set regbuf {}
                }
            } elseif {$line ne {}} {
                to_history $line    ;# errors / anything not stack-shaped
                set regbuf {}
            }
        }
        if {[eof $chan]} {
            on_eof
        }
    }

    proc on_eof {} {
        variable chan
        catch {close $chan}
        set chan {}
        # hp has exited - the user quit it (q / quit / =) or it died. There is
        # nothing left to drive, so close the GUI too rather than leave a dead
        # window: on a phone it is fullscreen with no decorations, so a stranded
        # window can't be dismissed. (The window's close button routes through
        # hp::quit, which sends q and reaches here the same way.)
        exit
    }

    # --- input ------------------------------------------------------------

    proc send {cmd} {
        variable chan
        if {$chan eq {}} return
        puts $chan $cmd
        flush $chan
    }

    proc submit {} {
        variable w
        set cmd [string trim [$w(entry) get]]
        $w(entry) delete 0 end
        if {$cmd eq {}} return
        to_history ":: $cmd"
        clear_stack ; # Needed here for the clear commands
        send $cmd
    }

    proc quit {} {
        send q
        after 200 exit
    }

    # --- startup ----------------------------------------------------------

    proc build_ui {} {
        variable w
        wm title . {hp calculator}

        # Stack pane (bottom, above the entry): at most five register lines,
        # no scrolling.
        set sf [ttk::labelframe .stackf -text Stack -padding 2]
        set s_tw [text $sf.t -width 24 -height 5 -font TkFixedFont -wrap none \
            -state disabled]
        grid $s_tw -sticky nsew
        grid columnconfigure $sf 0 -weight 1
        grid rowconfigure $sf 0 -weight 1

        # History pane (middle): scrolling transcript.
        set hf [ttk::labelframe .histf -text History -padding 2]
        set h_tw [text $hf.t -width 24 -height 12 -font TkFixedFont -wrap none \
            -state disabled]
        set h_sb [ttk::scrollbar $hf.sb -orient vertical -command "$h_tw yview"]
        $h_tw configure -yscrollcommand "$h_sb set"
        $h_tw tag configure unknown -foreground red
        grid $h_tw  -row 0 -column 0 -sticky nsew
        grid $h_sb -row 0 -column 1 -sticky ns
        grid columnconfigure $hf 0 -weight 1
        grid rowconfigure $hf 0 -weight 1

        # Classic entry, not ttk::entry: the SDLtk ttk theme on AndroWish ignores
        # -fieldbackground, so the pale-green field never appeared there. A plain
        # entry's -background is honoured on both the desktop and AndroWish.
        set cf   [ttk::frame .cmd]
        set cf_e [entry $cf.entry -background {#ccffcc} -relief sunken \
            -borderwidth 1]
        set cf_sb [ttk::button $cf.send -text Send -command hp::submit]
        #pack $cf_sb -side right -padx 2 -pady 2
        pack $cf_e -side left -fill x -expand 1

        # Portrait: History (scrollback) over Stack over command entry. The
        # Stack and the entry are essential and must never be pushed off a
        # shrinking window; the History transcript is the pane that may give
        # way. pack expresses that priority: slaves packed first are given their
        # requested space, and only the last -expand slave (History) absorbs the
        # slack - so when the window is short the transcript shrinks (and clips)
        # while the Stack and input line stay put. grid, by contrast, lays out
        # from the top and lets the bottom rows fall off the edge, which is why
        # the Stack and entry vanished when the pop-up was dragged smaller.
        # Inset the content from the toplevel edges. Under overrideredirect the
        # panes otherwise sit flush against the One UI pop-up window, so the
        # labelframe left/right borders are clipped and the entry's bottom
        # corners disappear into the window's rounded corners. A margin all round
        # - with extra at the bottom to clear the corner radius - keeps every
        # edge visible. (padx/pady here are the knobs to bump if a device rounds
        # its corners more aggressively.)
        pack $cf -side bottom -fill x              -padx 8 -pady {2 16}
        pack $sf -side bottom -fill x              -padx 8 -pady 2
        pack $hf -side top -fill both -expand 1    -padx 8 -pady {4 2}

        bind $cf_e <Return> hp::submit
        wm protocol . WM_DELETE_WINDOW hp::quit
        array set w [list hist $h_tw stack $s_tw entry $cf_e send $cf_sb]

        # On AndroWish the SDL surface fills the whole Android window (the device
        # screen in fullscreen, or the freeform/pop-up window in multi-window
        # mode), but '.' shrink-wraps to its requested size and sits top-left,
        # leaving the rest of the surface black. Drop the SDLtk window decorations
        # (otherwise their borders/title bar are clipped against the surface,
        # taking the close button and bottom row off the edges) and pin '.' to the
        # real drawable so the gridded content (row/column weight 1) fills it.
        # The borg command exists only under AndroWish, so the desktop build is
        # left untouched.
        if {[llength [info commands borg]]} {
            wm overrideredirect . 1
            bind . <Configure> {if {{%W} eq {.}} {hp::fit_root}}
            # Defeat the freeform/surface settling race: for a second or two after
            # the window first appears Android can resize the SDL surface (or
            # re-lay-out the window) back towards the display size, sometimes
            # WITHOUT a <Configure> on '.' to re-trigger us, leaving it stuck too
            # big so Stack/entry fall outside the pop-up. sdltk size is stable
            # throughout, so just re-apply a few times over the first ~5s; the
            # <Configure> binding handles any later user resize/rotation.
            foreach ms {0 200 500 1000 2000 3500 5000} { after $ms hp::fit_root }
        }
    }

    # Size '.' to the SDL surface. 'sdltk size' reports the actual drawable - the
    # screen in fullscreen and the pop-up window in multi-window mode - whereas
    # winfo screenwidth/height always report the whole display. Idempotent: the
    # guard makes a re-apply at the correct size a no-op (also avoids a
    # <Configure> feedback loop).
    proc fit_root {} {
        if {[catch {lassign [sdltk size] sw sh}]} return
        if {$sw > 0 && $sh > 0 &&
            ([winfo width .] != $sw || [winfo height .] != $sh)} {
            wm geometry . ${sw}x${sh}
        }
    }

    proc run {} {
        variable chan
        variable w
        build_ui
        set exe [find_exe]
        if {[catch {open "|[list $exe] -v -np" r+} chan]} {
            to_history "-- failed to start $exe --"
            to_history $chan
            set chan {}
            $w(entry) configure -state disabled
            $w(send)  configure -state disabled
            return
        }
        fconfigure $chan -blocking 0 -buffering line
        fileevent $chan readable hp::on_readable

        # Put the cursor in the entry at startup. Force it (there is no window
        # manager to assign focus under overrideredirect) and re-assert once the
        # pop-up has finished mapping/rescaling, since an immediate focus can be
        # dropped as Android settles the surface. (The soft keyboard is left to
        # appear on the first tap - focus alone does not raise it on AndroWish,
        # and auto-raising it gained nothing since a tap is needed to type.)
        focus -force $w(entry)
        after 400 [list catch [list focus -force $w(entry)]]
    }
}

hp::run
