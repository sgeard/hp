# roots.tcl
# Auto-differentiation and root-finding in Tcl
# Demonstrates finding all three roots of 9^x = x^6
# i.e. f(x) = 9^x - x^6 = 0
#
# Modified Newton-Raphson method based on:
# "Root-finding: from Newton to Halley and beyond" by Richard J. Martin
# https://arxiv.org/html/2312.12305v1

# =============================================================================
# AVD namespace - Forward-mode auto-differentiation up to 2nd derivative
# Each value is represented as a dict {v f' f''}
# Seed variable x with d1=1, d2=0; constants with d1=0, d2=0
# =============================================================================

namespace eval avd {

    # Constructors
    proc seed  {v}       { dict create v $v d1 1.0 d2 0.0 }
    proc const {v}       { dict create v $v d1 0.0 d2 0.0 }
    proc make  {v d1 d2} { dict create v $v d1 $d1 d2 $d2 }

    # Accessors
    proc v  {x} { dict get $x v  }
    proc d1 {x} { dict get $x d1 }
    proc d2 {x} { dict get $x d2 }

    # Addition: (f+g)' = f'+g'  ;  (f+g)'' = f''+g''
    proc add {a b} {
        make [expr {[v $a]+[v $b]}] \
             [expr {[d1 $a]+[d1 $b]}] \
             [expr {[d2 $a]+[d2 $b]}]
    }

    # Subtraction: (f-g)' = f'-g'  ;  (f-g)'' = f''-g''
    proc sub {a b} {
        make [expr {[v $a]-[v $b]}] \
             [expr {[d1 $a]-[d1 $b]}] \
             [expr {[d2 $a]-[d2 $b]}]
    }

    # Negation
    proc neg {a} {
        make [expr {-[v $a]}] [expr {-[d1 $a]}] [expr {-[d2 $a]}]
    }

    # Multiplication - product rule + chain rule for d2
    # (fg)'  = f'g + fg'
    # (fg)'' = f''g + 2f'g' + fg''
    proc mul {a b} {
        make [expr {[v $a]*[v $b]}] \
             [expr {[v $a]*[d1 $b]+[d1 $a]*[v $b]}] \
             [expr {[v $a]*[d2 $b]+2.0*[d1 $a]*[d1 $b]+[d2 $a]*[v $b]}]
    }

    # Division - quotient rule + chain rule for d2
    # (f/g)'  = (f'g - fg') / g^2
    # (f/g)'' = (f''g - 2f'g' - fg'') / g^2 + 2fg'^2/g^3
    proc div {a b} {
        set bv [v $b]
        make [expr {[v $a]/[v $b]}] \
             [expr {([d1 $a]*[v $b]-[v $a]*[d1 $b])/$bv**2}] \
             [expr {([d2 $a]*[v $b]-2.0*[d1 $a]*[d1 $b]-[v $a]*[d2 $b])/$bv**2 \
                   + 2.0*[v $a]*[d1 $b]**2/$bv**3}]
    }

    # Power: x^n where n is a constant real
    # (x^n)'  = n*x^(n-1) * x'
    # (x^n)'' = n*(n-1)*x^(n-2)*x'^2 + n*x^(n-1)*x''
    proc pow {x n} {
        set f1 [expr {$n*[v $x]**($n-1)}]
        set f2 [expr {$n*($n-1)*[v $x]**($n-2)}]
        make [expr {[v $x]**$n}] \
             [expr {$f1*[d1 $x]}] \
             [expr {$f2*[d1 $x]**2 + $f1*[d2 $x]}]
    }

    # r^x where r is a constant base (e.g. 9^x)
    # (r^x)'  = ln(r)*r^x * x'
    # (r^x)'' = ln(r)^2*r^x*x'^2 + ln(r)*r^x*x''
    proc rpow {r x} {
        set rv  [expr {$r**[v $x]}]
        set lnr [expr {log($r)}]
        set f1  [expr {$lnr*$rv}]
        set f2  [expr {$lnr**2*$rv}]
        make $rv \
             [expr {$f1*[d1 $x]}] \
             [expr {$f2*[d1 $x]**2 + $f1*[d2 $x]}]
    }

    # exp(x)
    # (e^x)'  = e^x * x'
    # (e^x)'' = e^x*x'^2 + e^x*x''
    proc exp {x} {
        set ev [expr {exp([v $x])}]
        make $ev \
             [expr {$ev*[d1 $x]}] \
             [expr {$ev*[d1 $x]**2 + $ev*[d2 $x]}]
    }

    # log(x) - natural log
    # (ln x)'  = x'/x
    # (ln x)'' = -x'^2/x^2 + x''/x
    proc log {x} {
        set f1 [expr {1.0/[v $x]}]
        set f2 [expr {-1.0/[v $x]**2}]
        make [expr {log([v $x])}] \
             [expr {$f1*[d1 $x]}] \
             [expr {$f2*[d1 $x]**2 + $f1*[d2 $x]}]
    }

    # sin(x)
    # (sin x)'  =  cos(x)*x'
    # (sin x)'' = -sin(x)*x'^2 + cos(x)*x''
    proc sin {x} {
        make [expr {sin([v $x])}] \
             [expr {cos([v $x])*[d1 $x]}] \
             [expr {-sin([v $x])*[d1 $x]**2 + cos([v $x])*[d2 $x]}]
    }

    # cos(x)
    # (cos x)'  = -sin(x)*x'
    # (cos x)'' = -cos(x)*x'^2 - sin(x)*x''
    proc cos {x} {
        make [expr {cos([v $x])}] \
             [expr {-sin([v $x])*[d1 $x]}] \
             [expr {-cos([v $x])*[d1 $x]**2 - sin([v $x])*[d2 $x]}]
    }

    # sqrt(x) - implemented as x^0.5
    proc sqrt {x} { pow $x 0.5 }
}

# =============================================================================
# Numerical namespace - Newton-Raphson and Modified Newton-Raphson
# =============================================================================

namespace eval numerical {

    variable max_iterations 256
    variable debug 0

    # Standard Newton-Raphson
    # f is a proc taking an avd dict and returning an avd dict
    # Convergence criterion: |f(x)/f'(x)| < eps  (scale-independent step size)
    # Returns dict: n_iterations, solved, solution
    proc newton_raphson {f x0 eps {ilimit 0}} {
        variable max_iterations
        variable debug
        set max_iter [expr {$ilimit > 0 ? $ilimit : $max_iterations}]
        set x $x0
        set i 1
        while {$i <= $max_iter} {
            set r   [$f [avd::seed $x]]
            set fx  [avd::v  $r]
            set dfx [avd::d1 $r]
            if {$debug} {
                puts [format "   %3d   f(%0.10f) = %g ; f'(x) = %g ; step = %g" \
                      $i $x $fx $dfx [expr {$fx/$dfx}]]
            }
            if {abs($dfx) > 0 && abs($fx/$dfx) < $eps} break
            set x [expr {$x - $fx/$dfx}]
            incr i
        }
        return [dict create n_iterations $i solved [expr {$i < $max_iter}] solution $x]
    }

    # Modified Newton-Raphson
    # Uses second derivative to compute q-factor correction to the Newton step
    # Taken from "Root-finding: from Newton to Halley and beyond", Richard J. Martin
    # https://arxiv.org/html/2312.12305v1
    #
    # q  = f*f'' / f'^2
    # qf = 1 + (q/2)*(1+q/3)   if q >= 0
    #    = 1/(1 - q/2)          if q <  0
    # x_new = x - qf * f/f'
    #
    # Convergence criterion: |f(x)/f'(x)| < eps  (scale-independent step size)
    proc modified_newton_raphson {f x0 eps {ilimit 0}} {
        variable max_iterations
        variable debug
        set max_iter [expr {$ilimit > 0 ? $ilimit : $max_iterations}]
        set x $x0
        set i 1
        while {$i <= $max_iter} {
            set r    [$f [avd::seed $x]]
            set fx   [avd::v  $r]
            set dfx  [avd::d1 $r]
            set d2fx [avd::d2 $r]
            set q  [expr {$fx*$d2fx/$dfx**2}]
            if {$q >= 0} {
                set qf [expr {1.0 + ($q/2.0)*(1.0 + $q/3.0)}]
            } else {
                set qf [expr {1.0/(1.0 - $q/2.0)}]
            }
            if {$debug} {
                puts [format "   %3d   f(%0.10f) = %g ; f'(x) = %g ; f''(x) = %g ; q = %g ; qf = %g ; step = %g" \
                      $i $x $fx $dfx $d2fx $q $qf [expr {$qf*$fx/$dfx}]]
            }
            if {abs($dfx) > 0 && abs($fx/$dfx) < $eps} break
            set x [expr {$x - $qf*$fx/$dfx}]
            incr i
        }
        return [dict create n_iterations $i solved [expr {$i < $max_iter}] solution $x]
    }
}

# =============================================================================
# number_of_solns - Algebraic root count for a^x = x^(2n), a > 1
#
# The positive roots are intersections of a^x and x^(2n).
# Tangency condition: gradients equal => ln(a) = 2n/x => x_t = 2n/ln(a)
# Substituting back and simplifying: critical base is a = e^(2n/e)
# i.e. ln(a) = 2n/e
#
# Three cases for positive roots, determined by f(x_t) = a^x_t - x_t^(2n):
#   f(x_t) > 0  =>  0 positive roots  (exponential always above power curve)
#   f(x_t) = 0  =>  1 positive root   (tangent, double root at x_t)
#   f(x_t) < 0  =>  2 positive roots  (exponential dips below power curve)
#
# There is always exactly 1 negative root for a > 1, n >= 1.
#
# Returns a dict:
#   n_pos    - number of positive roots (0, 1 or 2)
#   n_total  - total number of roots (n_pos + 1)
#   xt       - tangent point x_t = 2n/ln(a)
#   fxt      - f(x_t)
#   tangent  - 1 if near-tangent case (roots very close), 0 otherwise
#   x_pos_lo - starting point for lower positive root (or {} if none)
#   x_pos_hi - starting point for upper positive root (or {} if none)
# =============================================================================

proc number_of_solns {a n f eps} {
    if {$a <= 1} {
        error "number_of_solns: a must be > 1"
    }
    set two_n [expr {2.0*$n}]
    set lna   [expr {log($a)}]

    # Tangent point x_t = 2n/ln(a)
    set xt    [expr {$two_n/$lna}]

    # f(x_t) = a^x_t - x_t^(2n)
    set fxt   [expr {$a**$xt - $xt**$two_n}]

    # Near-tangent threshold: use sqrt(eps) as the closeness criterion
    set near  [expr {abs($fxt) < sqrt($eps)}]

    if {$fxt > 0 && !$near} {
        # No positive roots
        set n_pos    0
        set x_pos_lo {}
        set x_pos_hi {}
    } elseif {$near} {
        # One positive root (double) at x_t
        set n_pos    1
        set x_pos_lo $xt
        set x_pos_hi {}
    } else {
        # Two positive roots either side of x_t
        # Use second derivative at x_t to estimate separation:
        # f(x) ~ f(x_t) + 0.5*f''(x_t)*(x-x_t)^2
        # => delta ~ sqrt(-2*f(x_t)/f''(x_t))
        set r    [$f [avd::seed $xt]]
        set d2ft [avd::d2 $r]
        if {$d2ft != 0} {
            set delta [expr {sqrt(-2.0*$fxt/$d2ft)}]
        } else {
            set delta [expr {$xt * 0.1}]
        }
        set n_pos    2
        set x_pos_lo [expr {$xt - $delta}]
        set x_pos_hi [expr {$xt + $delta}]
    }

    return [dict create \
        n_pos    $n_pos \
        n_total  [expr {$n_pos + 1}] \
        xt       $xt \
        fxt      $fxt \
        tangent  $near \
        x_pos_lo $x_pos_lo \
        x_pos_hi $x_pos_hi]
}

# =============================================================================
# solve_axn - Find all roots of a^x = x^(2n) for a > 1, integer n >= 1
# =============================================================================

proc solve_axn {a n eps} {
    # Base function: a^x - x^(2n) = 0
    set two_n [expr {2.0*$n}]
    proc f_base {x} [string map [list %a% $a %two_n% $two_n] {
        avd::sub [avd::rpow %a% $x] [avd::pow $x %two_n%]
    }]

    set info  [number_of_solns $a $n f_base $eps]
    set n_pos [dict get $info n_pos]
    set roots [list]

    # Always one negative root - start from x = -1
    set res [numerical::modified_newton_raphson f_base -1.0 $eps]
    if {[dict get $res solved]} {
        lappend roots [dict get $res solution]
    }

    if {$n_pos == 2} {
        # Two positive roots - use estimated starting points from tangent analysis
        set x_lo [dict get $info x_pos_lo]
        set x_hi [dict get $info x_pos_hi]

        set res [numerical::modified_newton_raphson f_base $x_lo $eps]
        if {[dict get $res solved]} {
            set r1 [dict get $res solution]
            lappend roots $r1

            # Deflate and find second positive root
            proc f_deflated {x} [string map [list %r1% $r1] {
                avd::div [f_base $x] [avd::sub $x [avd::const %r1%]]
            }]
            set res2 [numerical::modified_newton_raphson f_deflated $x_hi $eps]
            if {[dict get $res2 solved]} {
                lappend roots [dict get $res2 solution]
            }
        }
    } elseif {$n_pos == 1} {
        # Tangent case - one positive root at x_t
        set xt [dict get $info xt]
        set res [numerical::modified_newton_raphson f_base $xt $eps]
        if {[dict get $res solved]} {
            lappend roots [dict get $res solution]
        }
    }
    ;# n_pos == 0: no positive roots, nothing to add

    return [list roots [lsort -real $roots] info $info]
}

# =============================================================================
# Test: find all roots of 9^x = x^6  (a=9, n=3)
# =============================================================================

set a   9
set n   3
set eps 1.0e-10

puts "Finding roots of f(x) = ${a}^x - x^[expr {2*$n}] = 0"
puts "eps = $eps"
puts "================================================================"

# Temporary f proc for number_of_solns diagnostic output
proc f {x} {
    avd::sub [avd::rpow $::a $x] [avd::pow $x [expr {2.0*$::n}]]
}

set info [number_of_solns $a $n f $eps]
puts [format "Tangent point    x_t   = %0.6f" [dict get $info xt]]
puts [format "f(x_t)                 = %g"     [dict get $info fxt]]
puts "Number of positive roots : [dict get $info n_pos]"
if {[dict get $info tangent]} {
    puts "WARNING: near-tangent case - positive roots are very close together"
}
puts "Total roots expected     : [dict get $info n_total]"
puts ""

set result [solve_axn $a $n $eps]
set roots  [dict get $result roots]

puts [format "Found %d root(s):" [llength $roots]]
puts ""
foreach r $roots {
    set fv [avd::v [f [avd::seed $r]]]
    puts [format "  x = %0.10f   f(x) = %g   %d^x = %0.6f   x^%d = %0.6f" \
          $r $fv $a [expr {double($a)**$r}] [expr {2*$n}] [expr {$r**double(2*$n)}]]
}
