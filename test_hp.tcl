# Test harness for the hp calculator

set n_passed 0
set n_failed 0

# Run and check the result
proc check {opts com_string ref} {
    global n_failed n_passed
    if {$opts eq {}} {
        set harg [list "$com_string ="]
    } else {
        set harg [list $opts "$com_string ="]
    }
    set result [exec -ignorestderr -- ./hp  {*}$harg]
    # Only report the first line if the test passes
    set brief_ref [lindex [split "$ref" \n] 0]
    if {$result eq $ref } {
        puts "PASS: '$com_string' => '$brief_ref'"
        incr n_passed
    } else {
        puts "FAIL: '$com_string' => '$result', expected '$ref'"
        incr n_failed
    }
}

#------------------------------------------------------------------------------

# Basic arithmetic
check ""   "2 3 +"          5.000000
check ""   "3 ^2"           9.000000
check ""   "2 3 ^x"         8.000000
check ""   "9 ^/2"          3.000000

# Trig
check ""   "90 sin"              "1.000000"
check ""  "radians pi 2 / sin"   "1.000000"
check ""  "radians pi cos"       "-1.000000"
check ""  "45 tan"               "1.000000"
check ""  "1 asin"               "90.000000"

# Complex - mixed polar/cartesian arithmetic
check "-c" "(1,90)p (1,0) +"  "(1.414214,45.000000) p"
check "-c" "(2,0) (0,1) *"    "(0.000000,2.000000)"
check "-c" "5 ^/2 1 + 2 / -- acos"  "(3.141593,1.061275)"

# Hypotenuse / parallel operator ||  (binary in real mode)
check ""   "3 4 ||"         "5.000000"

# regression: || must consume both operands. If it leaks the 2nd (y=3),
# the following + silently sees 5+3=8; correct consumption leaves one
# operand, so + reports underflow and x stays 5.
check ""   "3 4 || +"       "+ ??? needs 2 operands
5.000000"
check ""   "3 4 5 || ||"    "7.071068"

# || in complex mode is the modulus (unary); stable under chaining
check "-c" "(3,4) ||"       "(5.000000,0.000000)"
check "-c" "(3,4) || ||"    "(5.000000,0.000000)"

# Lambert W
check ""   "1 W"            "0.567143"
check ""   "-0.1 W"         "-3.577152"
check ""   "-0.1 W xy"      "-0.111833"

# Factorial
check "" "5 !"              "120.000000"
check "" "5 sqrt !"         "2.513480"

# Examples in help
check "" "fix2 18 2 - 8 2 / *"                   "64.00"
check "" "2 -- complex sqrt"                     "(0.000000,-1.414214)"
check "-c" "radians (1,pi_over_2)p ^ * degrees"  "(1.000000,180.000000) p"

# Stats
set ref "    count n        -> 3
    means ux , uy  -> 3.000000 , 4.000000
  stddevs sx , xy  -> 1.632993 , 1.632993
  medians mx , my  -> 3.000000 , 4.000000
lower_qs lqx , lqy -> 2.000000 , 3.000000
upper_qs uqx , uqy -> 4.000000 , 5.000000

Regression:  y = ax + b
   gradient a      ->1.000000
  intercept b      -> 1.000000
 covariance cov    -> 2.666667
correlation corr   -> 1.000000"
check "" "{ 1,2 3,4 5,6 } cov"   $ref

# Stats - bad input must be rejected cleanly, never silently truncated or crash
# multi-comma token (e.g. 690,540,500) is malformed: reject, not read as (690,540)
check "" "{ 690,540,500 }"  "690,540,500 ???
empty set { } -- no statistics"
# trailing comma / empty half used to crash with forrtl severe (24)
check "" "{ 1, }"           "1, ???
empty set { } -- no statistics"
# empty set must not index off the end of a zero-length array
check "" "{ }"              "empty set { } -- no statistics"
# a single pair has no regression (was NaN from divide-by-zero)
check "" "{ 1,2 }"          "    count n        -> 1
    means ux , uy  -> 1.000000 , 2.000000
  stddevs sx , xy  -> 0.000000 , 0.000000
  medians mx , my  -> 1.000000 , 2.000000
lower_qs lqx , lqy -> 1.000000 , 2.000000
upper_qs uqx , uqy -> 1.000000 , 2.000000

Regression: needs at least 2 points"

# nint: nearest integer; an exact .5 tie rounds to the even neighbour
check "" "3.4 nint"    "3.000000"
check "" "3 nint"      "3.000000"
check "" "2.5 nint"    "2.000000"
check "" "3.5 nint"    "4.000000"
check "" "-2.5 nint"   "-2.000000"

# Permutations and combinations; n beyond gamma's 170! range must not overflow
check "" "5 2 npr"     "20.000000"
check "" "5 2 ncr"     "10.000000"
check "" "200 3 npr"   "7880400.000000"
check "" "200 3 ncr"   "1313400.000000"

# Cube root of a negative real is real, not the principal complex branch
check "" "8 -- cbrt"   "-2.000000"
check "" "27 ^/3"      "3.000000"

# Stack underflow reports an error and leaves the stack unchanged
check "" "5 +"         "+ ??? needs 2 operands
5.000000"
check "" "sin"         "sin ??? needs 1 operand
0.000000"

# Both Lambert-W roots are real stack entries (drop exposes W0 in y)
check "" "-0.1 W drop" "-0.111833"

# Binary ops convert a polar operand to cartesian before ops that read
# raw parts (atan2); the result stays polar
check "-c" "(1,60)p 1 atan2"  "(26.565051,0.000000) p"

# regression: an x-only sequence followed by a pair sequence used to write
# to an unallocated/stale y_seq (crash)
set ref "    count n -> 3
    mean ux -> 2.000000
  stddev sx -> 0.816497
  median mx -> 2.000000
lower_q lqx -> 1.500000
upper_q uqx -> 2.500000
    count n        -> 2
    means ux , uy  -> 2.000000 , 3.000000
  stddevs sx , xy  -> 1.000000 , 1.000000
  medians mx , my  -> 2.000000 , 3.000000
lower_qs lqx , lqy -> 2.000000 , 3.000000
upper_qs uqx , uqy -> 2.000000 , 3.000000

Regression:  y = ax + b
   gradient a      ->1.000000
  intercept b      -> 1.000000
 covariance cov    -> 1.000000
correlation corr   -> 1.000000"
check "" "{ 1 2 3 } { 1,2 3,4 }" $ref

# Summary
puts "\nPassed: $n_passed  Failed: $n_failed"
exit [expr {$n_failed > 0 ? 1 : 0}]
