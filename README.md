# hp
## Command-line calculator

This is an *rpn* calclulator with a maximum stack size of 5. It has full support for real and complex numbers
and will calculate summary statistics for a set of reals of real pairs. Spaces are important since they are used
to distinguish tokens.

## Building

This code does not build with the default compiler on most systems; it is only known to build
with *ifort* from Intel's OneAPI or _nagfor_ from NAG (thanks to https://github.com/nncarlson).

The Intel compiler if freely available (you'll need both *basic* and *hpc* to get the Fortran compiler):
https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html 

Makefiles are provided that will build this application on Windows or Linux using either *nmake* or *make* respectively.

## Basic help

```
hp -h

Command Calculator
==================

Introduction
------------

This is a command-line calculator. It supports both real and complex modes, as well
as degrees/radians selection and precision control. It can be run interactively or as an
expression parser. This help is deliberately terse to encourage exploration.

-------------------------------------------------------------------------------
Operators: + - * / ^ ^/x ^x ^2 ^/2 ^3 ^/3 ^*2 ^*10 || ! %
Constants: pi e g G c two_pi pi_over_2
Functions: sin cos tan asin acos atan sinh cosh tanh log2 log lg len sq sqrt cb cbrt
           alog2 alog alog10 gamma ncr npr rem int nint
 Controls: fix[0-9] clx cl cla 
    Modes: real complex verbose terse degrees radians
 Memories: n=0...9  st<n> sw<n> rc<n> cl<n> m<n>+ m<n>- m<n>* m<n>/ msh
  Complex: ri _ || to_pol to_cart
  Actions: 1/ -- R r ? > < split drop
    Stats: { x1 x2 ... } { x1,y1 x2,y2 ... }
           n ux sx mx lqx uqx uy sy my lqy uqy a b cov corr
    Quits: q =

-------------------------------------------------------------------------------

Examples
--------
    hp "fix2 18 2 - 8 2 / * ="                    -> 64.00
    hp "2 -- complex sqrt ="                      -> (0.00000,-1.414214)
    hp -c "radians (1,pi_over_2)p ^ * degrees ="  -> (1.000000,180.000000) p
```

## Build and Test with FPM

   
   To build with fpm(1) 
   ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
   enter:

   ```bash
        git clone https://github.com/sgeard/hp.git
        cd hp
        fpm test --compiler ifort
        fpm run --compiler ifort
        
        # or using the response file (saves writing --compiler ifort everywhere)
        fpm @build
        fpm @test
        fpm @run
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        hp        = { git = "https://github.com/sgeard/hp.git" }
```

