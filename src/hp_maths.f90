! Pure elementary-maths kernels for the HP calculator (Phase 1, Layer 1).
!
! Each kernel is a pure function of value over complex(8) / real(8) only:
! no stack, no I/O, no angle/display/complex mode state, no engine state.
! Angle-unit scaling (degrees/radians), the polar/cartesian rpn_t
! representation, the multiple-roots table and Newton-Raphson all stay in
! the glue layer (rpn_stack_sm) -- they are not pure functions of value and
! so deliberately do not appear here.
!
! Naming: c_* take and return complex(8); r_* are real-only operations.
module hp_maths
    use iso_fortran_env, only: real64
    implicit none
    private

    ! Complex unary kernels
    public :: c_sqrt, c_cbrt, c_conj, c_negate, c_swap_reim
    public :: c_sin,  c_cos,  c_tan,  c_asin,  c_acos,  c_atan
    public :: c_sinh, c_cosh, c_tanh, c_asinh, c_acosh, c_atanh
    public :: c_exp,  c_exp2, c_exp10, c_ln,   c_log2,  c_log10
    ! Real-only kernels
    public :: r_cbrt, r_hypot, r_gamma, r_factorial, r_ncr, r_npr, r_atan2

    interface
        ! --- powers / roots ---
        pure module function c_sqrt(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_sqrt
        pure module function c_cbrt(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_cbrt

        ! --- sign / parts ---
        pure module function c_conj(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_conj
        pure module function c_negate(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_negate
        pure module function c_swap_reim(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_swap_reim

        ! --- trigonometric (radians: any angle-unit scaling is the caller's job) ---
        pure module function c_sin(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_sin
        pure module function c_cos(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_cos
        pure module function c_tan(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_tan
        pure module function c_asin(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_asin
        pure module function c_acos(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_acos
        pure module function c_atan(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_atan

        ! --- hyperbolic ---
        pure module function c_sinh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_sinh
        pure module function c_cosh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_cosh
        pure module function c_tanh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_tanh
        pure module function c_asinh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_asinh
        pure module function c_acosh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_acosh
        pure module function c_atanh(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_atanh

        ! --- exponential / logarithmic ---
        pure module function c_exp(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_exp
        pure module function c_exp2(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_exp2
        pure module function c_exp10(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_exp10
        pure module function c_ln(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_ln
        pure module function c_log2(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_log2
        pure module function c_log10(z) result(w)
            complex(real64), intent(in) :: z
            complex(real64)             :: w
        end function c_log10

        ! --- real-only kernels ---
        pure module function r_cbrt(x) result(y)
            real(real64), intent(in) :: x
            real(real64)             :: y
        end function r_cbrt
        pure module function r_hypot(x, y) result(h)
            real(real64), intent(in) :: x, y
            real(real64)             :: h
        end function r_hypot
        pure module function r_gamma(x) result(g)
            real(real64), intent(in) :: x
            real(real64)             :: g
        end function r_gamma
        pure module function r_factorial(x) result(f)
            real(real64), intent(in) :: x
            real(real64)             :: f
        end function r_factorial
        pure module function r_ncr(n, r) result(c)
            real(real64), intent(in) :: n, r
            real(real64)             :: c
        end function r_ncr
        pure module function r_npr(n, r) result(p)
            real(real64), intent(in) :: n, r
            real(real64)             :: p
        end function r_npr
        pure module function r_atan2(y, x) result(a)
            real(real64), intent(in) :: y, x
            real(real64)             :: a
        end function r_atan2
    end interface

end module hp_maths
