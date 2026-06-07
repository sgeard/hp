! Implementations of the pure elementary-maths kernels.
!
! Every right-hand side here is lifted verbatim from the corresponding _fr
! function in rpn_stack_sm so that the extraction is behaviour-preserving;
! the _fr functions now delegate here, giving a single source of truth for
! each formula. real64 is host-associated from the parent module.
submodule (hp_maths) hp_maths_sm
    implicit none

contains

    ! --- powers / roots ---

    pure module function c_sqrt(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = sqrt(z)
    end function c_sqrt

    pure module function c_cbrt(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = z ** (1.0d0/3)
    end function c_cbrt

    ! --- sign / parts ---

    pure module function c_conj(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = conjg(z)
    end function c_conj

    pure module function c_negate(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = -z
    end function c_negate

    pure module function c_swap_reim(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = cmplx(aimag(z), real(z, real64), real64)
    end function c_swap_reim

    ! --- trigonometric (radians) ---

    pure module function c_sin(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = sin(z)
    end function c_sin

    pure module function c_cos(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = cos(z)
    end function c_cos

    pure module function c_tan(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = tan(z)
    end function c_tan

    pure module function c_asin(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = asin(z)
    end function c_asin

    pure module function c_acos(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = acos(z)
    end function c_acos

    pure module function c_atan(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = atan(z)
    end function c_atan

    ! --- hyperbolic ---

    pure module function c_sinh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = sinh(z)
    end function c_sinh

    pure module function c_cosh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = cosh(z)
    end function c_cosh

    pure module function c_tanh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = tanh(z)
    end function c_tanh

    pure module function c_asinh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = asinh(z)
    end function c_asinh

    pure module function c_acosh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = acosh(z)
    end function c_acosh

    pure module function c_atanh(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = atanh(z)
    end function c_atanh

    ! --- exponential / logarithmic ---

    pure module function c_exp(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = exp(z)
    end function c_exp

    pure module function c_exp2(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = 2 ** z
    end function c_exp2

    pure module function c_exp10(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = 10 ** z
    end function c_exp10

    pure module function c_ln(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = log(z)
    end function c_ln

    pure module function c_log2(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = log(z) / log(2.0d0)
    end function c_log2

    pure module function c_log10(z) result(w)
        complex(real64), intent(in) :: z
        complex(real64)             :: w
        w = log(z) / log(10.0d0)
    end function c_log10

    ! --- real-only kernels ---

    pure module function r_hypot(x, y) result(h)
        real(real64), intent(in) :: x, y
        real(real64)             :: h
        h = hypot(x, y)
    end function r_hypot

    pure module function r_gamma(x) result(g)
        real(real64), intent(in) :: x
        real(real64)             :: g
        g = gamma(x)
    end function r_gamma

    pure module function r_factorial(x) result(f)
        real(real64), intent(in) :: x
        real(real64)             :: f
        if (x == 0) then
            f = 1
        else
            f = gamma(x + 1)
        end if
    end function r_factorial

    pure module function r_atan2(y, x) result(a)
        real(real64), intent(in) :: y, x
        real(real64)             :: a
        a = atan2(y, x)
    end function r_atan2

end submodule hp_maths_sm
