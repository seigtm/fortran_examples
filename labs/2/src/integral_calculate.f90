module Integral_calculate
   use Environment
   use IEEE_Arithmetic  ! Для ieee_quiet_nan.
   implicit none

contains
   pure real(R_) function Inverse_Sqrt(number)
      real(R_), intent(in) :: number

      Inverse_Sqrt = 1.0 / sqrt(number)
   end function Inverse_Sqrt

   pure real(R_) function Calculate_Delta(a, b, c) result(delta)
      real(R_), intent(in) :: A, B, C

      delta = 4 * a * c - b**2
   end function Calculate_Delta

   pure real(R_) function Integral(a, b, c, x)
      real(R_), intent(in)  :: a, b, c, x
      real(R_)              :: Delta, R, TwoCXPlusB

      TwoCXPlusB = 2 * c * x + b
      R          = a + b * x + c * x**2
      Delta      = Calculate_Delta(a, b, c)

      if (c > 0 .and. TwoCXPlusB > 0 .and. Delta == 0) then
         Integral = Inverse_Sqrt(c) * log(TwoCXPlusB)
      elseif (c < 0 .and. Delta < 0) then
         ! Если мы не сделаем -c, то всегда будем получать NaN.
         Integral = -Inverse_Sqrt(-c) * asin(TwoCXPlusB / sqrt(-Delta))
      elseif (c > 0) then
         Integral = Inverse_Sqrt(c) * log(abs(2 * sqrt(c * R) + TwoCXPlusB))
      else
         Integral = IEEE_Value(x, IEEE_Quiet_NaN)
      end if
   end function Integral

end module Integral_calculate
