module Trapezoidal_Rule
   use Environment
   implicit none

contains
   ! Чистая подпрограмма в регулярном стиле.
   ! Вычисление приближенного значения интеграла
   !  методом трапеций:
   ! I = Integral(f(x)dx) ~
   !  ~ h[ f(a)/2 + f(a+h) + f(a+2h) + ... + f(b-h) + f(b)/2 ].
   pure subroutine Integrate(a, b, h, X, I)
      real(R_), intent(in)  :: a, b, h
      real(R_), intent(out) :: X(:), I
      real(R_)              :: addition
      integer(I_)           :: j

      ! Заполняем массив значениями: a+h, a+2h, ..., b-h.
      X = [(a + j * h, j = 1, Size(X) - 1)]
      ! Производим непосредственное вычисление exp(x) * x^2.
      X = Exp(X) * X**2.0_R_
      ! Часть, которую не вычислить в цикле, но которая нам необходима.
      addition = (exp(a) * a**2.0_R_ + exp(b) * b**2.0_R_) / 2.0_R_
      ! Вычисляем интеграл, не забыв добавить вышеупомянутую часть.
      I = (Sum(X) + addition) * h
   end subroutine Integrate

end module Trapezoidal_Rule
