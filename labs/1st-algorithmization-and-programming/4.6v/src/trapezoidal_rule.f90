module Trapezoidal_Rule
   use Environment
   implicit none

contains
   ! Чистая подпрограмма в регулярном стиле.
   ! Вычисление приближенного значения интеграла
   !  методом трапеций:
   ! I = Integral(f(x)dx) ~
   !  ~ h[ f(a)/2 + f(a+h) + f(a+2h) + ... + f(b-h) + f(b)/2 ].
   pure subroutine Integrate(a, h, X, I)
      real(R_), intent(in)  :: a, h
      real(R_), intent(out) :: X(:), I
      integer(I_)           :: j

      ! Заполняем массив значениями:
      !  [ a, a+h, a+2h, ..., b-h, b ].
      X = [(a + j * h, j = 0, Size(X) - 1)]

      ! Производим непосредственное вычисление f(x) = exp(x) * x^2.
      !  [ f(a), f(a+h), f(a+2h), ..., f(b-h), f(b) ].
      X = Exp(X) * X**2.0_R_

      ! Первый и последний члены у нас разделены на 2:
      !  [ f(a)/2, f(a+h), f(a+2h), ..., f(b-h), f(b)/2 ].
      X(1) = X(1) / 2
      X(Size(X)) = X(Size(X)) / 2

      ! Суммируем элементы и домножаем на h,
      !  тем самым вычисляя значение интеграла.
      I = Sum(X) * h
   end subroutine Integrate
end module Trapezoidal_Rule

