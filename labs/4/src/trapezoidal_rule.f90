module Trapezoidal_Rule
   use Environment
   implicit none

contains
   ! Чистая функция в императивном стиле.
   ! Вычисление приближенного значения интеграла
   !  методом трапеций:
   ! I = Integral(f(x)dx) ~
   !  ~ h[ f(a)/2 + f(a+h) + f(a+2h) + ... + f(b-h) + f(b)/2 ].
   ! Входные параметры:
   !  a - нижняя граница интеграла;
   !  b - верхняя граница интеграла;
   !  h - шаг.
   ! Выходные параметры:
   !  i - значение вычисленного приближенного интеграла.
   pure real(R_) function IntegrateImp(a, b, h) result(Integral)
      real(R_), intent(in) :: a, b, h
      real(R_)             :: x
      integer(I_)          :: steps, i
   
      ! (f(a) + f(b)) / 2.
      Integral = (exp(a) * a**2.0 + exp(b) * b**2.0) / 2.0
      ! Количество шагов для цикла.
      ! Вычитаем 1, так как по формуле мы идём от 1h до (steps - 1)h.
      steps = Int((b - a) / h) - 1

      ! f(a + h) + f(a + 2h) + ... + f(b - h)
      do i = 1, steps
         x = a + i * h  ! Каждую итерацию мы добавляем i штук по h.
         Integral = Integral + exp(x) * x**2.0
      end do

      ! Домножаем на h согласно методу трапеций.
      Integral = Integral * h
   end function IntegrateImp

   ! Чистая подпрограмма в регулярном стиле.
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
