! Баранов К.П., группа 20021.
! Лабораторная работа №4, вариант 4.6 (в).

! Задача: вычислить следующий интеграл:
!  I[0.1 -> 0.4](exp(x) * x^2 * dx) при h = 0.002
! по формуле трапеций:
!  I = Integral(f(x)dx) ~ h[ f(a)/2 + f(a+h) + f(a+2h) + ... + f(b-h) + f(b)/2 ].

! Указания: Сперва вычислить все необходимые точки интерполяции
!  (для гарантированной векторизации).
! После провести вычисление функции в этих точках.
! Sum, использовать встроенные элементные функции.

program Integral
   use Environment
   use Trapezoidal_Rule
   use Trapezoidal_IO
   implicit none

   character(*), parameter   :: INPUT_FILE  = "../data/input.txt", &
                                OUTPUT_FILE = "output.txt"
   real(R_)                  :: a = 0, b = 0, h = 0, I = 0
   real(R_), allocatable     :: X(:)
   integer(I_)               :: N = 0

   call Read_Integral_Values(INPUT_FILE, a, b, h)
   call Output_Integral_Values(OUTPUT_FILE, a, b, h)
   ! Вычисляем количество шагов.
   N = Int((b - a) / h + .5_R_)
   ! Выделяем массив на это количество шагов.
   allocate(X(N))
   ! Вызываем нашу чистую подпрограмму.
   call Integrate(a, b, h, X, I)
   call Output_Integral(OUTPUT_FILE, I)
end program Integral
