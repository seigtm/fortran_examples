! Баранов К.П., группа 20021.
! Лабораторная работа №1, вариант 1.2.

! Задача: для заданных вещественных значений a, b, c, d, e, f, x
!  вычислить значение полинома:
!   p(x) = ax^5 + bx^4 + cx^3 + dx^2 + ex + f.

! Указания: Коэффициенты записывать в массив A.
!  Завести массив X со значениями x^n (неявный цикл).
!  Произвести скалярное произведение X на массив коэффициентов A (Dot_product).

program Polynomial
   use Environment
   implicit none

   ! Количество одночленов по условию задачи.
   integer(I_), parameter :: MONOMIAL_COUNT = 6
   ! Начальное значение неявного цикла - максимальная экспонента переменной x.
   integer(I_), parameter :: EXPONENT_MAX = MONOMIAL_COUNT - 1
   ! Переменная для неявного цикла.
   integer(I_)            :: Exponent = 0
   ! Переменные для хранения номеров устройств ввода/вывода.
   integer  :: In = 0, Out = 0
   ! Переменная x.
   real(R_) :: X = 0
   ! Массив со значениями x^n (переменная, возведённая в степень. Без коэффициентов).
   real(R_) :: XArray(MONOMIAL_COUNT)
   ! Массив коэффициентов полинома (32-битные вещественные числа).
   real(R_) :: Coefficients(MONOMIAL_COUNT)
   ! Пути к входному и выходному файлу и формат вывода данных.
   character(*), parameter :: INPUT_FILE  = "../data/input.txt", &
                              OUTPUT_FILE = "output.txt",        &
                              FMT         = "(a, ' = (', f0.3, ')')"

   open (file=INPUT_FILE, newunit=In)
      ! Считываем коэффициенты (a, b, c, d, e, f), а также переменную x.
      read (In, *) Coefficients, x
   close (In)

   open (file=OUTPUT_FILE, encoding=E_, newunit=Out)
      ! Выводим считанные данные в выходной файл в соответствии с форматом.
      write (Out, FMT) "a", Coefficients(1)
      write (Out, FMT) "b", Coefficients(2)
      write (Out, FMT) "c", Coefficients(3)
      write (Out, FMT) "d", Coefficients(4)
      write (Out, FMT) "e", Coefficients(5)
      write (Out, FMT) "f", Coefficients(6)
      write (Out, FMT) "x", X
   close (Out)

   ! Заполняем массив x_arr неявным циклом (x^5, x^4, ... x^0).
   XArray = [(X**Exponent, Exponent = EXPONENT_MAX, 0, -1)]

   open (file=OUTPUT_FILE, encoding=E_, newunit=Out, position="append")
      write (Out, *) ! Дописываем в файл пустую строчку.
      ! Вычисляем значение полинома при помощи встроенной dot_product.
      ! Дописываем результат в файл в соответствии с форматом.
      write (Out, FMT) "p(x)", dot_product(XArray, Coefficients)
   close (Out)

end program Polynomial
