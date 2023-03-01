! Баранов К.П., группа 20021.
! Лабораторная работа №6, вариант 6.2 (ж).

! Задача: вычислить сумму членов рядов, представляющих
!  значения следующих функций (суммирование производить
!  до тех пор, пока отношение текущего члена ряда к
!  накопленной сумме не станет меньше заданной величины
!  RELERR):
!  ln(x) = (x-1)/x + ((x-1)^2)/(2x^2) + ((x-1)^3)/(3x^3) + ... ,
!   x > 1/2.

! Указания: проводить вычисления, пока сумма не перестанет меняться
!  (см. решение примера). Очередной член (или очередной числитель и
!  знаменатель) вычислять относительно предыдущего. Если возможно,
!  то разницу между членами вычислять один раз до цикла (например x^2).
! В некоторых случаях шаг цикла удобно делать равным 2 (например, если
!  требуются только факториалы чётных чисел). Начальные значения
!  переменным давать до цикла. Сравнить результат со встроенной функцией.
! Меняя разновидность вещественного типа на двойную и четверную точность,
!  посмотреть, сколько членов ряда потребуется для сходимости.

program LnX
   use Environment
   use IEEE_Arithmetic  ! Для ieee_quiet_nan.
   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt", &
                              fmt         = '(a, T15, "= ", e13.6)'
   integer(I_)             :: in, out
   real(R_)                :: x, log_x

   open(file=input_file, newunit=in)
      read(In, *) x
   close(In)

   open(file=output_file, encoding=E_, newunit=out)
      write(Out, fmt) 'x', x
      write(Out, *)
   close(In)

   ! Вычисляем значение натурального логарифма нашей функцией.
   log_x = Ln_X(x)

   open(file=output_file, encoding=E_, newunit=out, position="append")
      ! Выводим наш ln(x), встроенный ln(x) и разницу между ними (ошибку).
      write (Out, fmt) "ln(x)",         log_x
      write (Out, fmt) "Fortran ln(x)", log(x)
      write (Out, fmt) "Error",         log_x - log(x)
   close(Out)

contains
   ! Реализация, в которой мы вычисляем текущие
   !  x и (x - 1) относительно предыдущих.
   real(R_) pure function Ln_X(x)
      real(R_), intent(in) :: x
      real(R_)             :: dividend, divisor, x_current, x_minus_1, old_ln_x
      integer(I_)          :: n

      ! Если x <= 0.5, то возвращаем NaN,
      !  т.к. по условию задачи x > 0.5.
      if (x <= 0.5) then
         Ln_X = IEEE_Value(x, IEEE_Quiet_NaN)
         return
      end if

      x_current = x  ! Повышает свою степень с каждой итерацией.

      ! Числитель первого члена ряда: x - 1.
      x_minus_1 = x_current - 1
      dividend = x_minus_1

      ! Знаменатель первого члена ряда: x.
      divisor = x_current
      ! Переменная для знаменателя, инкрементируемая с каждой итерацией.
      n = 1

      ! Вычисляем первый член ряда ln(x).
      Ln_X = dividend / divisor

      ! Цикл с постусловием: пока сумма не перестанет меняться.
      do
         ! Сохраняем текущую сумму для нашего постусловия (см. выше).
         old_ln_x  = Ln_X

         ! Числитель: (x - 1)^n.
         dividend = dividend * x_minus_1

         ! Знаменатель: n * x^n.
         n = n + 1
         x_current = x_current * x
         divisor = n * x_current

         ! Вычисление текущего члена ряда �� прибавление его к общей сумме.
         Ln_X = Ln_X + dividend / divisor

         ! Если сумма перестала меняться, выходим.
         if (old_ln_x == Ln_X) exit
      end do
   end function Ln_X

   ! Реализация, которая не удовлетворяет условию
   !  "очередной член вычислять относительно предыдущего",
   !  но которая, на мой взгляд, читаемее.
   real(R_) pure function Ln_X_Impl(x)
      real(R_), intent(in) :: x
      real(R_)             :: x_minus_1, old_ln_x
      integer(I_)          :: n

      ! Если x <= 0.5, то возвращаем NaN,
      !  т.к. по условию задачи x > 0.5.
      if (x <= 0.5) then
         Ln_X_Impl = IEEE_Value(x, IEEE_Quiet_NaN)
         return
      end if

      ! Ln_X изначально равен нулю.
      Ln_X_Impl = 0.0_R_
      ! Вычисляем заранее x - 1, чтобы не вычислять каждый раз.
      x_minus_1 = x - 1.0_R_
      ! От итерации с итерации мы на самом деле добавляем к предыдущей сумме:
      !  ((x - 1)^n) / (n * x^n), где n [1; +inf).
      ! По этой причине присваиваем n значение 1 и будем инкрементировать его.
      n = 1

      ! Цикл с постусловием: пока сумма не перестанет меняться.
      do
         old_ln_x  = Ln_X_Impl
         Ln_X_Impl = Ln_X_Impl + x_minus_1**n / (n * x**n)
         if (old_ln_x == Ln_X_Impl) exit
         n = n + 1
      end do
   end function Ln_X_Impl

end program LnX
