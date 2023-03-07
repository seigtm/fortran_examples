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

   open(file=output_file, encoding=E_, newunit=out, position="append") 
      ! В любом случае выводим результат вычисления встроенной функцией.
      write (Out, fmt) "Fortran ln(x)", log(x)

      if (x <= 0.5) then
         ! Выводим информацию о том, что наш ряд не сходится для x <= 0.5.
         write (Out, fmt) "Ln_X() не определён для заданного x", x
      else
         ! Вычисляем значение натурального логарифма нашей функцией.
         log_x = Ln_X(x)
         ! Выводим наш ln(x), встроенный ln(x) и разницу между ними (ошибку).
         write (Out, fmt) "ln(x)", log_x
         write (Out, fmt) "Error", log_x - log(x)
      end if
   close(Out)

contains
   ! (x-1)/x * [(x-1)/x / n]
   real(R_) pure function Ln_X(x)
      real(R_), intent(in) :: x
      real(R_)             :: dividend, old_ln_x, current_dividend
      integer(I_)          :: n

      n = 1
      ! Вычисляем первый член ряда ln(x).
      dividend = (x - 1) / x
      current_dividend = dividend
      Ln_X = dividend 

      ! Цикл с постусловием: пока сумма не перестанет меняться.
      do
         ! Сохраняем текущую сумму для нашего постусловия (см. выше).
         old_ln_x  = Ln_X
         ! Вычисление текущего члена ряда и прибавление его к общей сумме.
         current_dividend = current_dividend * dividend
         n = n + 1
         Ln_X = Ln_x + current_dividend / n
         ! Если сумма перестала меняться, выходим.
         if (old_ln_x == Ln_X) &
            exit
      end do
   end function Ln_X
end program LnX
