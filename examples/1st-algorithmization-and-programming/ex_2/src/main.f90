! Объявляем программу.
program exercise_2
   ! Включаем модули Environment и IEEE_Arithmetic.
   use Environment
   use IEEE_Arithmetic  ! Для ieee_quiet_nan.
   ! Отключаем механизм задания типов переменных по умолчанию.
   implicit none
   ! Задаём именованные символьные константы с длинами,
    !  равными длинам постоянных выражений после знака =.
    ! * - это предполагаемый параметр длины.
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   ! Переменные интегрального типа (32-битные целые знаковые числа).
   integer                 :: In = 0, Out = 0
   ! Переменные вещественного типа (32-битные).
   real(R_)                :: x = 0, y = 0, z = 0, w = 0, fval = 0

   ! Открываем файл по пути из константы input_file.
   ! unit - это номер устройства, указывающего на файл.
   ! Номер может быть любым числом в диапазоне [9 - 99].
   ! Каждый открытый файл должен обладать уникальным значением unit.
   ! Однако в данном случае используется спецификатор newunit из Fortran 2008.
   ! newunit автоматически выбирает значение unit и сохраняет выбранное значение,
   !  в данном случае, в переменную In.
   open (file=input_file, newunit=In)
      ! Считываем из устройства с номером In
      ! значения в переменные x, y, z, w (вещественные 32-битные числа).
      ! * означает отсутствие формата чтения из файла.
      read (In, *) x, y, z, w
   close (In)

   ! Открываем файл по пути из константы output_file
   !  с кодировкой E_ из модуля Environment ("UTF-8").
   open (file=output_file, encoding=E_, newunit=Out)
      ! Пишем в файл с номером устройства Out в соответствии с форматом.
      write (Out, "(4(a, f0.2/))") "x = ", x, "y = ", y, "z = ", z, "w = ", w
   ! Закрываем устройство, ассоциируемое с номером Out.
   close (Out)

   ! Вызываем функцию F и присваиваем результат переменной fval.
   fval = F(x, y, z, w)

   ! Открываем файл по пути из константы output_file в режиме append.
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      ! Пишем в файл с номером устройства Out в соответствии с форматом.
      write (Out, "('f = ', f0.2)") fval
   ! Закрываем устройство, ассоциируемое с номером Out.
   close (Out)

   !fval = FImpure(x, y, z, w)

! Встроенные процедуры следуют после данного оператора.
contains
   ! Чистая функция.
   pure function F(x, y, z, w)
      real(R_) F, x, y, z, w
      ! Атрибут intent позволяет указать намерение,
      !  с которым аргументы используются в процедуре.
      ! in - Используется как входные значения,
      !  не изменяется в функции.
      intent(in)  x, y, z, w

      if (x>0 .and. z<5) then
         F = x*x + w
      else if (x>0 .and. z>=5) then
         F = cos(z) + x*y
      else if (x<0 .and. z>5) then
         F = w + cos(y)*x
      else
         ! Присвоение не числа -- NaN.
         F = IEEE_Value(x, IEEE_Quiet_NaN)
      end if
   end function F

   ! Нечистая функция с вычислениями и с вводом/выводом одновременно.
   ! Так реализовывать не нужно.
   ! Демонстрирует применение блока БЕЗ использования меток.
   function FImpure(x, y, z, w) result(F)
      real(R_) F, x, y, z, w
      intent(in)  x, y, z, w

      open (file=output_file, encoding=E_, newunit=Out, position='append')
         ! В данном случае блок поможет нам преждевременно выйти
         !  из него в ветке else, не дойдя до строчки 95.
         eval: block
            if (x>0 .and. z<5) then
               F = x*x + w
            else if (x>0 .and. z>=5) then
               F = cos(z) + x*y
            else if (x<0 .and. z>5) then
               F = w + cos(y)*x
            else
               write (Out, "('f is indetermined')")
               exit eval
            end if
            write (Out, "('f = ', f0.2)") F
         end block eval
      close (Out)
   end function FImpure
! Конец программы.
end program exercise_2
