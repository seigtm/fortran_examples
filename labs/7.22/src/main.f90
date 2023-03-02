! Баранов К.П., группа 20021.
! Лабораторная работа №7, вариант 7.22.

! Задача: в матрице B(100, 20) найти разность
!  между наибольшим и наименьшим значениями
!  элементов.

! Указания: MaxVal.

program MinMaxDiff
   use Environment
   implicit none

   character(*), parameter   :: input_file  = "../data/input.txt", output_file = "output.txt"
   integer(I_)               :: in, n, m, i, out
   real(R_), allocatable     :: matrix(:, :)
   real(R_)                  :: difference

   open(file=input_file, newunit=in)
      read(in, *) n, m
      allocate(matrix(n, m))
      ! Хранение в памяти по столбцам.
      read(in, *) (matrix(i, :), i = 1, n)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(2(a, " = ", i0/))') "n", n, "m", m
      write(out, "(a)") "matrix:"
      write(out, "(" // m // "f7.2)") (matrix(i, :), i = 1, n)
      write(out, *)
   close(out)

   difference = MaxVal(matrix) - MinVal(matrix)  ! max - min.

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, '(a, " = ", f0.2)') "difference (max - min)", difference
   close(out)
end program MinMaxDiff
