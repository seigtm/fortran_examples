! Баранов К.П., группа 20021.
! Лабораторная работа №7, вариант 7.41+().

! Задача: преобразовать двумерный массив A(20, 30)
!  в одномерный B(600), просматривая массив A
!  по столбцам (строкам).

! Указания: правильно выбрать структуру данных.
! Конструктор массива, в котором указан исходный массив.

program ArrayConverter
   use Environment
   implicit none

   character(*), parameter   :: input_file  = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable     :: matrix_2d(:, :)
   real(R_), allocatable     :: matrix_1d(:)
   integer(I_)               :: in, n, m, i, out

   open(file=input_file, newunit=in)
      read(in, *) n, m
      allocate(matrix_2d(n, m))
      ! Хранение в памяти по столбцам.
      read(in, *) (matrix_2d(i, :), i = 1, n)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(2(a, " = ", i0/))') "n", n, "m", m
      write(out, "(a)") "matrix_2d:"
      write(out, "(" // m // "f7.2)") (matrix_2d(i, :), i = 1, n)
      write(out, *)
   close(out)

   ! Сформируется по столбцам, то есть:
   !  [ 1 2 3 ]
   !  [ 4 5 6 ]
   !            => [ 1 4 2 5 3 6 ].
   matrix_1d = [matrix_2d]

   ! Сформируется по строкам, то есть:
   !  [ 1 2 3 ]
   !  [ 4 5 6 ]
   !            => [ 1 2 3 4 5 6 ].
   ! matrix_1d = [(matrix_2d(i, :), i = 1, n)]

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, "(a)") "matrix_1d:"
      write(out, "(" // size(matrix_1d) // "f7.2)") matrix_1d
   close(out)

end program ArrayConverter
