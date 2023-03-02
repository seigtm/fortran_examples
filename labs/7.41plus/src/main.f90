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

   character(*), parameter   :: input_file_1  = "../data/input1.txt", &
                                input_file_2  = "../data/input2.txt", &
                                output_file = "output.txt"
   real(R_), allocatable     :: matrix_2d_1(:, :), matrix_2d_2(:, :)
   real(R_), allocatable     :: matrix1_by_columns(:), matrix2_by_rows(:)
   integer(I_)               :: in, n1, m1, n2, m2, i, out

   ! Считываем первую двумерную матрицу.
   open(file=input_file_1, newunit=in)
      read(in, *) n1, m1
      allocate(matrix_2d_1(n1, m1))
      ! Хранение в памяти по столбцам.
      read(in, *) (matrix_2d_1(i, :), i = 1, n1)
   close(in)

   ! Считываем вторую двумерную матрицу.
   open(file=input_file_2, newunit=in)
      read(in, *) n2, m2
      allocate(matrix_2d_2(n2, m2))
      ! Хранение в памяти по столбцам.
      read(in, *) (matrix_2d_2(i, :), i = 1, n2)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      ! Первая матрица.
      write(out, "(a)") "matrix_2d_1:"
      write(out, '(2(a, " = ", i0/))') "n", n1, "m", m1
      write(out, "(" // m1 // "f7.2)") (matrix_2d_1(i, :), i = 1, n1)
      write(out, *)

      ! Вторая матрица.
      write(out, "(a)") "matrix_2d_2:"
      write(out, '(2(a, " = ", i0/))') "n", n2, "m", m2
      write(out, "(" // m2 // "f7.2)") (matrix_2d_2(i, :), i = 1, n2)
      write(out, *)
   close(out)

   ! Сформируется из первой матрицы по столбцам, то есть:
   !  [ 1 2 3 ]
   !  [ 4 5 6 ]
   !            => [ 1 4 2 5 3 6 ].
   matrix1_by_columns = [matrix_2d_1]

   ! Сформируется из второй матрицы по строкам, то есть:
   !  [ 1 2 3 ]
   !  [ 4 5 6 ]
   !            => [ 1 2 3 4 5 6 ].
   matrix2_by_rows = [(matrix_2d_2(i, :), i = 1, n2)]

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, "(a)") "matrix1_by_columns:"
      write(out, "(" // size(matrix1_by_columns) // "f7.2)") matrix1_by_columns
      write(out, "(a)") "matrix2_by_rows:"
      write(out, "(" // size(matrix2_by_rows) // "f7.2)") matrix2_by_rows
   close(out)

end program ArrayConverter
