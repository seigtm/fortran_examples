! Баранов К.П., группа 20021.
! Лабораторная работа №7, вариант 7.32г.

! Задача: упорядочить элементы в строках матрицы B(10,15) так, чтобы:
!          |b_(ij)| <= |b_(i(j+1))|.

! Указания:
!  1) Использовать do concurrent для внешнего цикла по строкам.
!  2) Ходить по самим строкам при сортировке при размещении матрицы в памяти по столбцам неэффективно.
!  Матрицу B лучше хранить и обрабатывать в транспонированном виде, чтобы проходя по строкам,
!   читать элементы в порядке их расположения в памяти.
!  3) Вычислять модуль элементов каждый раз неэффективно.
!  4) Можно перед сортировкой сформировать: массив из модулей элементов строки AbsS(:)
!  и массив номеров элементов в строке Indexes(:). Сортируя первый из них, отсортировать и
!  второй. После сортировки присвоить к строке ту же строку с отсортированным порядком эле-
!  ментов, используя массив индексов как вектор индексов: AbsS(:) = B(i, Indexes()), B(i, :) =
!  AbsS или при хранении в транспонированном виде AbsS(:) = B(Indexes(), i), B(:, i) =
!  B(Indexes(), i). См. указания к сортировке.
!  Такой подход требует использования нерегулярного обращения к массиву – B(i,
!  Indexes()). Для того, чтобы избежать этого, перед сортировкой лучше сформировать только
!  массив из модулей элементов строки AbsA(:). Сортируя первый из них, сортировать и строку.

program SortMatrixRows
   use Environment
   implicit none

   character(*), parameter   :: input_file  = "../data/input.txt", output_file = "output.txt"
   integer(I_)               :: in, out, i, rows, columns
   real(R_), allocatable     :: matrix(:, :), matrix_abs(:, :)

   open(file=input_file, newunit=in)
      read(in, *) rows, columns  ! rows - строк, columns - столбцов.
      ! Хранение в памяти по строкам.
      ! 1 индекс - столбца (j), 2 индекс - строки (i).
      allocate(matrix(columns, rows))
      read(in, *) matrix
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(2(a, " = ", i0/))')  "columns", columns, "rows", rows
      write(out, "(a)") "matrix:"
      write(out, "(" // columns // "f7.2)") (matrix(:, i), i = 1, rows)
      write(out, *)
   close(out)

   matrix_abs = Abs(matrix)
   call SortMatrix(matrix, matrix_abs)

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, "(a)") "sorted matrix:"
      write(out, "(" // columns // "f7.2)") (matrix(:, i), i = 1, rows)
   close(out)

contains
   pure subroutine SortMatrix(matrix, matrix_abs)
      real(R_), intent(out) :: matrix(:, :), matrix_abs(:, :)
      integer(I_)           :: i, j, min_index
      real(R_)              :: tmp

      do concurrent(i = 1:UBound(matrix, 2))  ! Векторизация.
         do j = 1, UBound(matrix, 1)
            min_index = MinLoc(matrix_abs(j:, i), dim=1) + j - 1
            if (j /= min_index) then
               tmp                  = matrix(j, i)
               matrix(j, i)         = matrix(min_index, i)
               matrix(min_index, i) = tmp

               tmp                      = matrix_abs(j, i)
               matrix_abs(j, i)         = matrix_abs(min_index, i)
               matrix_abs(min_index, i) = tmp
            end if
         end do
      end do
   end subroutine SortMatrix

end program SortMatrixRows
