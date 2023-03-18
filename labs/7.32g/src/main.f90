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
   integer(I_)               :: in, out, i, columns, rows
   real(R_), allocatable     :: matrix(:, :), matrix_row_abs(:)

   open(file=input_file, newunit=in)
      read(in, *) columns, rows ! columns - столбцов, rows - строк.
      allocate(matrix(columns, rows))
      ! Хранение в памяти по строкам.
      ! 1 индекс - столбца, 2 индекс - строки.
      read(in, *) matrix
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(2(a, " = ", i0/))')  "columns", columns, "rows", rows
      write(out, "(a)") "matrix:"
      write(out, "(" // columns // "f7.2)") (matrix(:, i), i = 1, rows)
      write(out, *)
   close(out)

   allocate(matrix_row_abs(columns))
   do concurrent(i = 1:rows)
      matrix_row_abs = abs(matrix(:, i))
      call SortMatrixRow(matrix(:, i), matrix_row_abs)
   end do

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, "(a)") "sorted matrix:"
      write(out, "(" // columns // "f7.2)") (matrix(:, i), i = 1, rows)
   close(out)

contains
   pure subroutine SortMatrixRow(matrix_row, matrix_row_abs)
      real(R_), intent(out) :: matrix_row(:), matrix_row_abs(:)
      integer(I_)           :: i, min_index
      real(R_)              :: tmp
      ! Сортировка элементов массива в порядке возрастания их модулей
      !  (от меньшего к большему) методом выбора:
      ! 1. Находим в неотсортированной части массива индекс минимального элемента.
      ! 2. Меняем его с первым элементом в неотсортированной части
      !    (если это не один и тот же элемент).
      ! 3. Отсортированная часть увеличилась. Повторяем пп. 1-2.
      do i = 1, Size(matrix_row_abs) - 1
         ! MinLoc вернёт нам индекс относительно i, потому нам нужно
         !  добавить к этому значению i и вычесть единицу.
         min_index = MinLoc(matrix_row_abs(i:), 1) + i - 1

         ! Если текущий индекс не является индексом минимального элемента.
         ! (Мы пропускаем замену за ненадобностью, если это один и тот же элемент).
         if (i /= min_index) then
            ! Меняем местами текущий и минимальный на данной итерации элементы.
            tmp                       = matrix_row_abs(i)
            matrix_row_abs(i)         = matrix_row_abs(min_index)
            matrix_row_abs(min_index) = tmp

            tmp                   = matrix_row(i)
            matrix_row(i)         = matrix_row(min_index)
            matrix_row(min_index) = tmp
         end if
      end do
   end subroutine SortMatrixRow

end program SortMatrixRows

