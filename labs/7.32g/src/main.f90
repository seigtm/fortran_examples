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
   integer(I_)               :: in, out, i, n, m
   real(R_), allocatable     :: matrix(:, :)

   open(file=input_file, newunit=in)
      read(in, *) n, m
      allocate(matrix(n, m))
      read(in, *) (matrix(i, :), i = 1, n)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(2(a, " = ", i0/))')  "n", n, "m", m
      write(out, "(a)") "matrix:"
      write(out, "(" // m // "f7.2)") (matrix(i, :), i = 1, n)
      write(out, *)
   close(out)

   ! TODO: code here...

   open(file=output_file, encoding=E_, newunit=out, position="append")
      ! TODO: output results.
   close(out)

end program SortMatrixRows

