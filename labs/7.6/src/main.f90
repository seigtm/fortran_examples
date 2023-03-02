! Баранов К.П., группа 20021.
! Лабораторная работа №7, вариант 7.6.

! Задача: вычислить значение величины:
!  S = sqrt(prod(i=1 to n) * sum(j=1 to n) * a_(ij)^2),
!  если матрица A = (a_(ij)) квадратная и содержит
!  900 элементов.

! Указания: вычисления провести, используя два подхода:
!  1) Sum, Product, SqRt;
!  2) Norm2, Product.
! Сравнить результат двух подходов.
! Использовать параметр dim в Sum и Norm2.

program SquareMatrix
   use Environment
   implicit none

   character(*), parameter   :: input_file  = "../data/input.txt", output_file = "output.txt"
   integer(I_)               :: in, matrix_size, row, out
   real(R_), allocatable     :: matrix(:, :)
   real(R_)                  :: s_sum_prod_sqrt, s_norm2_prod

   open(file=input_file, newunit=in)
      read(in, *) matrix_size
      allocate(matrix(matrix_size, matrix_size))
      ! Хранение в памяти по столбцам.
      read (in, *) (matrix(row, :), row = 1, matrix_size)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(a, " = ", i0)')  "matrix_size", matrix_size
      write(out, "(a)") "matrix:"
      write(out, "(" //matrix_size // "f6.2)") (matrix(row, :), row = 1, matrix_size)
      write(out, *)
   close(out)

   s_sum_prod_sqrt = CalculateS_SumProdSqrt(matrix)
   s_norm2_prod = CalculateS_Norm2Prod(matrix)

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, '(2(a, T24, "= ", f0.6/))') "S (Sum, Product, SqRt)", s_sum_prod_sqrt, "S (Norm2, Product)", s_norm2_prod
   close(out)

contains
   real(R_) pure function CalculateS_SumProdSqrt(matrix) result(s)
      real(R_), intent(in) :: matrix(:, :)
      real(R_)             :: matrix_squared(size(matrix, dim=2), size(matrix, dim=2))
      real(R_)             :: row_sums(Size(matrix, dim=2))
      integer(I_)          :: i

      ! 1. Возводим все элементы матрицы в квадрат.
      matrix_squared = matrix ** 2.0_R_
      ! 2. Вычисляем сумму элементов каждой строки матрицы.
      row_sums = [(Sum(matrix_squared(i,:)), i = 1, size(matrix_squared, dim=2))]
      ! 3. Перемножаем эти суммы и берём от результата квадратный корень.
      s = Sqrt(Product(row_sums))
   end function CalculateS_SumProdSqrt

   real(R_) pure function CalculateS_Norm2Prod(matrix) result(s)
      real(R_), intent(in) :: matrix(:, :)

      ! 1. Вычисляем Евклидову векторную норму каждой строки матрицы.
      !    Norm2(A) == Sqrt(Sum(a_(ij)^2)).
      ! 2. Перемножаем эти нормы.
      s = Product(Norm2(matrix, dim=2))
   end function CalculateS_Norm2Prod

end program SquareMatrix
