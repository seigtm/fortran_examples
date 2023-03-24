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
   integer(I_)               :: in, matrix_size, out ! , i
   real(R_), allocatable     :: matrix(:, :)
   real(R_)                  :: s_sum_prod_sqrt, s_norm2_prod

   open(file=input_file, newunit=in)
      read(in, *) matrix_size
      allocate(matrix(matrix_size, matrix_size))
      read(in, *) matrix
      ! read(in, *) (matrix(i, :), i = 1, matrix_size)
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, '(a, " = ", i0)')  "matrix_size", matrix_size
      write(out, "(a)") "matrix:"
      write(out, "(" // matrix_size // "f6.2)") matrix
      ! write(out, "(" // matrix_size // "f6.2)") (matrix(i, :), i = 1, matrix_size)
      write(out, *)
   close(out)

   ! 1. Возводим элементы матрицы в квадрат;
   ! 2. Вычисляем сумму элементов в каждой строчке матрицы;
   ! 3. Перемножаем эти суммы;
   ! 4. Берём от результата квадратный корень.
   s_sum_prod_sqrt = Sqrt(Product(Sum(matrix ** 2, dim=1))) 
   ! 1. Вычисляем Евклидову векторную норму каждой строки матрицы:
   !      Norm2(A) == Sqrt(Sum(a_(ij)^2));
   ! 2. Перемножаем эти нормы.
   s_norm2_prod = Product(Norm2(matrix, dim=1))

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, '(2(a, T24, "= ", f0.6/))') "S (Sum, Product, SqRt)", s_sum_prod_sqrt, "S (Norm2, Product)", s_norm2_prod
   close(out)

end program SquareMatrix

