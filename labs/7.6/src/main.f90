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
   ! use Square_Matrix_Mod
   ! use Square_Matrix_IO
   implicit none

   character(*), parameter   :: INPUT_FILE  = "../data/input.txt", &
                                OUTPUT_FILE = "output.txt"
end program SquareMatrix
