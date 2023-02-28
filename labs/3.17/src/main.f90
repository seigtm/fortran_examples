! Баранов К.П., группа 20021.
! Лабораторная работа №3, вариант 3.17.

! Задача: записать нули на место элементов квадратной
!  матрицы A(50, 50), расположенных выше подобной диагонали.

program MatrixTransformations
   use Environment
   use Matrix_Transformations_Module
   use Matrix_Transformations_IO
   implicit none

   character(*), parameter   :: INPUT_FILE  = "../data/input.txt", &
                                OUTPUT_FILE = "output.txt"
   real(R_),     allocatable :: Matrix(:, :)

   call Read_Matrix(INPUT_FILE, Matrix)
   call Output_Matrix(OUTPUT_FILE, Matrix, "rewind")
   call Zero_Elements_Above_Side_Diagonal(Matrix)
   call Output_Matrix(OUTPUT_FILE, Matrix, "append")
end program MatrixTransformations
