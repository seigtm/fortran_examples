module Matrix_Transformations_Module
   use Environment
   implicit none

contains
   ! 0 0 0 0 1
   ! 0 0 0 1 1
   ! 0 0 1 1 1
   ! 0 1 1 1 1
   ! 1 1 1 1 1
   pure subroutine Zero_Elements_Above_Side_Diagonal(matrix)
      real(R_), intent(out) :: matrix(:, :)
      integer(I_)           :: Column, MatrixSize

      MatrixSize = size(array=matrix, dim=2)
      ! Проход по столбцам: как расположена матрица в памяти.
      ! Считываем матрицу из файла в таком виде по столбцам:
      !  [ 1, 2, 3 ]
      !  [ 4, 5, 6 ]
      !  [ 7, 8, 9 ]
      !   (в памяти) => [ 1, 4, 7, 2, 5, 8, 3, 6, 9 ]
      !      (обход) => [ 1, 4, 2 ].
      do Column=1, MatrixSize - 1
         matrix(1:MatrixSize-Column, Column) = 0
      end do
   end subroutine Zero_Elements_Above_Side_Diagonal

end module Matrix_Transformations_Module
