module Matrix_Transformations_Module
   use Environment
   implicit none

contains
   ! 0 0 0 0 1
   ! 0 0 0 1 1
   ! 0 0 1 1 1
   ! 0 1 1 1 1
   ! 1 1 1 1 1
   subroutine Zero_Elements_Above_Side_Diagonal(matrix)
      real(R_), intent(inout) :: matrix(:, :)
      integer(I_)             :: Column, MatrixSize

      MatrixSize = size(array=matrix, dim=2)

      do Column = 1, MatrixSize - 1
         matrix(1:MatrixSize-Column, Column) = 0.0
      end do
   end subroutine Zero_Elements_Above_Side_Diagonal

end module Matrix_Transformations_Module
