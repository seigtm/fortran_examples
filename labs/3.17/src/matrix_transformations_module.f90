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
      ! DO CONCURRENT указывает, что отдельные итерации цикла
      !  не имеют взаимозависимостей.
      ! Порядок выполнения итераций может быть произвольным.
      ! Тем самым, показываем компилятору, что этот цикл можно распараллелить.
      do concurrent (Column=1:MatrixSize - 1)
         ! Проход по столбцам: как расположена матрица в памяти.
         matrix(1:MatrixSize-Column, Column) = 0.0
      end do
   end subroutine Zero_Elements_Above_Side_Diagonal

end module Matrix_Transformations_Module
