module Norm2_Module
   use Environment
   implicit none

contains
   real(R_) pure function Euclidean_Norm(matrix)
      real(R_), intent(in) :: matrix(:, :)
      integer(I_)          :: column, row

      Euclidean_Norm = 0
      ! Вычисляем сумму квадратов элементов каждой строки матрицы.
      do row = 1, Size(matrix, dim=1)
         do column = 1, Size(matrix, dim=1)
            Euclidean_Norm = Euclidean_Norm + matrix(row, column) ** 2.0_R_
         end do
      end do
      ! Берём от результата квадратный корень.
      Euclidean_Norm = Sqrt(Euclidean_Norm)
   end function Euclidean_Norm

   subroutine Special_Case(matrix)
      real(R_), intent(out) :: matrix(:, :)
      real(R_) :: multiplied(Size(matrix, dim=1), Size(matrix, dim=1))
      integer(I_) :: matrix_size, i, j, k

      matrix_size = Size(matrix, dim=1)
      multiplied = 0
      ! Вычисляем матрицу Special_Case = E + matrix + 1/2 * matrix^2:
      !  1. Возводим матрицу в квадрат.
      !     Мне нужно имплементировать matmul(), так как фактически
      !      здесь умножаются 2 матрицы друг на друга, а это не просто
      !      возвести элементы в квадрат :)
      do j = 1, matrix_size
         do k = 1, matrix_size
            do i = 1, matrix_size
               multiplied(i, j) = multiplied(i, j) + matrix(i, k) * matrix(k, j)
            end do
         end do
      end do
      !  2. Вычисляем матрицу.
      ! С -O3 компилятор всё равно привёл бы деление на 2 к инструкции mulss.
      matrix = 1.0_R_ + matrix + 0.5_R_ * multiplied
   end subroutine Special_Case

end module Norm2_Module
