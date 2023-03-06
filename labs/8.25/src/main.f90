! Баранов К.П., группа 20021.
! Лабораторная работа №8, вариант 8.25.

! Задача: составить процедуру вычисления сферической нормы матрицы:
!  ||z|| = sqrt(sum[i=1 to n](sum[j=1 to n](|z_(ij)|^2)).
! Если норма заданной матрицы X(20, 20) меньше единицы, вычислить матрицу:
!  Y = E + X + 1/2 * X^2, где E - единичная матрица.
!  Найти её норму и напечатать.

! Указания: все процедуры, предлагаемые для разработки,
!  должны быть чистыми – иметь квалификатор pure.
! Они не должны использовать встроенные функции по работе
!  с массивами или сечения.

! Сферическая норма матрицы - это и есть Норма Фробениуса или же Евклидова Норма.
! Её можно вычислить с помощью встроенной функции Norm2().
! По сути мы реализуем эту функцию (изобретаем велосипед).

program MatrixNorm
   use Environment

   ! TODO: перенести подпрограммы и функции
   !  из блока contains в отдельные модули.
   ! use Matrix_Norm_Mod
   ! use Matrix_Norm_IO

   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable   :: matrix(:, :)
   real(R_)                :: z

   call Read_Matrix(input_file, Matrix)
   call Output_Matrix(output_file, Matrix, "rewind")

   write(*,*) Norm2(matrix)

   z = Euclidean_Norm(matrix)
   if (z < 1) then
      ! Y = E + X + 1/2 * X^2.
      call Special_Case(matrix)
      z = Euclidean_Norm(matrix)
   end if

   call Output_Euclidean_Norm(output_file, z, "append")

contains
   subroutine Read_Matrix(input_file, matrix)
      character(*),          intent(in)  :: input_file
      real(R_), allocatable, intent(out) :: matrix(:, :)
      integer(I_)                        :: in, row, matrix_size

      open (file=input_file, newunit=in)
         read (in, *) matrix_size
         allocate (matrix(matrix_size, matrix_size))
         ! Хранение в памяти по столбцам.
         read (in, *) (matrix(row, :), row = 1, matrix_size)
      close (in)
   end subroutine Read_Matrix

   subroutine Output_Matrix(output_file, matrix, position)
      character(*), intent(in) :: output_file, position
      real(R_),     intent(in) :: matrix(:, :)
      integer(I_)              :: row, out, matrix_size

      matrix_size = size(array=matrix, dim=2)

      open (file=output_file, encoding=E_, newunit=out, position=position)
         write (out, "(" // matrix_size // "f6.2)") (matrix(row, :), row = 1, matrix_size)
         write (out, *)
      close (out)
   end subroutine Output_Matrix

   subroutine Output_Euclidean_Norm(output_file, norm, position)
      character(*), intent(in) :: output_file, position
      real(R_),     intent(in) :: norm
      integer(I_)              :: out

      open (file=output_file, encoding=E_, newunit=out, position=position)
         write (out, *) "|| z || = ", norm
      close (out)

   end subroutine Output_Euclidean_Norm

   real(R_) pure function Euclidean_Norm(matrix)
      real(R_), intent(in) :: matrix(:, :)
      integer(I_)          :: column, row, matrix_size

      Euclidean_Norm = 0
      matrix_size = Size(matrix, dim=2)
      ! Вычисляем сумму квадратов элементов каждой строки матрицы.
      do row = 1, matrix_size
         do column = 1, matrix_size
            Euclidean_Norm = Euclidean_Norm + matrix(row, column) ** 2.0_R_
         end do
      end do
      ! Берём от результата квадратный корень.
      Euclidean_Norm = Sqrt(Euclidean_Norm)
   end function Euclidean_Norm

   pure subroutine Special_Case(matrix)
      real(R_), intent(out) :: matrix(:, :)

      ! TODO: подумай, есть ли в этом смысл, если
      !  конкретно в случае сложения матриц ничего не меняется:
      !  они складываются поэлементно.

      ! real(R_)              :: E(Size(matrix, dim=2), Size(matrix, dim=2))
      ! Единичная матрица.
      ! E = 1.0_R_
      
      ! TODO: matrix**2 не то, что мне нужно.
      ! Мне нужно имплементировать matmul(), так как фактически
      !  здесь умножаются 2 матрицы друг на друга, а это не просто
      !  возвести элементы в квадрат :)
      
      ! Вычисляем матрицу Special_Case = E + matrix + 1/2 * matrix^2.
      matrix = 1.0_R_ + matrix + 0.5_R_ * matrix**2

      ! Ниже строчка, если мы всё же будем создавать единичную матрицу,
      !  а не просто складывать поэлементно с единицей.
      ! В таком случае сотри верхнюю и раскомментируй эту.
      ! matrix = E + matrix + 0.5_R_ * matrix**2

   end subroutine Special_Case

end program MatrixNorm
