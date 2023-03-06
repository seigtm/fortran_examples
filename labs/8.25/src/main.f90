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
   use IEEE_Arithmetic  ! Для NaN.
   use Norm2_Module
   use Norm2_IO

   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable   :: matrix(:, :)
   real(R_)                :: z

   call Read_Matrix(input_file, matrix)
   call Output_Matrix(output_file, matrix, "rewind")
   ! if (.not. Is_Square_Matrix(matrix)) then
   !    call Output_Error(output_file, "append")
   ! else
   !    ! Everything else...
   ! end if

   ! FIXME: remove, it's here for debug purposes.
   write(*,*) Norm2(matrix) 

   z = Euclidean_Norm(matrix)

   if (IEEE_Is_NaN(z)) then
      ! call Output_Error(output_file, "append")  ! TODO: implement or get rid of.
   else if (z < 1) then
      ! Y = E + X + 1/2 * X^2.
      call Special_Case(matrix)
      z = Euclidean_Norm(matrix)
      call Output_Euclidean_Norm(output_file, z, "append")  ! FIXME: stupid.
   else
      call Output_Euclidean_Norm(output_file, z, "append")  ! FIXME: stupid.
   end if
end program MatrixNorm
