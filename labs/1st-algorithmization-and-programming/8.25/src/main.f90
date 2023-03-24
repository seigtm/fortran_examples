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
   use Norm2_Module
   use Norm2_IO

   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable   :: matrix(:, :)
   real(R_)                :: z

   call Read_Matrix(input_file, matrix)
   call Output_Matrix(output_file, matrix, "rewind")

   z = Euclidean_Norm(matrix)
   if (z < 1) then
      ! Y = E + X + 1/2 * X^2.
      call Special_Case(matrix)
      z = Euclidean_Norm(matrix)
   endif

   call Output_Euclidean_Norm(output_file, z, "append")
end program MatrixNorm
