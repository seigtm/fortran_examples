! Баранов К.П., группа 20021.
! Лабораторная работа №2, вариант 2.21.

! Задача: вычислить значение u по вещественным значениям a, b, c и x.
! u = int(dx / sqrt(a + bx + cx^2)) = {
!  1 / (sqrt(c)) * ln(|2sqrt(cR) + 2cx + b|)        при c > 0;
! -1 / (sqrt(c)) * asin((2cx + b) / (sqrt(-Delta))) при c < 0              и Delta < 0;
!  1 / (sqrt(c)) * ln(2cx + b)                      при c > 0, 2cx + b > 0 и Delta = 0;
! Здесь Delta = 4ac - b^2, R = a + bx + cx^2.

! Integration (англ.) - взять интеграл.
program Integration
   use Environment
   use Integral_calculate
   use Integral_IO

   implicit none
   ! Входные данные.
   real(R_) :: A = .0, B = .0, C = .0, X = .0
   ! Пути к файлам и формат ввода/вывода.
   character(*), parameter :: INPUT_FILE  = "../data/input.txt", &
                              OUTPUT_FILE = "output.txt"

   call Read_Integral_Values(INPUT_FILE, A, B, C, X)
   call Output_Integral_Values(OUTPUT_FILE, A, B, C, X)
   call Output_Integral_Result(OUTPUT_FILE, Integral(A, B, C, X))
end program Integration
