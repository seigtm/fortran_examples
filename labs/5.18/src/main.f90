! Баранов К.П., группа 20021.
! Лабораторная работа №5, вариант 5.18.

! Задача: дана совокупность A из 10'000 значений.
! Найти среднее арифметическое выборки, состоящей из первых
!  100 значений, удовлетворяющей условию:
!  p <= ai <= q, где:
!  p и q - заданные значения, а
!  ai - значение элемента заданной совокупности.
! Предусмотреть печать необходимого пояснения,
!  если в выборке оказалось менее 100 чисел.

program Sampling
   use Environment
   ! use Sampling_Mod
   ! use Sampling_IO
   implicit none

   character(*), parameter   :: INPUT_FILE  = "../data/input.txt", &
                                OUTPUT_FILE = "output.txt"
   integer(I_), parameter :: p = 1, q = 5
   integer(I_), parameter :: A(*) = (/1, 2, 3, 4, 3, 2, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 1, 2/)
   logical,     parameter :: Pos(*) = p <= A .and. A <= q

   write(*, *) Pos

end program Sampling
