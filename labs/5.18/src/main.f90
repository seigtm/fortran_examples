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
   ! TODO: перенеси обработку и ввод/вывод в отдельные модули.
   !       ввод/вывод осуществляй из файла, как обычно.
   ! use Sampling_Mod
   ! use Sampling_IO
   implicit none

   character(*), parameter  :: INPUT_FILE  = "../data/input.txt", OUTPUT_FILE = "output.txt"
   ! p и q - заданные значения, по которым будет осуществляться выборка.
   ! N - количество первых значений выборки, по которым будет находиться среднее арифметическое.
   integer(I_), parameter   :: p = 3, q = 5, N = 6
   ! Исходный массив.
   integer(I_), allocatable :: A(:)
   ! Выборка элементов массива A, удовлетворяющая условию p <= ai <= q.
   integer(I_), allocatable :: GoodA(:)
   integer(I_)              :: In, Out, sizeA

   open(file=INPUT_FILE, newunit=In)
      read(In, *) sizeA
      allocate(A(sizeA))
      read(In, *) A
   close(In)

   GoodA = pack(A, p <= A .and. A <= q)

   open(file=OUTPUT_FILE, encoding=E_, newunit=Out, position="rewind")
      ! Если в выборке менее N чисел.
      if(size(GoodA) < N) then
         write(Out, *) "В выборке менее "// N //" чисел."
      else
         write(Out, *) sum(GoodA(1:N)) / real(N, R_)  ! Вычисляем среднее арифметическое.
      end if
   close(Out)

end program Sampling
