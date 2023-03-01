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
   use IEEE_Arithmetic  ! Для ieee_quiet_nan.
   implicit none

   character(*), parameter  :: INPUT_FILE  = "../data/input.txt",  &
                               OUTPUT_FILE = "output.txt",         &
                               fmt_int     = '(a, T7, "= ", i0)',  &
                               fmt_real    = '(a, T7, "= ", f0.2)'
   ! p и q - заданные значения, по которым будет осуществляться выборка.
   ! mean_val - среднее арифметическое.
   real(R_)                 :: p, q, mean_val
   ! N - количество первых значений выборки, по которым будет находиться среднее арифметическое.
   integer(I_)              :: In, Out, sizeA, N
   ! Исходный массив.
   real(R_), allocatable :: A(:)

   open(file=INPUT_FILE, newunit=In)
      read(In, *) sizeA, p, q, N
      allocate(A(sizeA))
      read(In, *) A
   close(In)

   open(file=output_file, encoding=E_, newunit=out)
      write(Out, fmt_int)  "sizeA", sizeA
      write(Out, fmt_real) "p", p
      write(Out, fmt_real) "q", q
      write(Out, fmt_int)  "N", N
      write(Out, '(a, T7, "= (", ' // sizeA // 'f6.2, " )")') "A", A
      write(Out, *)
   close(In)

   mean_val = Mean(A, p, q, N)

   open(file=OUTPUT_FILE, encoding=E_, newunit=Out, position="append")
      ! Если в выборке менее N чисел.
      if(IEEE_Is_NaN(mean_val)) then
         write(Out, *) "В выборке менее "// N //" чисел."
      else
         write(Out, fmt_real) "mean", mean_val
      end if
   close(Out)

contains
   real(R_) pure function Mean(A, p, q, N)
      real(R_),    intent(in) :: A(:)
      real(R_),    intent(in) :: p, q
      integer(I_), intent(in) :: N
      ! Выборка элементов массива A, удовлетворяющая условию p <= ai <= q.
      real(R_), allocatable   :: GoodA(:)

      ! Выборка элементов массива A, удовлетворяющая условию p <= ai <= q.
      GoodA = pack(A, p <= A .and. A <= q)

      ! Если в выборке менее N чисел.
      if(size(GoodA) < N) then
         ! Возвращаем NaN.
         Mean = IEEE_Value(Mean, IEEE_Quiet_NaN)
         return
      endif

      ! Вычисляем среднее арифметическое.
      Mean = sum(GoodA(1:N)) / real(N, R_)
   end function Mean

end program Sampling
