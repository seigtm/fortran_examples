program exercise_5
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   ! N - количество элементов в массиве. M - количество положительных элементов в массиве.
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   integer                 :: S = 0  ! Сумма положительных элементов массива.
   integer, allocatable    :: Z(:)   ! Массив чисел.
   logical, allocatable    :: Pos(:) ! Массив-маска положительных чисел.

   open (file=input_file, newunit=In)
      read (In, *) N  ! Считываем размер массива.
      allocate (Z(N)) ! Выделяем памяти на N элементов массива Z.
      read (In, *) Z  ! Считываем массив.
   close (In)

   !call PositiveImp(Z, S, M)

   ! Размещение данных в НАЧАЛЕ работы программы,
   ! а не внутри подпрограммы при КАЖДОМ её вызове.
   allocate(Pos(N))            ! Выделяем памяти на N элементов массива Pos.
   call Positive(Z, Pos, S, M) ! Вызываем нашу чистую подпрограмму.

   ! Выводим считаннные и вычисленные данные.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      ! Выводим количество положительных элементов в массиве и их сумму.
      write (Out, '(/2(a, T12, "= ", i0/))') 'Pos. items', M, "Sum", S
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine PositiveImp(Z, S, M)
      integer     Z(:), S, M
      intent(in)  Z
      intent(out) S, M
      integer     i

      S = 0
      M = 0
      do i = 1, N
         If (Z(i) > 0) then
            S = S + Z(i)
            M = M + 1
         end if
      end do
   end subroutine PositiveImp

   ! Чистая подпрограмма в императивном стиле.
   pure subroutine Positive(Z, Pos, S, M)
      integer     Z(:), S, M
      logical     Pos(:)
      intent(in)  Z
      intent(out) Pos, S, M

      ! В одну строку создаём в массиве маску:
      ! [ 1 4 -1 -3 7 ] => [ 1 1 0 0 1 ]
      Pos = Z > 0
      ! Вычисляем сумму элементов массива Z по маске Pos.
      S = Sum(Z, Pos)
      ! Подсчитываем количество положительных элементов массива.
      M = Count(Pos)
   end subroutine Positive
end program exercise_5
