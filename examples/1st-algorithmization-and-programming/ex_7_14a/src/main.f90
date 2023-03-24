! Сумма элементов квадратной матрицы выше главной диагонали.

program exercise_7_14a
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:, :), Sums(:)
   real(R_)                :: s = 0

   open (file=input_file, newunit=In)
      read (In, *) N                   ! Считываем размер квадратной матрицы (NxN).
      allocate (Z(N, N))               ! Выделяем память для квадратной матрицы.
      read (In, *) (Z(i, :), i = 1, N) ! Считываем квадратную матрицу. Хранение в памяти по столбцам.
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      ! Выводим квадратную матрицу в файл.
      write (Out, '('//N//'f6.2)') (Z(i, :), i = 1, N)
   close (Out)

   !s = UpperSumImp(Z)

   ! Размещение массивов в НАЧАЛЕ работы программы,
   ! а не внутри процедур при КАЖДОМ их вызове.
   ! Вылеляем памяти для N-1 элементов массива Sums
   !  и инициализируем их нулями.
   allocate(Sums(N-1), source=0._R_)
   call UpperSum(Z, Sums, s)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      ! Выводим сумму элементов выше главной диагонали матрицы.
      write (Out, '(a, T5, "= ", f9.6)') "Sum", s
   close (Out)

contains
   ! Чистая функция в исперативном стиле.
   pure function UpperSumImp(Z) result(s)
      real(R_)    s, Z(:, :)
      intent(in)  Z
      integer     i, j

      ! Проход по столбцам -- как расположена матрица в памяти.
      s = 0
      do j = 2, N
         do i = 1, j-1
            s = s + Z(i, j)
         end do
      end do

      ! Так лучше не делать -- вводятся избыточные итерации и проверки:
      !do j = 1, N
      !   do i = 1, N
      !      if (j > i) then
      !         s = s + Z(i, j)
      !      end if
      !   end do
      !end do
   end function UpperSumImp

   ! Чистая функция в регулярном стиле.
   pure subroutine UpperSum(Z, Sums, s)
      real(R_)       s, Z(:, :), Sums(:)
      intent(in)     Z
      intent(inout)  Sums, s
      integer        j

      ! Суммирование НЕПОЛНЫХ столбцов, расположенных выше главной диагонали
      ! для возможности векторизации.
      do concurrent (j = 2:N)
         !do concurrent (integer :: j = 2:N)
         ! Типичный вопрос из практикума:
         !  После этой строки что будет храниться по смыслу в каждом элементе массива Sums?
         Sums(1:j-1) = Sums(1:j-1) + Z(1:j-1, j) ! ВЕКТОРИЗАЦИЯ.
         ! Внимание, матрица представлена ниже так, как она находится в файле.
         ! В памяти храним по столбцам.
         ! 1  [2] [3] [4] -> Sums:
         ! 5   6  [7] [8] -> [2+3+4=9, 7+8=15, 2],
         ! 9   0   1  [2] -> а если быть точнее (по итерациям):
         ! 3   4   5   6  -> [2] -> [2+3, 7] -> [2+3+4, 7+8, 2].
         ! Если представлять как в памяти, то на самом деле это выглядит так:
         !  1   5   9  3
         ! [2]  6   0  4   -> [  2          ] Z(1:2-1, 2) -> Z( 1,  2) ->  1 столбец,  2 строка.
         ! [3] [7]  1  5   -> [ 2+3,   7    ] Z(1:3-1, 3) -> Z(1:2, 3) -> 1-2 столбцы, 3 строка.
         ! [4] [8] [2] 6   -> [2+3+4, 7+8, 2] Z(1:4-1, 4) -> Z(1:3, 4) -> 1-3 столбцы, 4 строка.
         ! Ответ на типичный вопрос: в каждом элементе массива Sums будет храниться
         !  сумма неполного столбца, расположенного выше главной диагонали: [2+3+4, 7+8, 2].
      end do
      s = Sum(Sums)

      ! Так лучше не делать -- нерегулярное обращение к элементам матрицы в Sum.
      ! ! Формирование матрицы-маски, где верхняя треугольная матрица истина.
      ! ! allocate (Upper(N, N))
      ! ! do concurrent (i = 1:N, j = 1:N, j > i)
      ! !    Upper(i, j) = .true.
      ! ! end do
      ! ! ! Нахождение суммы элементов по сформированной маске.
      ! ! s = Sum(Z, Upper)
   end subroutine UpperSum
end program exercise_7_14a
