program exercise_7_21
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:, :)
   real(R_)                :: max_neg = 0, min_pos = 0
   integer, allocatable    :: Indexes(:, :), Ind_max_neg(:, :), Ind_min_pos(:, :)
   logical, allocatable    :: Mask(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)

   ! Размещение массивов в НАЧАЛЕ работы программы,
   ! а не внутри подпрограмм при КАЖДОМ их вызове.
   allocate (Indexes(N*M, 2))
   allocate (Mask(N*M), source=.false.)
  
   !call MaxNegAndMinPos_Imp(C, max_neg, min_pos, Mask, Indexes, Ind_max_neg, Ind_min_pos)
   call MaxNegAndMinPos(C, max_neg, min_pos, Mask, Indexes, Ind_max_neg, Ind_min_pos)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, f6.2)') "Наибольшие из отрицательных:", max_neg
      write (Out, '(2i3)') (Ind_max_neg(i, :), i = 1, UBound(Ind_max_neg, 1))
      write (Out, '(a, f6.2)') "Наименьшие из положительных:", min_pos
      write (Out, '(2i3)') (Ind_min_pos(i, :), i = 1, UBound(Ind_min_pos, 1))
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine MaxNegAndMinPos_Imp(C, max_neg, min_pos, Mask, Indexes, Ind_max_neg, Ind_min_pos)
      real(R_), intent(in)    :: C(:, :)
      real(R_), intent(out)   :: max_neg, min_pos
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) :: Ind_max_neg(:, :), Ind_min_pos(:, :)
      logical, intent(out)    :: Mask(:)

      integer N_max_neg, N_min_pos, i, j

      ! Формируем двумерный массив индексов:
      ! | 1 | 1 |
      ! | 2 | 1 |
      ! ...
      ! | N | 1 |
      ! ...
      ! | 1 | 2 |
      ! | 2 | 2 |
      ! ...
      ! | N | 2 |
      ! ...
      ! | 1 | M |
      ! | 2 | M |
      ! ...
      ! | N | M |
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      ! Поиск наибольшего из отрицательных.
      max_neg = -Huge(C)
      do j = 1, M
         do i = 1, N
            if (C(i, j) < 0 .and. max_neg < C(i, j)) &
               max_neg = C(i, j)
         end do
      end do
 
      ! Получаем маску для элементов, равных наибольшему из отрицательных.
      N_max_neg = 0
      do concurrent (i = 1:N, j = 1:M, C(i, j) == max_neg)
         Mask(i+(j-1)*N) = .true.
         N_max_neg = N_max_neg + 1
      end do
      
      ! Формируем массив индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_max_neg(N_max_neg, 2))
      
      ! Упаковка массива индексов.
      j = 1
      do i = 1, N*M
         if (Mask(i)) then
            Ind_max_neg(j, :) = Indexes(i, :)
            j = j + 1
         end if
      end do
      
      ! Поиск наименьшего из положительных.
      min_pos = Huge(C)
      do j = 1, M
         do i = 1, N
            if (C(i, j) > 0 .and. min_pos > C(i, j)) &
               min_pos = C(i, j)
         end do
      end do
 
      ! Получаем маску для элементов, равных наименьшему из положительных.
      do concurrent (i = 1:N*M)
         Mask(i) = .false.
      end do
      N_min_pos = 0
      do concurrent (i = 1:N, J = 1:M, C(i, j) == min_pos)
         Mask(i+(j-1)*N) = .true.
         N_min_pos = N_min_pos + 1
      end do

      ! Формируем массив индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_min_pos(N_min_pos, 2))
      ! Упаковка массива индексов.
      j = 1
      do i = 1, N*M
         if (Mask(i)) then
            Ind_min_pos(j, :) = Indexes(i, :)
            j = j + 1
         end if
      end do
   end subroutine MaxNegAndMinPos_Imp

    ! Чистая подпрограмма в регулярном стиле.
   pure subroutine MaxNegAndMinPos(C, max_neg, min_pos, Mask, Indexes, Ind_max_neg, Ind_min_pos)
      real(R_), intent(in)    :: C(:, :)
      real(R_), intent(out)   :: max_neg, min_pos
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) :: Ind_max_neg(:, :), Ind_min_pos(:, :)
      logical, intent(out)    :: Mask(:)

      integer N_max_neg, N_min_pos, i, j

      ! Формируем двумерный массив индексов:
      ! | 1 | 1 |
      ! | 2 | 1 |
      ! ...
      ! | N | 1 |
      ! ...
      ! | 1 | 2 |
      ! | 2 | 2 |
      ! ...
      ! | N | 2 |
      ! ...
      ! | 1 | M |
      ! | 2 | M |
      ! ...
      ! | N | M |
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      ! Поиск наибольшего из отрицательных.
      max_neg = MaxVal(C, C < 0)
      ! Получаем маску для элементов, равных наибольшему из отрицательных.
      Mask        = [C == max_neg]
      N_max_neg   = Count(Mask)

      ! Формируем массив индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_max_neg(N_max_neg, 2))
      ! Упаковка массива индексов по каждой из координат.
      Ind_max_neg(:, 1) = Pack(Indexes(:, 1), Mask)
      Ind_max_neg(:, 2) = Pack(Indexes(:, 2), Mask)
      
      ! Второй способ: работа сразу с двумя столбцами (требующий размещения в памяти).
      ! 1. Добавляем к маске второй ТАКОЙ ЖЕ столбец:
      ! Two_dim_mask = Spread(Mask_max_neg, 2, 2)).
      ! 2. Запаковываем сразу весь массив индексов по столбцам:
      ! Ind_by_col   = Pack(Ind, Two_dim_mask).
      ! 3. Преобразуем полученный массив в два столбца:
      ! Ind_max_neg  = Reshape(Ind_by_col, [N_max_neg, 2])

      ! Третий способ: работа сразу с двумя столбцами одним оператором:
      !Ind_max_neg = Reshape( Pack(Ind, Spread(Mask_max_neg, 2, 2)), [N_max_neg, 2])
 
      ! Поиск наименьшего из положительных.
      min_pos = MinVal(C, C > 0)
      ! Получаем маску для элементов, равных наименьшему из положительных.
      Mask        = [C == min_pos]
      N_min_pos   = Count(Mask)

      ! Формируем массив индексов, удовлетворяющих заданной маске:

      ! Размещение массивов индексов.
      allocate(Ind_min_pos(N_min_pos, 2))
      ! Упаковка массива индексов по каждой из координат.
      Ind_min_pos(:, 1) = Pack(Indexes(:, 1), Mask)
      Ind_min_pos(:, 2) = Pack(Indexes(:, 2), Mask)
   end subroutine MaxNegAndMinPos
end program exercise_7_21
