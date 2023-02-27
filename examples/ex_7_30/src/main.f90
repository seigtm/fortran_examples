program exercise_7_30
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: A(:, :)
   logical, allocatable    :: NotNullRows(:), NotNullCols(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (A(N, M))
      read (In, *) (A(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (A(i, :), i = 1, N)
   close (Out)
   
   ! Размещение массивов данных В НАЧАЛЕ программы,
   ! а не внутри подпрограммы при КАЖДОМ её вызове.
   allocate (NotNullRows(N), source=.false.)
   allocate (NotNullCols(M), source=.false.)
   
   !call PackMatrix_Imp(A, N, M, NotNullRows, NotNullCols)
   call PackMatrix(A, N, M, NotNullRows, NotNullCols)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(/'//N//'('//M//'f6.2/))') (A(i, 1:M), i = 1, N)
   close (Out)

contains

! Чистая подпрограмма в императивном стиле.
pure subroutine PackMatrix_Imp(A, N, M, NotNullRows, NotNullCols)
   real(R_), intent(inout) :: A(:, :)
   integer, intent(inout)  :: N, M
   logical, intent(out)    :: NotNullRows(:), NotNullCols(:)

   integer i, j, CurrentNotNull, Current, FirstNull

   ! Позиции ненулевых строк и их число.
   N = 0
   do i = 1, Size(NotNullRows)
      do j = 1, Size(NotNullCols)
         if (A(i, j) /= 0) then
            NotNullRows(i) = .true.
            N = N + 1
            exit
         end if
      end do
   end do
   
   ! Позиции ненулевых столбцов и их число.
   M = 0
   do j = 1, Size(NotNullCols)
      do i = 1, Size(NotNullCols)
         if (A(i, j) /= 0) then
            NotNullCols(j) = .true.
            M = M + 1
            exit
         end if
      end do
   end do
   
   ! Упаковка каждого НЕнулевого столбца в матрице.
   if (N < Size(NotNullRows)) then
      ! Нахождение номера первой нулевой строки.
      do FirstNull = 1, Size(NotNullRows)
         if (.not. NotNullRows(FirstNull)) &
            exit
      end do
   
      ! Упаковка каждого НЕнулевого столбца в матрице.
      ! concurrent
      do concurrent (j = 1:Size(NotNullCols))
         if (NotNullCols(j)) &
            call PackColumn_Imp(A(:, j), NotNullRows, FirstNull, N)
      end do
   end if

   ! Сдвиг всех НЕнулевых столбцов в начало матрицы.

   if (M < Size(NotNullCols)) then
      ! Нахождение номера первого нулевого столбца.
      do FirstNull = 1, Size(NotNullCols)
         if (.not. NotNullCols(FirstNull)) &
            exit
      end do
  
      ! Сдвиг НЕнулевых столбцов в начало матрицы.
      CurrentNotNull = FirstNull
      do Current = FirstNull, M
         ! Нахождение следующего НЕнулевого столбца.
         ! Такой обязательно найдётся,
         ! раз Current <= M (число ненулевых столбцов).
         do
            CurrentNotNull = CurrentNotNull + 1
            if (NotNullCols(CurrentNotNull)) &
               exit
         end do

         ! Запись очередного НЕнулевого столбца на место текущего нулевого столбца.
         do i = 1, N
            A(i, Current) = A(i, CurrentNotNull)
         end do
      end do
   end if
end subroutine PackMatrix_Imp

! Чистая подпрограмма по упаковке j-ого столбца.
pure subroutine PackColumn_Imp(Column, NotNullRows, FirstNull, N)
   real(R_), intent(inout) :: Column(:)
   integer, intent(in)     :: FirstNull, N
   logical, intent(in)     :: NotNullRows(:)

   integer CurrentNotNull, Current

   CurrentNotNull = FirstNull
   do Current = FirstNull, N
      ! Нахождение следующего НЕнулевого элемента в столбце.
      ! Такой обязательно найдётся,
      ! раз Current <= N (число ненулевых строк).
      do
         CurrentNotNull = CurrentNotNull + 1
         if (NotNullRows(CurrentNotNull)) &
            exit
      end do
      ! Запись очередного НЕнулевого элемента столбца
      ! на место текущего нулевого элемента столбца.
      Column(Current) = Column(CurrentNotNull)
   end do
end subroutine PackColumn_Imp

! Чистая подпрограмма в регулярном стиле.
pure subroutine PackMatrix(A, N, M, NotNullRows, NotNullCols)
   real(R_), intent(inout) :: A(:, :)
   integer, intent(inout)  :: N, M
   logical, intent(out)    :: NotNullRows(:), NotNullCols(:)

   integer j, CurrentNotNull, Current, FirstNull

   ! Позиции ненулевых строк. 
   NotNullRows = Any(A /= 0, dim=2)  ! ВЕКТОРИЗАЦИЯ. 
   ! Позиции ненулевых столбцов.
   NotNullCols = Any(A /= 0, dim=1)  ! ВЕКТОРИЗАЦИЯ. 
   
   ! Обновляем число НЕнулевых строк и столбцов.
   N = Count(NotNullRows)
   M = Count(NotNullCols)
   
   ! Упаковка каждого НЕнулевого столбца в матрице.
   if (N < Size(NotNullRows)) then
      do concurrent (j = 1:Size(NotNullCols), NotNullCols(j))
         A(:, j) = Pack(A(:, j), NotNullRows)
      end do
   end if 

   ! Сдвиг всех ненулевых столбцов в начало матрицы.

   if (M < Size(NotNullCols)) then
      ! Нахождение первого нулевого столбца.
      do FirstNull = 1, Size(NotNullCols)
         if (.not. NotNullCols(FirstNull)) &
            exit
      end do
      
      ! Сдвиг НЕнулевых столбцов в начало матрицы.
      CurrentNotNull = FirstNull
      do Current = FirstNull, M
         ! Нахождение следующего НЕнулевого столбца.
         ! Такой обязательно найдётся,
         ! раз Current <= M (число ненулевых столбцов).
         do
            CurrentNotNull = CurrentNotNull + 1
            if (NotNullCols(CurrentNotNull)) &
               exit
         end do

         ! Запись очередного НЕнулевого столбца на место текущего нулевого столбца.
         A(1:N, Current) = A(1:N, CurrentNotNull)
      end do
   end if
   
   ! Второй способ. Упаковка по маске -- неэффективный, т. к.
   ! ! при упаковке векториация не задействуется.
   ! allocate (NotNullRows(N, 1), NotNullCols(1, M))
   ! ! Позиции ненулевых строк (N, 1). 
   ! NotNullRows(:, 1) = Any(A /= 0, dim=2)  ! ВЕКТОРИЗАЦИЯ. 
   ! ! Позиции ненулевых столбцов (1, M).
   ! NotNullCols(1, :) = Any(A /= 0, dim=1)  ! ВЕКТОРИЗАЦИЯ. 
   ! N = Count(NotNullRows)
   ! M = Count(NotNullCols)
   ! A = Reshape( Pack(A, MatMul(NotNullRows, NotNullCols)), [N, M] )
end subroutine PackMatrix
end program exercise_7_30
