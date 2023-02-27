program exercise_7_5a
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:), Negatives(:)
   logical, allocatable    :: Neg(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)
 
   ! Проверять знак числа при сортировке очень дорого,
   ! поэтому в отдельный массив помещаются отрицательные числа и сортируются.
   
   ! Размещаем массивы в НАЧАЛЕ работы программы,
   ! а не внутри процедур при КАЖДОМ их вызове.
   ! Формируем маску для отрицательных эелементов.
   allocate(Neg(M), source = .false.)
   allocate(Negatives(M))
   
   !call SortNegativesImp(A, Neg, Negatives)
   call SortNegatives(A, Neg, Negatives)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//M//"f6.2)") A
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine SortNegativesImp(A, Neg, Negatives)
      real(R_), intent(inout) :: A(:), Negatives(:)
      logical, intent(inout)  :: Neg(:)

      real(R_) :: tmp
      integer  :: N, i, MinInd, k

      ! Помещаем отрицательные элементы в отдельный массив.
      k = 1
      do i = 1, M
         if (A(i) < 0) then
            Neg(i)         = .true.
            Negatives(k)   = A(i)
            k = k + 1
         end if
      end do

      ! Сортируем массив из отрицательных элементов методом выбора:
      ! 1. Находим в неотсортированной части массива индекс минимального элемента.
      ! 2. Меняем его с первым элементом в неотсортированной части
      !    (если это не один и тот же элемент).
      ! 3. Отсортированная часть увеличилась. Повторяем пп. 1-2.
      N = k - 1
      do i = 1, N-1
         MinInd = i
         do k = i+1, N
            if (Negatives(k) < Negatives(MinInd)) &
               MinInd = k
         end do
         if (i /= MinInd) then
            tmp               = Negatives(i)
            Negatives(i)      = Negatives(MinInd)
            Negatives(MinInd) = tmp
         end if
      end do
      ! Распаковываем отрицательные элементы на прежние места.
      k = 1
      do i = 1, Size(A)
         if (Neg(i)) then
            A(i) = Negatives(k)
            k = k + 1
         end if
      end do
   end subroutine SortNegativesImp

   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine SortNegatives(A, Neg, Negatives)
      real(R_), intent(inout) :: A(:), Negatives(:)
      logical, intent(inout)  :: Neg(:)

      real(R_) :: tmp
      integer  :: i, MinInd

      Neg = A < 0
      ! Помещаем отрицательные элементы в отдельный массив.
      Negatives = Pack(A, Neg)
      ! Сортируем массив из отрицательных элементов методом выбора:
      ! 1. Находим в неотсортированной части массива индекс минимального элемента.
      ! 2. Меняем его с первым элементом в неотсортированной части
      !    (если это не один и тот же элемент).
      ! 3. Отсортированная часть увеличилась. Повторяем пп. 1-2.
      do i = 1, Size(Neg)-1
         MinInd = MinLoc(Negatives(i:), 1) + i-1
         if (i /= MinInd) then
            tmp               = Negatives(i)
            Negatives(i)      = Negatives(MinInd)
            Negatives(MinInd) = tmp
         end if
      end do
      ! Распаковываем отрицательные элементы на прежние места.
      A = Unpack(Negatives, Neg, A)
   end subroutine SortNegatives
end program exercise_7_5a
