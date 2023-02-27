program exercise_7_48
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: A(:, :), B(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N, N))
      read (In, *) (A(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)
  
   !B = GetUpperMatrix_Imp(A)
   B = GetUpperMatrix(A)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, '('//Size(B)//'f6.2)') B 
   close (Out)

contains
   ! Чистая процедура в императивном стиле.
   pure function GetUpperMatrix_Imp(A) result(B)
      real(R_), intent(in) :: A(:, :)
      real(R_)             :: B((Size(A) - UBound(A, 1)) / 2) ! == (N*N - N) / 2
     
      integer i, j, N
      N = UBound(A, 1)

      ! 1. Задача попроще - формирование вектора B из ВСЕХ элементов матрицы A, чтением по строкам.
      ! B = [A(1, 1), A(1, 2), ..., A(2, 1), A(2, 2), ...]
      ! Сведение двух индексов i и j к одному k, двигаясь по строкам:
      ! k   | i, j | k(i, j)
      ! ====================
      ! 1   | 1, 1 | j
      ! 2   | 1, 2 | j
      ! ... | ... (N раз)
      ! N+1 | 2, 1 | j + N
      ! N+2 | 2, 2 | j + N
      ! ... | ... (N раз)
      ! N*N | N, N | j + N*(i-1)
      ! 
      ! Поэтому k(i, j) = j + N*(i-1)
      ! 
      ! 2. Исходная задача - формирование вектора B из наддиагональных элементов матрицы A, чтением по строкам.
      ! Можно использовать предыдущую формулу для k, но вычитать из неё ко-во не вошедших элементов S(i)
      ! (из 1-ой строки не вошёл 1 элемент, из 2-ой -- 2 и т. д.):
      ! S(i) = 1 + 2 + ... + i = (1+i)*i/2, поэтому k(i, j) = j + N*(i-1) - (1+i)*i/2 ! делим на 2 в последнюю очередь,
      ! чтобы точно разделилось без остатка.
      do i = 1, N-1
         do j = i+1, N
            B(j + N*(i-1) - (1+i)*i/2) = A(i, j)
         end do
      end do
   end function GetUpperMatrix_Imp

   ! Чистая процедура в регулярном стиле.
   pure function GetUpperMatrix(A) result(B)
      real(R_), intent(in) :: A(:, :)
      real(R_)             :: B((Size(A) - UBound(A, 1)) / 2) ! == (N*N - N) / 2
     
      integer i, j, N
      N = UBound(A, 1)

      ! См. комментарии выше.
      do concurrent (i = 1:N-1, j = 1:N, j>i) ! ВЕКТОРИЗАЦИЯ
         B(j + N*(i-1) - (1+i)*i/2) = A(i, j)
      end do
      ! или через присваивание, но БЕЗ векторизации
      !B = [(A(i, i+1:), i = 1, N-1)]
   end function GetUpperMatrix
end program exercise_7_48
