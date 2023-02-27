program exercise_7_25
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, K = 0
   integer, allocatable    :: Matched_strings(:, :)
   real(R_), allocatable   :: A(:,:)
   logical, allocatable    :: Matched(:), Mask(:)
   integer, allocatable    :: Ind(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      ! Т. к. будет вестись сравнение строк, то эффективнее будет размещать
      ! матрицу по строкам -- A(M, N). Тогда любая i-ая строка A(:, i) будет сплошной.
      allocate (A(M, N))
      read (In, *) A
   close (In)
   
   ! Вывод данных.
   open (file=output_file, encoding=E_, position='rewind', newunit=Out)
       write (Out, '('//M//'f6.2)') A
   close (Out)

   Ind = [(i, i = 1, N)]
  

   ! Размещение массивов данных в НАЧАЛЕ работы программы,
   ! а не внутри процедур при КАЖДОМ их вызове.
   ! Размещение массива для записи номеров совпадающих строк.
   ! В первой строке будет храниться число совпавших строк,
   ! хранящихся ниже в столбцах этого массива.
   ! Для примера входного файлв:
   ! | 3 | 4 | 0 | 0 | -- строка массива № 0
   ! | 1 | 2 | 0 | 0 | -- строка массива № 1
   ! | 3 | 4 | 0 | 0 | -- строка массива № 2
   ! | 6 | 7 | 0 | 0 | -- ...
   ! | 0 | 9 | 0 | 0 |
   ! | 0 | 0 | 0 | 0 |
   ! | 0 | 0 | 0 | 0 |
   ! | 0 | 0 | 0 | 0 |
   ! | 0 | 0 | 0 | 0 |
   ! | 0 | 0 | 0 | 0 |
   ! Число строк в массиве -- N+1 для хранения первой строки,
   ! а также всех номеров строк в первом столбце в случае,
   ! если все строки совпадают.
   ! Число столбцов в массиве -- N/2 на случай, если
   ! будет наибольшее число пар совпавших строк.
   allocate (Matched_strings(0:N, N/2))
   ! Маска уже совпадавших прежде строк. 
   allocate (Matched(N), source=.false.)
   ! Маска строк, совпавших с данной.
   allocate (Mask(N))

   !call FindMatchedStrings_Imp(A, Matched, Mask, Matched_strings, K)
   call FindMatchedStrings(A, Ind, Matched, Mask, Matched_strings, K)
   
   ! Вывод данных.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (Out, *)
      do i = 1, K
         write (Out, '('//Matched_strings(0, i)//'i3)') Matched_strings(1:Matched_strings(0, i), i)
      end do
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine FindMatchedStrings_Imp(A, Matched, Mask, Matched_strings, K)
      real(R_), intent(in) :: A(:, :)
      logical, intent(out) :: Matched(:), Mask(:)
      integer, intent(out) :: Matched_strings(0:N, N/2), K

      integer  i, j, l, Matched_num

      ! Работа с i-ой строкой. 
      ! k - номер группы совпавших строк.
      K = 1 
      do i = 1, N
         ! Строка обрабатывается, если прежде ни с кем не совпадала.
         if(.not. Matched(i)) then
            ! i-ая строка уже с собой совпадает.
            Matched_strings(0, k) = 1 
            Mask(i) = .true.
            ! Создание маски для строк, полностью совпадающих с i-ой.
            do j = i+1, N
               Mask(j) = .true.
               do l = 1, M
                  if (A(l, j) /= A(l, i)) then
                     Mask(j) = .false.
                     exit
                  end if
               end do
               if (Mask(j)) &
                  Matched_strings(0, K) = Matched_strings(0, K) + 1
            end do
            ! Записываем номера совпавших строк, если таких больше 1.
            if(Matched_strings(0, K) > 1) then
               Matched_num = 1
               l = i
               do while (l <= N .and. Matched_num <= Matched_strings(0, K))
                  if (Mask(l)) then
                     Matched_strings(Matched_num, K) = l
                     Matched_num = Matched_num + 1
                  end if
                  l = l + 1
               end do
               K = K + 1
               ! Помечаем совпавшие строки, чтобы в будущем не
               ! обрабатывать их повторно.
               do l = i, N
                  Matched(l) = Matched(l) .or. Mask(l)
               end do
            end if
         end if
      end do
      ! Число групп совпавших строк.
      K = K - 1
   end subroutine FindMatchedStrings_Imp
   
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine FindMatchedStrings(A, Ind, Matched, Mask, Matched_strings, K)
      real(R_), intent(in) :: A(:, :)
      integer, intent(in)  :: Ind(:)
      logical, intent(out) :: Matched(:), Mask(:)
      integer, intent(out) :: Matched_strings(0:N, N/2), K

      integer :: i, j

      ! Работа с i-ой строкой. 
      ! k - номер группы совпавших строк.
      k = 1 
      do i=1, N
         ! Строка обрабатывается, если прежде ни с кем не совпадала.
         if(.not. Matched(i)) then
            ! i-ая строка уже с собой совпадает.
            Mask(i) = .true.
            ! Создание маски для строк, полностью совпадающих с i-ой.
            do concurrent (j = i+1:N)
               Mask(j) = All(A(:, j) == A(:, i))
            end do
            ! Записываем номера совпавших строк, если таких больше 1.
            Matched_strings(0, k) = Count(Mask(i:))
            if(Matched_strings(0, k) > 1) then
               Matched_strings(1:, k) = Pack(Ind(i:), Mask(i:))
               k = k + 1
               ! Помечаем совпавшие строки, чтобы в будущем не
               ! обрабатывать их повторно.
               Matched(i:) = Matched(i:) .or. Mask(i:)
            end if
         end if
      end do
      ! Число групп совпавших строк.
      k = k - 1
   end subroutine FindMatchedStrings
end program exercise_7_25
