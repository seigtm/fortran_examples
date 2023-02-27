program exercise_7_14a
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:, :), Sums(:)
   real(R_)                :: s = 0

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (Z(i, :), i = 1, N)
   close (Out)

   !s = UpperSumImp(Z)

   ! Размещение массивов в НАЧАЛЕ работы программы,
   ! а не внутри процедур при КАЖДОМ их вызове.
   allocate(Sums(N-1), source=0._R_)
   call UpperSum(Z, Sums, s)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
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
         Sums(1:j-1) = Sums(1:j-1) + Z(1:j-1, j) ! ВЕКТОРИЗЯЦИЯ.
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
