program exercise_4
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_)                :: x1 = 0, x2 = 0, h = 0 ! x начальное, x конечное и шаг.
   real(R_), allocatable   :: X(:), F(:), Sin_X(:) 
   
   open (file=input_file, newunit=In)
      read (In, *) x1, x2, h
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "h", h
   close (Out)
   
   ! Вычислили размеры для массивов исходя из значений x начального, x конечного и шага.
   N = Int((x2-x1) / h + .5_R_)+ 1 ! (0.2776 - 0.1552) / 0.0002 + 0.5 + 1 = 613.5 => Int(614).
   ! Размещение данных В НАЧАЛЕ работы программы,
   ! а не внутри процедуры при КАЖДОМ её вызове.
   allocate (X(N), F(N), Sin_x(N))  ! Sin_x(N) - не используется.
  
   !call TabFImp(x1, h, X, F)
   call TabF(x1, h, X, F)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("  X   |   f")')
      write (Out, '(f0.4, T7, "| ", f0.4)') (X(i), F(i), i = 1, N)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   pure subroutine TabFImp(x1, h, X, F)
      real(R_)    x1, h, X(:), F(:)
      intent(in)  x1, h
      intent(out) X, F
      integer     N, i
      
      N = Size(X)
      X(1) = x1
      do i = 1, N-1
         F(i) = 1 + 1 / Sin(X(i))
         X(i+1) = X(i) + h
      end do
      ! Функция закодирована по-другому, чтобы дважды не вычислять Sin(x).
      F(N) = 1 + 1 / Sin(X(N))
   end subroutine TabFImp
   
   ! Чистая функция в регулярном стиле.
   pure subroutine TabF(x1, h, X, F)
      real(R_)    x1, h, X(:), F(:)
      intent(in)  x1, h
      intent(out) X, F
      integer     i
      ! Одной строчкой с помощью неявного цикла
      !  заполняем массив X-ов от x начального до x конечного с шагом h.
      X = [(x1 + h*(i-1), i = 1, Size(X))]
      ! А теперь в одну строчку заполняем массив F.
      ! Функция закодирована по-другому, чтобы дважды не вычислять Sin(x).
      F = 1 + 1 / Sin(X)
   end subroutine TabF
end program exercise_4
