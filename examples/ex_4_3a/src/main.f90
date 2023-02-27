program exercise_4_3a
   use Environment
   
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: a = 0, b = 0, h = 0, I = 0
   real(R_), allocatable   :: X(:)

   open (file=input_file, newunit=In)
      read (In, *) a, b, h
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", h
   close (Out)
  
   N = Int((b - a) / h + .5_R_)

   !I = IntegrateImp(a, h, N)
   ! Размещение данных в НАЧАЛЕ работы программе,
   ! а не при КАЖДОМ вызове процедуры.
   allocate(X(N))
   call Integrate(a, h, X, I)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T4, "= ", f0.4)') "I", I
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   pure function IntegrateImp(a, h, N) result(I)
      real(R_)    I, a, h
      integer     N
      intent(in)  a, h, N
      real(R_)    x
      integer     j
   
      I = 0
      x = a
      do j = 1, N
         I = I + .8_R_*x*Exp(-(x*x + .5_R_))
         x = x + h
      end do
      I = I * h
   end function IntegrateImp
   
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine Integrate(a, h, X, I)
      real(R_)    a, h, X(:), I
      intent(in)  a, h
      intent(out) X, I
      integer     j
  
      X = [(a + (j-1)*h, j = 1, Size(X))]
      X = .8_R_* X * Exp(-(X*X + .5_R_))
      I = Sum(X) * h
   end subroutine Integrate
end program exercise_4_3a
