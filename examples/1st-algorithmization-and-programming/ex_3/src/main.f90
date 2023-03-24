program exercise_3
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: P = 1
   real(R_), allocatable   :: B(:)

   open (file=input_file, newunit=In)
      read (In, *) N
   allocate (B(N))

   read (In, *) B
   close (In)

   ! P = ProdImp(B)
   ! Чистая функция в регулярном стиле.
   P = Product(B)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f6.2)") B
      write (Out, *)
      write (Out, "('Product = ', f0.2)") P
   close (Out)

contains

   ! Чистая функция в императивном стиле.
   pure function ProdImp(A) result(Prod)
      real(R_)    Prod, A(:)
      intent(in)  A
      integer     i

      Prod = A(1)
      do i = 2, N
         Prod = Prod * A(i)
      end do
   end function ProdImp
end program exercise_3
