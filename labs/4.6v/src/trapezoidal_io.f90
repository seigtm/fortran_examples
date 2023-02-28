module Trapezoidal_IO
   use Environment
   implicit none

contains
   subroutine Read_Integral_Values(input_file, a, b, h)
      character(*), intent(in)  :: input_file
      real(R_),     intent(out) :: a, b, h
      integer(I_)               :: In

      open (file=input_file, newunit=In)
         read (In, *) a, b, h
      close (In)
   end subroutine Read_Integral_Values

   subroutine Output_Integral_Values(output_file, a, b, h)
      character(*), intent(in) :: output_file
      real(R_),     intent(in) :: a, b, h
      character(*), parameter  :: FMT = '(3(a, T4, "= ", f0.5/))'
      integer(I_)              :: Out

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, FMT) "a", a, "b", b, "h", h
      close (Out)
   end subroutine Output_Integral_Values

   subroutine Output_Integral(output_file, i)
      character(*), intent(in) :: output_file
      real(R_),     intent(in) :: i
      character(*), parameter  :: FMT = '(a, T4, "= ", f0.5)'
      integer(I_)              :: Out

      open (file=output_file, encoding=E_, newunit=Out, position="append")
         write (Out, FMT) "i", i
      close (Out)
   end subroutine Output_Integral

end module Trapezoidal_IO
