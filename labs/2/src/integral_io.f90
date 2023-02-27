module Integral_IO
   use Environment
   implicit none

   ! Формат ввода/вывода.
   character(*), parameter :: FMT = "(a, ' = (', f0.3, ')')"

contains
   ! Чтение параметров для интеграла.
   subroutine Read_Integral_Values(input_file, a, b, c, x)
      character(*), intent(in)  :: input_file
      real(R_),     intent(out) :: a, b, c, x
      integer                   :: In = 0

      open (file=input_file, newunit=In)
         read (In, *) a, b, c, x
      close (In)
   end subroutine Read_Integral_Values

   ! Вывод параметров интеграла.
   subroutine Output_Integral_Values(output_file, A, B, C, X)
      character(*), intent(in) :: output_file
      real(R_),     intent(in) :: A, B, C, X
      integer                  :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, FMT) "a", a
         write (Out, FMT) "b", b
         write (Out, FMT) "c", c
         write (Out, FMT) "x", x
      close (Out)
   end subroutine Output_Integral_Values

   ! Вывод значения интеграла.
   subroutine Output_Integral_Result(output_file, u)
      character(*), intent(in) :: OUTPUT_FILE
      real(R_),     intent(in) :: u
      integer                  :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *)
         write (Out, FMT) "u", u
      close (Out)
   end subroutine Output_Integral_Result

end module Integral_IO
