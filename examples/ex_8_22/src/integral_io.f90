module Integral_IO
   use Environment

   implicit none
contains
   ! Чтение параметра p.
   subroutine ReadP(input_file, p1, p2, delta_p)
      character(*), intent(in) :: input_file
      real(R_), intent(out)    :: p1, p2, delta_p

      integer :: In = 0
   
      open (file=input_file, newunit=In)
         read (In, *) p1, p2, delta_p
      close (In)
   end subroutine ReadP
  
   ! Вывод параметра p.
   subroutine OutputP(output_file, p1, p2, delta_p)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: p1, p2, delta_p

      integer :: Out = 0
   
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(3(a, T9, "= ", f0.4/))') "p1", p1, "p2", p2, "delta_p", delta_p
      close (Out)
   end subroutine OutputP
   
   ! Вывод значений интеграла для разных p.
   subroutine OutputIntegral(output_file, P, I)
      character(*), intent(in) :: output_file
      real(R_), intent(in) :: I(:), P(:)

      integer :: Out = 0, j = 0
   
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, '("  p    |   I")')
         write (Out, '(f7.4, T8, "| ", f7.4)') (P(j), I(j), j = 1, Size(P))
      close (Out)
   end subroutine OutputIntegral
end module Integral_IO
