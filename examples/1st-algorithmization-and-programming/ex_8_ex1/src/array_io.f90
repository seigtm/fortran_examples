module Array_IO
   use Environment

   implicit none
contains
   ! Функция чтения размера массива.
   integer function ReadN(input_file)
      character(*), intent(in) :: input_file
      
      integer :: In = 0
      
      open (file=input_file, newunit=In)
         read (In, *) ReadN
      close (In)
   end function ReadN

   ! Процедура вывода массива и матрицы.
   subroutine OutputArrays(output_file, Y, Z, N)
      character(*), intent(in)   :: output_file
      integer, intent(in)        :: Y(:), Z(:, :), N

      integer :: Out = 0, i = 0

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, *) N
         write (Out, '('//Size(Y)//'i4)') Y
         write (Out, '('//N//'i4)') (Z(i, :), i = 1, N)
      close (Out)
   end subroutine OutputArrays
end module Array_IO
