module Norm2_IO
   use Environment
   implicit none

contains
   subroutine Read_Matrix(input_file, matrix)
      character(*),          intent(in)  :: input_file
      real(R_), allocatable, intent(out) :: matrix(:, :)
      integer(I_)                        :: in, row, matrix_size

      open (file=input_file, newunit=in)
         read (in, *) matrix_size
         allocate (matrix(matrix_size, matrix_size))
         ! Хранение в памяти по столбцам.
         read (in, *) (matrix(row, :), row = 1, matrix_size)
      close (in)
   end subroutine Read_Matrix

   subroutine Output_Matrix(output_file, matrix, position)
      character(*), intent(in) :: output_file, position
      real(R_),     intent(in) :: matrix(:, :)
      integer(I_)              :: row, out, matrix_size

      matrix_size = size(array=matrix, dim=2)

      open (file=output_file, encoding=E_, newunit=out, position=position)
         write (out, "(" // matrix_size // "f6.2)") (matrix(row, :), row = 1, matrix_size)
         write (out, *)
      close (out)
   end subroutine Output_Matrix

   subroutine Output_Euclidean_Norm(output_file, norm, position)
      character(*), intent(in) :: output_file, position
      real(R_),     intent(in) :: norm
      integer(I_)              :: out

      open (file=output_file, encoding=E_, newunit=out, position=position)
         write (out, *) "|| z || = ", norm
      close (out)
   end subroutine Output_Euclidean_Norm

end module Norm2_IO
