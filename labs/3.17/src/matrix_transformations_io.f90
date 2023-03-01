module Matrix_Transformations_IO
   use Environment
   implicit none

contains
   subroutine Read_Matrix(input_file, matrix)
      character(*),          intent(in)    :: input_file
      real(R_), allocatable, intent(inout) :: matrix(:, :)
      integer(I_)                          :: In, Row, MatrixSize

      open (file=input_file, newunit=In)
         read (In, *) MatrixSize
         allocate (matrix(MatrixSize, MatrixSize))
         ! Хранение в памяти по столбцам.
         read (In, *) (matrix(Row, :), Row = 1, MatrixSize)
      close (In)
   end subroutine Read_Matrix

   subroutine Output_Matrix(output_file, matrix, position)
      character(*), intent(in) :: output_file, position
      real(R_),     intent(in) :: matrix(:, :)
      integer(I_)              :: Row, Out, MatrixSize

      MatrixSize = size(array=matrix, dim=2)

      open (file=output_file, encoding=E_, newunit=Out, position=position)
         write (Out, "("//MatrixSize//"f6.2)") (matrix(Row, :), Row = 1, MatrixSize)
         write (Out, *)
      close (Out)
   end subroutine Output_Matrix

end module Matrix_Transformations_IO
