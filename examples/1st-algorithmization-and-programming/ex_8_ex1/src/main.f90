program exercise_8_ex1
   use Environment
   use Array_IO
   use Array_process

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: N = 0, i = 0
   integer, allocatable    :: Z(:, :), Y(:)

   N = ReadN(input_file)
   
   Y  = [(i**2, i = 1, N*N)] ! Отдельную процедуру заводить нет необходимости.

   !Z = Form_Imp(Y)
   Z = Reshape(Y, [N, N]) ! Отдельную процедуру заводить нет необходимости.
  
   call OutputArrays(output_file, Y, Z, N)
end program exercise_8_ex1
