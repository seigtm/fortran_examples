program exercise_8_ex2
   use Environment
   use Matrix_IO
   use Matrix_process
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   !integer                 :: i = 0, N = 0
   real(R_), allocatable   :: B(:, :), C(:)

   B = ReadMatrix(input_file)

   call OutputMatrix(output_file, B)
   
   !N = UBound(B, 1)
   !allocate (C(N))
   !do concurrent (i = 1:N)
   !   C(i) = MaxVal_Imp(B, i)
   !end do

   C = MaxVal(B, dim=2) ! Оформлять отдельную чистую функцию в регулярном стиле нет необходимости.

   call OutputArray(output_file, C)
end program exercise_8_ex2
