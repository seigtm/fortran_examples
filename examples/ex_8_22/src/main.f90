program exercise_8_22
   use Environment
   use Integral_IO
   use Integral_calculate 

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_)                :: p1 = 0, p2 = 0, delta_p = 0
   real(R_), allocatable   :: I(:), P(:), X(:)
   integer                 :: M = 0

   call ReadP(input_file, p1, p2, delta_p)
   
   call OutputP(output_file, p1, p2, delta_p)

   ! Размещение массивов ОДИН раз в начале работы программы,
   ! а НЕ КАЖДЫЙ раз при вызове процедуры.
   M = Int((p2 - p1) / delta_p + .5_R_) + 1
   allocate(P(M), X(N), I(M))

   !call Integral_Imp(p1, delta_p, P, X, I)
   call Integral(p1, delta_p, P, X, I)
   
   call OutputIntegral(output_file, P, I)
end program exercise_8_22
