module Array_process
   use Environment

   implicit none
contains
   ! Чистая функция в императивном стиле.
   pure function Form_Imp(B) result(A)
      integer, intent(in)  :: B(:)
      integer, allocatable :: A(:, :)

      integer i, j, k, N

      N = Int(SqRt(Real(Ubound(B, 1) + 0.5)))
      allocate(A(N, N))
      k = 1
      do j = 1, N
         do i = 1, N
            A(i, j) = B(k)
            k = k + 1
         end do
      end do
   end function Form_Imp
end module Array_process
