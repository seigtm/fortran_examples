module Matrix_process
   use Environment
   
   implicit none
contains
   ! Чистая функция в императивном стиле.
   pure real(R_) function MaxVal_Imp(A, l)
      real(R_)    A(:, :)
      integer     l
      intent(in)  A, l

      integer i

      MaxVal_Imp = A(l, 1)
      do i = 2, UBound(A, 2)
         if (A(l, i) > MaxVal_Imp) &
            MaxVal_Imp = A(l, i)
      end do
   end function MaxVal_Imp
end module Matrix_process
