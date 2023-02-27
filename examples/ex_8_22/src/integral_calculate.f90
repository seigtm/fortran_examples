module Integral_calculate 
   use Environment

   implicit none
   integer, parameter      :: N = 100
   real(R_), parameter     :: a = 0, b = 1, h = (b - a) / N

contains
   ! Чистая функция вычисления интеграла для заданного p в императивном стиле.
   pure subroutine Integral_Imp(p1, delta_p, P, X, I)
      real(R_), intent(in)    :: p1, delta_p
      real(R_), intent(out)   :: I(:), P(:), X(:)
      integer                 :: j, k

      P = [(p1 + delta_p*(j-1),  j = 1, Size(P))]  ! ВЕКТОРИЗАЦИЯ.
      X = [(a + (j-1)*h,         j = 1, Size(X))]  ! ВЕКТОРИЗАЦИЯ.
      do j = 1, Size(P)
         I(j) = 0
         do k = 1, Size(X)
            I(j) = I(j) + F_Imp(p(j), x(k))
         end do
         I(j) = I(j) * h
      end do
   end subroutine Integral_Imp

   ! Чистая вектор-функция от p и x в императивном стиле.
   pure real(R_) function F_Imp(p, x) result(F)
      real(R_), intent(in) :: p, X
      
      real(R_), parameter  :: q = 2.75_R_

      F = SqRt(p + q * x**2) / (3 + p*x + q*x**2) ! ВЕКТОРИЗАЦИЯ.
   end function F_Imp

   ! Чистая функция вычисления интеграла для заданного p в регулярном стиле.
   pure subroutine Integral(p1, delta_p, P, X, I)
      real(R_), intent(in)    :: p1, delta_p
      real(R_), intent(out)   :: I(:), P(:), X(:)
      integer                 :: j

      P = [(p1 + delta_p*(j-1),  j = 1, Size(P))]  ! ВЕКТОРИЗАЦИЯ.
      X = [(a + (j-1)*h,         j = 1, Size(X))]  ! ВЕКТОРИЗАЦИЯ.
      I = [(Sum(F(P(j), X)),     j = 1, Size(P))]
      I = I * h                                    ! ВЕКТОРИЗАЦИЯ.
   end subroutine Integral

   ! Чистая вектор-функция от скаляра p и массива X в регулярном стиле.
   pure function F(p, X)
      real(R_)    p, X(:)
      real(R_)    F(UBound(X, 1))
      intent(in)  p, X
      
      real(R_), parameter  :: q = 2.75_R_

      F = SqRt(p + q * X**2) / (3 + p*X + q*X**2) ! ВЕКТОРИЗАЦИЯ.
   end function F
end module Integral_calculate 
