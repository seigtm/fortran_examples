program exercise_5
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   integer                 :: S = 0
   integer, allocatable    :: Z(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)

   !call PositiveImp(Z, S, M)

   ! Размещение данных в НАЧАЛЕ работы программы,
   ! а не внутри подпрограммы при КАЖДОМ её вызове.
   allocate(Pos(N))
   call Positive(Z, Pos, S, M)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      write (Out, '(/2(a, T12, "= ", i0/))') 'Pos. items', M, "Sum", S
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine PositiveImp(Z, S, M)
      integer     Z(:), S, M
      intent(in)  Z
      intent(out) S, M
      integer     i

      S = 0
      M = 0
      do i = 1, N
         If (Z(i) > 0) then
            S = S + Z(i)
            M = M + 1
         end if
      end do
   end subroutine PositiveImp

   ! Чистая подпрограмма в императивном стиле.
   pure subroutine Positive(Z, Pos, S, M)
      integer     Z(:), S, M
      logical     Pos(:)
      intent(in)  Z
      intent(out) Pos, S, M

      Pos = Z > 0
      S = Sum(Z, Pos)
      M = Count(Pos)
   end subroutine Positive
end program exercise_5
