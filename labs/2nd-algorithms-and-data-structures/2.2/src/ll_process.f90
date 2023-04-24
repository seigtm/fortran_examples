module ll_process
   use environment
   use ll_io
   implicit none

contains
   recursive subroutine check_string(list, alphabet, result)
      type(node), allocatable, intent(in) :: list, alphabet
      integer(I_),         intent(inout)  :: result

      if(allocated(list)) then
         if(contains(alphabet, list%value)) then
            call check_string(list%next, alphabet, result)
         else
            result = ichar(list%value)
         end if
      endif
   end subroutine check_string

   recursive logical function contains(alphabet, ch) result(res)
      type(node), allocatable, intent(in) :: alphabet
      character(kind=CH_),     intent(in) :: ch

      write(*,*) alphabet%value, ch

      if(alphabet%value == ch) then
         res = .true.
      else if(allocated(alphabet%next)) then
         res = contains(alphabet%next, ch)
      else
         res = .false.
      end if
   end function contains
end module ll_process
