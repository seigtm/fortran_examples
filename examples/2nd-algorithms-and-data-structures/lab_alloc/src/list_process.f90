
module List_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use List_IO

   implicit none

contains
   recursive subroutine Put(Elem, value)
      type(node), allocatable, intent(inout) :: Elem
      integer, intent(in)     :: value

      if (.not. Allocated(Elem)) then
         !allocate (Elem, source=node(value))
         allocate (Elem)
         Elem%value = value
      else
        call Put(Elem%next, value)
     end if
   end subroutine Put

   pure subroutine Get(Elem, value)
      type(node), allocatable, intent(inout) :: Elem
      integer, intent(out)  :: value
      type(node), allocatable :: temp
      
      if (allocated(Elem)) then
         value = Elem%value
         !Elem = Elem%next
         !call move_alloc(Elem%next, Elem)
         call move_alloc(Elem%next, temp)
         call move_alloc(temp, Elem)
      else
        value = 0
     end if
   end subroutine Get 

   pure recursive subroutine Delete(current, value)
      type(node), allocatable, intent(inout) :: current
      integer, intent(in)     :: value
      
      type(node), allocatable :: temp
      
      if (Allocated(current)) then
         if (current%value == value) then
            ! current = current%next
			! call move_alloc(current%next, current)
			call move_alloc(current%next, temp)
            call move_alloc(temp, current)
         else
            call Delete(current%next, value)
         end if
     end if
   end subroutine Delete

end module List_process
