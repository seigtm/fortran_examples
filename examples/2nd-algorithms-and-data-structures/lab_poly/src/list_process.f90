! Copyright 2019 Fyodorov S. A.

module List_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use List_IO

   implicit none

contains
!   pure recursive subroutine Put(Elem, value)
!      type(node), pointer  :: Elem
!      integer, intent(in)  :: value
!
!      if (.not. Associated(Elem)) then
!         allocate (Elem, source=node(value, Null()))
!         ! Можно было также:
!         ! allocate (Elem)
!         ! Elem%value = value
!         ! Elem%next => Null() ! Необязательно, если инициализируется как Null()
!      else
!        call Put(Elem%next, value)
!     end if
!   end subroutine Put
!
!   pure subroutine Get(Elem, value)
!      type(node), pointer  :: Elem
!      integer, intent(out)  :: value
!      
!      type(node), pointer  :: tmp
!
!      if (Associated(Elem)) then
!         value = Elem%value
!         tmp => Elem
!         Elem => Elem%next
!         deallocate (tmp)
!      else
!        value = 0
!     end if
!   end subroutine Get 
!
!   pure recursive subroutine Delete(current, value)
!      type(node), pointer  :: current
!      integer, intent(in)  :: value
!      
!      type(node), pointer  :: tmp
!      
!      if (Associated(current)) then
!         if (current%value == value) then
!            tmp => current
!            current => tmp%next
!            ! или current => current%next
!            deallocate(tmp)
!         else
!            call Delete(current%next, value)
!         end if
!     end if
!   end subroutine Delete
!
!   pure recursive subroutine Delete_in_tree(current, value)
!      type(node_tree), pointer  :: current
!      integer, intent(in)  :: value
!
!      type(node_tree), pointer  :: left, right
!     
!      if (Associated(current)) then
!        if (current%value == value) then
!           left  => current%left
!           right => current%right
!           deallocate (current)
!           current => right
!           if (Associated(left)) &
!              call Put_to_left(current, left)
!        else if (current%value > value) then
!           call Delete_in_tree(current%left, value)
!        else if (current%value < value) then
!           call Delete_in_tree(current%right, value)
!        end if
!     end if
!   end subroutine Delete_in_tree
!
!   pure recursive subroutine Put_to_left(current, left)
!      type(node_tree), pointer  :: current
!      type(node_tree), pointer  :: left
!
!      if (.not. Associated(current)) then
!         current => left
!      else
!         call Put_to_left(current%left, left)
!      end if
!   end subroutine Put_to_left
end module List_process
