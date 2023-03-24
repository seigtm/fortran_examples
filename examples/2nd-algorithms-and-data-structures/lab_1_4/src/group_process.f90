! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Сортировка списка класса по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine Sort_class_list(Group, N)
      type(student), intent(inout)  :: Group(:)
      integer, intent(in)           :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(Group, 1, N-1)
      
      ! Если необходимо, делаем то же с последними N-1 элементами.
      if (N >= 3) &
         call Sort_class_list(Group, N-1)
   end subroutine Sort_class_list
  
   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(Group, j, N)
      type(student), intent(inout)  :: Group(:)
      integer, intent(in)           :: j, N
      
      type(student)  :: tmp_stud

      ! Если требуется, то меняем местами текущего студента со следующим.
      if (Swap(Group, j)) then
         tmp_stud = Group(j+1)
         Group(j+1) = Group(j)
         Group(j) = tmp_stud
      end if
      if (j < N) &
         call Drop_down(Group, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Group, j)
      type(student)  :: Group(:)
      integer        :: j
      intent(in)     Group, j

      Swap = .false.
      if (Group(j)%Aver_mark < Group(j+1)%Aver_mark) then
         Swap = .true.
      else if (Group(j)%Aver_mark == Group(j+1)%Aver_mark) then
         if (Group(j)%Surname > Group(j+1)%Surname) then
            Swap = .true.
         else if (Group(j)%Surname==Group(j+1)%Surname .and. Group(j)%Initials<Group(j+1)%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap
end module group_process
