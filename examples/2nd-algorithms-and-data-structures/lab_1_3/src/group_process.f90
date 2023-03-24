! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none
   
contains
   ! Сортировка списка класса по среднему баллу.
   pure subroutine Sort_class_list(Group)
      type(student), intent(inout)  :: Group(:)

      integer        :: i, j
      type(student)  :: tmp_stud

      ! Сортировка списка класса по среднему баллу методом пузырька.
      do i = Size(Group), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять учащихся местами.
            if (Swap(Group, j)) then
               ! Перестановка местами двух эелементов списка, начиная с текущего.
               tmp_stud = Group(j+1)
               Group(j+1) = Group(j)
               Group(j) = tmp_stud
			   ! Group(j:j+1) = Group(j+1:j:-1)
            end if
         end do
      end do
   end subroutine Sort_class_list

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Group, j)
      type(student), intent(in)  :: Group(:)
      integer, intent(in)        :: j

      Swap = .false.
      if (Group(j)%Aver_mark < Group(j+1)%Aver_mark) then
         Swap = .true.
      else if (Group(j)%Aver_mark == Group(j+1)%Aver_mark) then
         if (Group(j)%Surname > Group(j+1)%Surname) then
            Swap = .true.
         else if (Group(j)%Surname==Group(j+1)%Surname .and. Group(j)%Initials>Group(j+1)%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap
end module group_process
