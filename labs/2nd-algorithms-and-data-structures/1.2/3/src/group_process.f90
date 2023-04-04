module group_process
   use environment
   use group_io
   implicit none

contains
   ! Сортировка списка группы по среднему баллу.
   pure subroutine sort_students_list(group)
      type(student), intent(inout)  :: group(:)
      integer                       :: i, j
      type(student)                 :: student_tmp

      ! Сортировка списка группы по среднему баллу методом пузырька.
      do i = Size(group), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять учащихся местами.
            if (should_swap(group, j)) then
               ! Перестановка местами двух эелементов списка, начиная с текущего.
               student_tmp = group(j+1)
               group(j+1)  = group(j)
               group(j)    = student_tmp
            end if
         end do
      end do
   end subroutine sort_students_list

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function should_swap(group, j)
      type(student), intent(in) :: group(:)
      integer,       intent(in) :: j

      should_swap = .false.
      if (group(j)%avg_mark < Group(j+1)%avg_mark) then
         should_swap = .true.
      else if (group(j)%avg_mark == group(j+1)%avg_mark) then
         if (group(j)%surname > group(j+1)%surname) then
            should_swap = .true.
         else if (group(j)%surname == group(j+1)%surname .and. group(j)%initials > group(j+1)%initials) then
            should_swap = .true.
         end if
      end if
   end function should_swap
end module group_process
