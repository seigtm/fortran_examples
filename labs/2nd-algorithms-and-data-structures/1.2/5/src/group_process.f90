module group_process
   use environment
   use group_io
   implicit none

contains
   ! Хвостовая рекурсивная сортировка списка группы по среднему баллу.
   pure recursive subroutine sort_students_list(group, n)
      type(student), intent(inout) :: group(:)  ! Список студентов.
      integer,       intent (in)   :: n

      ! Работаем только с первыми n элементами: помещаем в ИХ конец менее успешного.
      call drop_down(group, 1, n-1)
      ! Если необходимо, делаем то же с последними n-1 элементами.
      if(n >= 3) &
         call sort_students_list(group, n-1)
   end subroutine sort_students_list

   ! Помещаем c j-ой на n-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine drop_down(group, j, n)
      type(student), intent(inout) :: group(:)
      integer,       intent(in)    :: j, n
      type(student)                :: student_tmp

      ! Если требуется, то меняем местами текущего студента со следующим.
      if(should_swap(group, j)) then
         student_tmp = group(j+1)
         group(j+1)  = group(j)
         group(j)    = student_tmp
      end if
      if(j < n) &
         call drop_down(group, j+1, n)
   end subroutine drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function should_swap(group, j)
      type(student), intent(in) :: group(:)  ! Список студентов.
      integer,       intent(in) :: j

      ! Изначально студенты не должны меняться местами.
      should_swap = .false.
      ! Если у текущего студента средний балл меньше, чем у следующего,
      if (group(j)%avg_mark < Group(j+1)%avg_mark) then
         should_swap = .true.  ! ! то они должны поменяться местами.
      ! Иначе, если у них одинаковый средний балл,
      else if (group(j)%avg_mark == group(j+1)%avg_mark) then
         ! Сравниваем их фамилии.
         if (group(j)%surname > group(j+1)%surname) then
            should_swap = .true.
         ! Если у них одинаковая фамилия, сравниваем их инициалы.
         else if (group(j)%surname == group(j+1)%surname .and. group(j)%initials > group(j+1)%initials) then
            should_swap = .true.
         end if
      end if
   end function should_swap
end module group_process
