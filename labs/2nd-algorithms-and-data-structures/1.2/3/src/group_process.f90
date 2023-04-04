module group_process
   use environment
   use group_io
   implicit none

contains
   ! Сортировка списка группы по среднему баллу.
   pure subroutine sort_students_list(group)
      type(student), intent(inout)  :: group(:)     ! Список студентов.
      type(student)                 :: student_tmp  ! Временная переменная для хранения студента.
      integer                       :: i, j

      ! Сортировка списка группы по среднему баллу методом пузырька.
      do i = Size(group), 2, -1  ! ! Проходим по списку в обратном порядке, начиная с конца.
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Если текущий студент должен поменяться местами со следующим,
            if (should_swap(group, j)) then
               ! перестановка местами двух элементов списка, начиная с текущего.
               student_tmp = group(j+1)
               group(j+1)  = group(j)
               group(j)    = student_tmp
            end if
         end do
      end do
   end subroutine sort_students_list

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
