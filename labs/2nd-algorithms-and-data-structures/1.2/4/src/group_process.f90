module group_process
   use environment
   use group_io
   implicit none

contains
   ! Сортировка списка группы по среднему баллу.
   pure subroutine sort_students_list(group)
      type(students), intent(inout)        :: group
      character(surname_length, kind=CH_)  :: surname_tmp
      character(initials_length, kind=CH_) :: initials_tmp
      character(kind=CH_)                  :: sex_tmp
      real(R_)                             :: avg_mark_tmp
      integer                              :: i, j

      ! Сортировка списка группы по среднему баллу методом пузырька.
      do i = students_count, 2, -1  ! Проходим в обратном порядке, начиная с конца.
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Если текущий студент должен поменяться местами со следующим,
            if (should_swap(group, j)) then
               surname_tmp        = group%surname(j+1)
               group%surname(j+1) = group%surname(j)
               group%surname(j)   = surname_tmp

               initials_tmp        = group%initials(j+1)
               group%initials(j+1) = group%initials(j)
               group%initials(j)   = initials_tmp

               sex_tmp        = group%sex(j+1)
               group%sex(j+1) = group%sex(j)
               group%sex(j)   = sex_tmp

               avg_mark_tmp        = group%avg_mark(j+1)
               group%avg_mark(j+1) = group%avg_mark(j)
               group%avg_mark(j)   = avg_mark_tmp
            end if
         end do
      end do
   end subroutine sort_students_list

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function should_swap(group, j)
      type(students), intent(in) :: group  ! Список студентов.
      integer,        intent(in) :: j

      ! Изначально студенты не должны меняться местами.
      should_swap = .false.
      ! Если у текущего студента средний балл меньше, чем у следующего,
      if (group%avg_mark(j) < Group%avg_mark(j+1)) then
         should_swap = .true.  ! ! то они должны поменяться местами.
      ! Иначе, если у них одинаковый средний балл,
      else if (group%avg_mark(j) == group%avg_mark(j+1)) then
         ! Сравниваем их фамилии.
         if (group%surname(j) > group%surname(j+1)) then
            should_swap = .true.
         ! Если у них одинаковая фамилия, сравниваем их инициалы.
         else if (group%surname(j) == group%surname(j+1) .and. group%initials(j) > group%initials(j+1)) then
            should_swap = .true.
         end if
      end if
   end function should_swap
end module group_process
