module group_process
   use environment
   use group_io
   implicit none

contains
   ! Получение списков по прописке.
   pure recursive subroutine get_list_by_registration(stud, list, amount, registration)
      type(student),       intent(in)    :: stud
      type(student),       pointer       :: list
      integer(I_),         intent(inout) :: amount
      character(kind=CH_), intent(in)    :: registration

      ! Если найден студент с нужной пропиской,
      !  то размещаем в новом списке элемент и копируем его данные.
      if (stud%registration == registration) then
         allocate(list, source=stud)
         amount = amount + 1
         list%next => Null()
         ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
         if(associated(stud%next)) &
            call get_list_by_registration(stud%next, list%next, amount, registration)
      ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
      else if(Associated(stud%next)) then
         call get_list_by_registration(stud%next, list, amount, registration)
     else
        list => Null()
      end if
   end subroutine get_list_by_registration

   ! Сортировка списка класса по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine sort_student_list(student_list, n)
      type(student), pointer, intent(inout) :: student_list
      integer,                intent(in)    :: n

      ! Работаем только с первыми n элементами: помещаем в ИХ конец менее успешного.
      call drop_down(student_list, 1, n-1)
      
      ! Если необходимо, делаем то же с первыми n-1 элементами.
      if (n >= 3) &
         call sort_student_list(student_list, n-1)
   end subroutine sort_student_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine drop_down(student_list, j, n)
      type(student), pointer    :: student_list
      integer,       intent(in) :: j, n

      ! Если требуется, то меняем местами текущего студента со следующим.
      if(should_swap(student_list)) &
         call swap(student_list)
      if(j < n) &
         call drop_down(student_list%next, j+1, n)
   end subroutine drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function should_swap(current)
      type(student), intent(in)  :: current

      should_swap = .false.
      if(current%avg_mark < current%next%avg_mark) then
         should_swap = .true.
      else if(current%avg_mark == current%next%avg_mark) then
         if(current%surname > current%next%surname) then
            should_swap = .true.
         else if(current%surname==current%next%surname .and. current%initials>current%next%initials) then
            should_swap = .true.
         end if
      end if
   end function should_swap

   ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine swap(current)
      type(student), pointer  :: current
      type(student), pointer  :: student_tmp

      student_tmp      => current%next
      ! Трактуем так: student_tmp ссылается ТУДА ЖЕ, КУДА current%next,
      !  т.е. (на следуюший объект от объекта, на который ссылается current).
      current%next     => current%next%next
      student_tmp%next => current
      current          => student_tmp
   end subroutine swap
end module group_process
