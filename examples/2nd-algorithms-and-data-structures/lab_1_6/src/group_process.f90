! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Получение списков по полу.
   pure recursive subroutine Get_list_by_gender(Stud, List, Amount, Gender)
      type(student), intent(in)        :: Stud
      type(student), pointer           :: List
      integer(I_), intent(inout)       :: Amount
      character(kind=CH_), intent(in)  :: Gender
     
      ! Если найден студент нужного пола, то размещаем в новом списке элемент и копируем его данные.
      if (Stud%Sex == Gender) then
         allocate (List, source=Stud)
         Amount = Amount + 1
         List%Aver_Mark = Real(Sum(List%Marks), R_) / MARKS_AMOUNT
         List%next => Null() ! НЕ ЗАБЫВАЕМ!
         ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
         if (Associated(Stud%next)) &
            call Get_list_by_gender(Stud%next, List%next, Amount, Gender)
      ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
      else if (Associated(Stud%next)) then
         call Get_list_by_gender(Stud%next, List, Amount, Gender)
      else
	 List => Null()  ! Зачем мы делаем это?
      end if

   end subroutine Get_list_by_gender
 
   ! Сортировка списка класса по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine Sort_class_list(ClassList, N)
      type(student), pointer, intent(inout)  :: ClassList
      integer, intent(in)                    :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(ClassList, 1, N-1)
      
      ! Если необходимо, делаем то же с первыми N-1 элементами.
      if (N >= 3) &
         call Sort_class_list(ClassList, N-1)
   end subroutine Sort_class_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(ClassList, j, N)
      type(student), pointer  :: ClassList
      integer, intent(in)                    :: j, N

      ! Если требуется, то меняем местами текущего студента со следующим.
      if (Swap(ClassList)) &
         call Swap_from_current(ClassList)
      if (j < N) &
         call Drop_down(ClassList%next, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Current)
      type(student), intent(in)  :: Current

      Swap = .false.
      if (Current%Aver_Mark < Current%next%Aver_Mark) then
         Swap = .true.
      else if (Current%Aver_Mark == Current%next%Aver_Mark) then
         if (Current%Surname > Current%next%Surname) then
            Swap = .true.
         else if (Current%Surname==Current%next%Surname .and. Current%Initials>Current%next%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap

   ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Current)
      type(student), pointer  :: Current

      type(student), pointer  :: tmp_stud
               
      tmp_stud       => Current%next
	  ! трактуем так: tmp_stud ссылается ТУДА ЖЕ, КУДА Current%next,
	  ! т. е. (на следуюший объект от объекта,
	  ! на который ссылается current)
      Current%next   => Current%next%next
      tmp_stud%next  => Current
      Current        => tmp_stud
   end subroutine Swap_from_current

end module Group_process
