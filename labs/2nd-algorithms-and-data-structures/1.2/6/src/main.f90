! Баранов К.П., 20021, ЛР 1, вариант 2, средства 6.
! Средства:
!   - массивы строк;
!   - массивы символов;
!   ? массив структур (либо это);
!   ? структура массивов (либо это);
!   - файлы записей;
!   + хвостовая рекурсия;
!   + однонаправленные списки заранее неизвестной длины.

! Необходимо прочитать список известной длины из не менее чем 12 строк.
! Данные в одной строке имеют заданный формат и отделяются друг от друга дополнительным пробелом.

! Дан список группы в виде:
!   ФАМИЛИЯ   И.О.    ПОЛ   ПРОПИСКА СРЕДНИЙ_БАЛЛ
!   15 симв. 5 симв. 1 симв. 1 симв.   4 симв.
!   Пример входного файла (в графе прописки буква П стоит у петербурцев, С — у гостей Санкт-Петербурга):
!   Иванов И. И. М П 4.35
!   Отсортировать по убыванию среднего балла по отдельности списки петербуржцев и гостей Санкт-Петербурга.
!   Пример выходного файла:
!   Петербуржцы:
!   Иванов И. И. М 4.35
!   Барабашкин И. И. М 4.32
!   Гости города:
!   Петров И. И. М 4.35
!   Петрыкин И. И. М 4.32

program sort_students
   use environment
   use group_io
   use group_process
   implicit none

   character(:), allocatable      :: input_file, output_file
   character(kind=CH_), parameter :: citizen = char(1055, CH_), guest = char(1057, CH_)  ! 'П', 'С'.
   type(student), pointer         :: group_list => Null(), guest_list => Null(), citizen_list => Null()
   integer(I_)                    :: guest_count = 0, citizen_count = 0

   input_file  = "../data/input.txt"
   output_file = "output.txt"

   group_list => read_group_list(input_file)

   if(Associated(group_list)) then
      call output_students_list(output_file, group_list, "Исходный список:", "rewind")

      call get_list_by_registration(group_list, citizen_list, citizen_count, citizen)
      call get_list_by_registration(group_list, guest_list,   guest_count,   guest)

      call sort_students_list(citizen_list, citizen_count)
      call sort_students_list(guest_list,   guest_count)

      if(Associated(citizen_list)) &
         call output_students_list(output_file, citizen_list, "Петербуржцы:",  "append")
      if(Associated(guest_list)) &
         call output_students_list(output_file, guest_list,   "Гости города:", "append")
   end if
end program sort_students
