! Баранов К.П., 20021, ЛР 1, вариант 2, средства 4.
! Средства:
!   - массивы строк;
!   - массивы символов;
!   - массив структур;
!   + структура массивов;
!   + файлы записей;
!   - хвостовая рекурсия;
!   - однонаправленные списки заранее неизвестной длины.

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
   use group_process
   use group_io
   implicit none

   character(:),        allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: citizen = char(1055, CH_), guest = char(1057, CH_)  ! 'П', 'С'.
   type(students)                   :: group, citizens, guests
   integer(I_)                      :: citizen_count, guest_count
   ! Variables for timing.
   integer(I_) :: start(8)
   real(R_)    :: time_used

   input_file  = "../data/input.txt"
   output_file = "output.txt"
   data_file   = "input.dat"

   call create_data_file(input_file, data_file)
   group = read_students_list(data_file)
   call output_students_list(output_file, group, "Исходный список:", "rewind", students_count)

   ! Timing starts here.
   call date_and_time(values=start)

   citizen_count = Count(group%registration == citizen)
   guest_count   = Count(group%registration == guest)

   citizens%surname      = Pack(group%surname,      group%registration == citizen)
   guests%surname        = Pack(group%surname,      group%registration == guest)
   citizens%initials     = Pack(group%initials,     group%registration == citizen)
   guests%initials       = Pack(group%initials,     group%registration == guest)
   citizens%sex          = Pack(group%sex,          group%registration == citizen)
   guests%sex            = Pack(group%sex,          group%registration == guest)
   citizens%registration = Pack(group%registration, group%registration == citizen)
   guests%registration   = Pack(group%registration, group%registration == guest)
   citizens%avg_mark     = Pack(group%avg_mark,     group%registration == citizen)
   guests%avg_mark       = Pack(group%avg_mark,     group%registration == guest)

   call sort_students_list(citizens)
   call sort_students_list(guests)

   ! Timing ends here.
   call elapsed_time(time_used, start)
   write(*,*) "Elapsed time in milliseconds = ", time_used

   call output_students_list(output_file, citizens, "Петербуржцы:",  "append", citizen_count)
   call output_students_list(output_file, guests,   "Гости города:", "append", guest_count)
end program sort_students
