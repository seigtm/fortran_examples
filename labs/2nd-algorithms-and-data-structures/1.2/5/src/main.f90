! Баранов К.П., 20021, ЛР 1, вариант 2, средства 5.
! Средства:
!   - массивы строк;
!   - массивы символов;
!   ? массив структур (либо это) - ВЫБРАЛ ЭТО;
!   ? структура массивов (либо это);
!   + файлы записей;
!   + хвостовая рекурсия;
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
   type(student)                    :: group(students_count)
   type(student),       allocatable :: citizens(:), guests(:)

   input_file  = "../data/input.txt"
   output_file = "output.txt"
   data_file   = "input.dat"

   call create_data_file(input_file, data_file)
   group = read_students_list(data_file)
   call output_students_list(output_file, group, "Исходный список:", "rewind")

   ! Создание массивов граждан и гостей города путём фильтрации из списка
   !  "group" с помощью функции "Pack".
   citizens = Pack(group, group%registration == citizen)
   guests   = Pack(group, group%registration == guest)

   call sort_students_list(citizens, size(citizens))
   call sort_students_list(guests,   size(guests))

   call output_students_list(output_file, citizens, "Петербуржцы:",  "append")
   call output_students_list(output_file, guests,   "Гости города:", "append")
end program sort_students
