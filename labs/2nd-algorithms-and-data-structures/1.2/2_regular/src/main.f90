! Баранов К.П., 20021, ЛР 1, вариант 2, средства 2.
! Средства:
!   - массивы строк;
!   + массивы символов;
!   - массив структур;
!   - структура массивов;
!   - файлы записей;
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
   use group_io
   use group_process
   implicit none

   integer(I_)                      :: i = 0
   character(*),        parameter   :: input_file = "../data/input.txt", output_file = "output.txt"
   character(kind=CH_), parameter   :: citizen = char(1055, CH_), guest = char(1057, CH_)  ! 'П', 'С'.
   character(kind=CH_), allocatable :: surnames_guests(:, :), surnames_citizens(:, :)
   character(kind=CH_), allocatable :: initials_guests(:, :), initials_citizens(:, :)
   real(R_),            allocatable :: avg_marks_guests(:), avg_marks_citizens(:)
   character(kind=CH_), allocatable :: genders_citizens(:), genders_guests(:)
   real(R_)                         :: avg_marks(students_count) = 0
   character(kind=CH_)              :: surnames(surname_length, students_count) = "",  &
      initials(initials_length, students_count) = "", &
      genders(students_count) = "", &
      registrations(students_count)

   call read_students_list(  input_file,  surnames, initials, genders, registrations, avg_marks)
   call output_students_list(output_file, surnames, initials, genders, registrations, avg_marks, &
      "Исходный список:", "rewind")

   call get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
      surnames_citizens, initials_citizens, genders_citizens, avg_marks_citizens, citizen)
   call get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
      surnames_guests, initials_guests,     genders_guests,   avg_marks_guests,   guest)

   call sort_students_list(surnames_citizens, initials_citizens, genders_citizens, avg_marks_citizens)
   call sort_students_list(surnames_guests,   initials_guests,   genders_guests,   avg_marks_guests)

   call output_students_list(output_file, surnames_citizens, initials_citizens, genders_citizens, &
      [(citizen, i = 1, Size(avg_marks_citizens))], avg_marks_citizens, "Петербуржцы:",  "append")
   call output_students_list(output_file, surnames_guests,   initials_guests,   genders_guests,   &
      [(guest,   i = 1, Size(avg_marks_guests))],   avg_marks_guests,   "Гости города:", "append")
end program sort_students

