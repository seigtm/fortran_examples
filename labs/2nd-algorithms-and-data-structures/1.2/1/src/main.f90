! Баранов К.П., 20021, ЛР 1, вариант 2, средства 1.
! Средства:
!   + массивы строк;
!   - массивы символов;
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

   implicit none

   integer, parameter               :: students_count = 18, surname_length = 15, initials_length = 5
   character(kind=CH_), parameter   :: citizen = char(1055, CH_)  ! 'П'

   character(:), allocatable  :: input_file, output_file, format

   character(surname_length, kind=CH_)                :: surname_tmp = "", surnames(students_count) = ""
   character(surname_length, kind=CH_), allocatable   :: surnames_guests(:), surnames_citizens(:)

   character(initials_length, kind=CH_)               :: initials_tmp = "", initials(students_count) = ""
   character(initials_length, kind=CH_), allocatable  :: initials_guests(:), initials_citizens(:)

   character(kind=CH_)              :: gender_tmp = "", genders(students_count) = ""
   character(kind=CH_), allocatable :: genders_guests(:), genders_citizens(:)

   character(kind=CH_)                             :: registrations(students_count) = ""

   integer, allocatable                            :: position_guests(:), position_citizens(:)

   real(R_)                                        :: avg_mark_tmp = 0, avg_marks(students_count) = 0
   real(R_), allocatable                           :: avg_marks_citizens(:), avg_marks_guests(:)

   logical, allocatable                            :: is_citizen(:), is_guest(:)
   integer                                         :: citizen_count = 0, guest_count = 0

   integer :: in, out, io, i, j
   integer, parameter                              :: indexes(*) = [(i, i = 1, students_count)]
   logical :: swap

   input_file = "../data/input.txt"
   output_file = "output.txt"

   ! Чтение списка класса: фамилии, инициалы, пол, прописка и средний балл.
   open(file=input_file, encoding=E_, newunit=in)
   format = '(4(a, 1x), f5.2)'
   read(in, format, iostat=io) (surnames(i), initials(i), genders(i), registrations(i), avg_marks(i), i = 1, students_count)
   close(in)
   ! Обработка статуса чтения.
   out = OUTPUT_UNIT
   open(out, encoding=E_)
   select case(io)
    case(0)
    case(IOSTAT_END)
      write(out, *) "End of file has been reached while reading group list."
    case(1:)
      write(out, *) "Error while reading group list: ", io
    case default
      write(out, *) "Undetermined error has been reached while reading group list: ", io
   end select

   ! Вывод списка группы.
   open(file=output_file, encoding=E_, newunit=out)
   write(out, '(a)') "Исходный список:"
   ! Пояснения к записи те же, что и к чтению.
   write(out, format, iostat=IO) (surnames(i), initials(i), genders(i), registrations(i), avg_marks(i), i = 1, students_count)
   close(out)
   ! Обработка статуса записи.
   out = OUTPUT_UNIT
   open(out, encoding=E_)
   select case(io)
    case(0)
    case(IOSTAT_END)
      write(out, *) "End of file has been reached while writing class list."
    case(1:)
      write(out, *) "Error while writing class list: ", io
    case default
      write(out, *) "Undetermined error has been reached while writing class list: ", io
   end select

   ! Составление логической маски, соответствующей жителям.
   is_citizen    = registrations == citizen
   citizen_count = Count(is_citizen)
   ! Получение массивов, связынных с жителями.
   ! Использование массива номеров жителей в списке.
   position_citizens = Pack(indexes, is_citizen)  ! == [1, 2, 3]
   allocate(surnames_citizens(citizen_count), initials_citizens(citizen_count), &
      genders_citizens(citizen_count), avg_marks_citizens(citizen_count))
   do concurrent(i = 1:citizen_count)
      ! Получение списков жителей.
      surnames_citizens(i)  = surnames(position_citizens(i))
      initials_citizens(i)  = initials(position_citizens(i))
      genders_citizens(i)   = genders(position_citizens(i))
      avg_marks_citizens(i) = avg_marks(position_citizens(i))
   end do

   ! Составление логической маски, соответствующей гостям.
   is_guest    = .not. is_citizen
   guest_count = students_count - citizen_count
   ! Получение массивов, связынных с гостями.
   ! Использование массива номеров гостей в списке.
   position_guests = Pack(indexes, is_guest)  ! == [4, 5]
   allocate(surnames_guests(guest_count), initials_guests(guest_count), &
      genders_guests(guest_count), avg_marks_guests(guest_count))
   do concurrent(i = 1:guest_count)
      ! Получение списков гостей.
      surnames_guests(i)  = surnames(position_guests(i))
      initials_guests(i)  = initials(position_guests(i))
      genders_guests(i)   = genders(position_guests(i))
      avg_marks_guests(i) = avg_marks(position_guests(i))
   end do

   ! Сортировка списка жителей по среднему баллу методом пузырька.
   do i = citizen_count, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if(avg_marks_citizens(j) < avg_marks_citizens(j+1)) then
            Swap = .true.
         else if(avg_marks_citizens(j) == avg_marks_citizens(j+1)) then
            if (surnames_citizens(j) > surnames_citizens(j+1)) then
               Swap = .true.
            else if(surnames_citizens(j) == surnames_citizens(j+1) &
               .and. initials_citizens(j) > initials_citizens(j+1)) then
               Swap = .true.
            end if
         end if

         if(swap) then
            surname_tmp            = surnames_citizens(j+1)
            surnames_citizens(j+1) = surnames_citizens(j)
            surnames_citizens(j)   = surname_tmp

            initials_tmp           = initials_citizens(j+1)
            initials_citizens(j+1) = initials_citizens(j)
            initials_citizens(j)   = initials_tmp

            gender_tmp            = genders_citizens(j+1)
            genders_citizens(j+1) = genders_citizens(j)
            genders_citizens(j)   = gender_tmp

            avg_mark_tmp            = avg_marks_citizens(j+1)
            avg_marks_citizens(j+1) = avg_marks_citizens(j)
            avg_marks_citizens(j)   = avg_mark_tmp
         end if
      end do
   end do

   ! Сортировка списка гостей по среднему баллу методом пузырька.
   do i = guest_count, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if(avg_marks_guests(j) < avg_marks_guests(j+1)) then
            Swap = .true.
         else if(avg_marks_guests(j) == avg_marks_guests(j+1)) then
            if (surnames_guests(j) > surnames_guests(j+1)) then
               Swap = .true.
            else if(surnames_guests(j) == surnames_guests(j+1) &
               .and. initials_guests(j) > initials_guests(j+1)) then
               Swap = .true.
            end if
         end if

         if(swap) then
            surname_tmp          = surnames_guests(j+1)
            surnames_guests(j+1) = surnames_guests(j)
            surnames_guests(j)   = surname_tmp

            initials_tmp         = initials_guests(j+1)
            initials_guests(j+1) = initials_guests(j)
            initials_guests(j)   = initials_tmp

            gender_tmp          = genders_guests(j+1)
            genders_guests(j+1) = genders_guests(j)
            genders_guests(j)   = gender_tmp

            avg_mark_tmp          = avg_marks_guests(j+1)
            avg_marks_guests(j+1) = avg_marks_guests(j)
            avg_marks_guests(j)   = avg_mark_tmp
         end if
      end do
   end do

   ! Вывод отсортированного списка жителей.
   open(file=output_file, encoding=E_, position='append', newunit=out)
   write(out, '(/a)') "Петербуржцы:"
   write(out, format, iostat=io) &
      (surnames_citizens(i), initials_citizens(i), genders_citizens(i), "П", avg_marks_citizens(i), i = 1, citizen_count)
   close(out)
   ! Обработка статуса записи.
   out = OUTPUT_UNIT
   open(out, encoding=E_)
   select case(io)
    case(0)
    case(IOSTAT_END)
      write(out, *) "End of file has been reached while writing sorted citizens list."
    case(1:)
      write(out, *) "Error while writing sorted citizens list: ", io
    case default
      write(out, *) "Undetermined error has been reached while writing sorted citizens list: ", io
   end select

   ! Вывод отсортированного списка гостей.
   open(file=output_file, encoding=E_, position='append', newunit=out)
   write(out, '(/a)') "Гости города:"
   write(out, format, iostat=io) &
      (surnames_guests(i), initials_guests(i), genders_guests(i), "Г", avg_marks_guests(i), i = 1, guest_count)
   close(out)
   ! Обработка статуса записи.
   out = OUTPUT_UNIT
   open(out, encoding=E_)
   select case(io)
    case(0)
    case(IOSTAT_END)
      write(out, *) "End of file has been reached while writing sorted guests list."
    case(1:)
      write(out, *) "Error while writing sorted guests list: ", io
    case default
      write(out, *) "Undetermined error has been reached while writing sorted guests list: ", io
   end select
end program sort_students
