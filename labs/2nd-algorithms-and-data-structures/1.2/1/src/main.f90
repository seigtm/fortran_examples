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
 
    integer, parameter               :: students_count = 25500, surname_length = 15, initials_length = 5
    character(kind=CH_), parameter   :: citizen = char(1055, CH_), guest = char(1057, CH_)  ! 'П', 'С'.
 
    character(:), allocatable  :: input_file, output_file, format
 
    character(surname_length, kind=CH_)               :: surnames(students_count) = ""
    character(surname_length, kind=CH_),  allocatable :: surnames_guests(:), surnames_citizens(:)
    character(initials_length, kind=CH_)              :: initials(students_count) = ""
    character(initials_length, kind=CH_), allocatable :: initials_guests(:), initials_citizens(:)
    character(kind=CH_)                               :: genders(students_count) = ""
    character(kind=CH_),                  allocatable :: genders_guests(:), genders_citizens(:)
    character(kind=CH_)                               :: registrations(students_count) = ""
    real(R_)                                          :: avg_marks(students_count) = 0
    real(R_), allocatable                             :: avg_marks_citizens(:), avg_marks_guests(:)
    integer                                           :: in, out, io, i
 
    ! Variables for timing.
    integer(I_) :: start(8)
    real(R_)    :: time_used
 
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
 
    ! Timing starts here.
    call date_and_time(values=start)
 
    ! Получение списков жителей и гостей Санкт-Петербурга.
    call get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
       surnames_citizens, initials_citizens, genders_citizens, avg_marks_citizens, &
       citizen)
    call get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
       surnames_guests, initials_guests, genders_guests, avg_marks_guests, &
       guest)
    
    ! Сортировка списов.
    call sort_students_list(surnames_citizens, initials_citizens, genders_citizens, avg_marks_citizens)
    call sort_students_list(surnames_guests, initials_guests, genders_guests, avg_marks_guests)
 
    ! Timing ends here.
    call elapsed_time(time_used, start)
    write(*,*) "Elapsed time in milliseconds = ", time_used
 
    ! Вывод отсортированного списка жителей.
    open(file=output_file, encoding=E_, position='append', newunit=out)
    write(out, '(/a)') "Петербуржцы:"
    write(out, format, iostat=io) &
       (surnames_citizens(i), initials_citizens(i), genders_citizens(i), citizen, avg_marks_citizens(i), &
            i = 1, size(avg_marks_citizens))
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
       (surnames_guests(i), initials_guests(i), genders_guests(i), guest, avg_marks_guests(i), &
            i = 1, size(avg_marks_guests))
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
 
 contains
    pure subroutine get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
       surnames_registration, initials_registration, genders_registration, avg_marks_registration, registration)
       character(surname_length,  kind=CH_), intent(in) :: surnames(:)
       character(initials_length, kind=CH_), intent(in) :: initials(:)
       character(kind=CH_),                  intent(in) :: genders(:)
       character(kind=CH_),                  intent(in) :: registrations(:)
       real(R_),                             intent(in) :: avg_marks(:)
       character(kind=CH_),                  intent(in) :: registration
 
       character(surname_length,  kind=CH_), allocatable, intent(out) :: surnames_registration(:)
       character(initials_length, kind=CH_), allocatable, intent(out) :: initials_registration(:)
       character(kind=CH_),                  allocatable, intent(out) :: genders_registration(:)
       real(R_),                             allocatable, intent(out) :: avg_marks_registration(:)
 
       logical, allocatable :: is_registration(:)
       integer, allocatable :: position_registration(:)
       integer(I_)          :: registration_count, i
       integer, parameter   :: indexes(*) = [(i, i = 1, students_count)]
 
       is_registration = registrations == registration
       registration_count = Count(is_registration)
 
       position_registration = Pack(indexes, is_registration)
       allocate(surnames_registration(registration_count), &
          initials_registration(registration_count), &
          genders_registration(registration_count), &
          avg_marks_registration(registration_count))
 
       do concurrent(i = 1:registration_count)
          surnames_registration(i)  = surnames(position_registration(i))
          initials_registration(i)  = initials(position_registration(i))
          genders_registration(i)   = genders(position_registration(i))
          avg_marks_registration(i) = avg_marks(i)
       end do
    end subroutine get_list_by_registration
 
    pure subroutine sort_students_list(surnames, initials, genders, avg_marks)
       character(surname_length,  kind=CH_), intent(inout) :: surnames(:)
       character(initials_length, kind=CH_), intent(inout) :: initials(:)
       character(kind=CH_),                  intent(inout) :: genders(:)
       real(R_),                             intent(inout) :: avg_marks(:)
 
       integer(I_) :: i, j
 
       do i = Size(avg_marks), 2, -1
          do j = 1, i-1
             if(should_swap(avg_marks, surnames, initials, j)) &
                call swap(surnames, initials, genders, avg_marks, j)
          end do
       end do
    end subroutine sort_students_list
 
    pure logical function should_swap(avg_marks, surnames, initials, j)
       real(R_),                             intent(in) :: avg_marks(:)
       character(surname_length,  kind=CH_), intent(in) :: surnames(:)
       character(initials_length, kind=CH_), intent(in) :: initials(:)
       integer(I_),                          intent(in) :: j
 
       should_swap = .false.
       ! Проверка на то, стоит ли менять учащихся местами.
       if(avg_marks(j) < avg_marks(j+1)) then
          should_swap = .true.
       else if(avg_marks(j) == avg_marks(j+1)) then
          if(surnames(j) > surnames(j+1)) then
             should_swap = .true.
          else if(surnames(j) == surnames(j+1) &
             .and. initials(j) > initials(j+1)) then
             should_swap = .true.
          end if
       end if
    end function should_swap
 
    pure subroutine swap(surnames, initials, genders, avg_marks, j)
       character(surname_length, kind=CH_),  intent(inout) :: surnames(:)
       character(initials_length, kind=CH_), intent(inout) :: initials(:)
       character(kind=CH_),                  intent(inout) :: genders(:)
       real(R_),                             intent(inout) :: avg_marks(:)
       integer(I_),                          intent(in)    :: j
 
       character(surname_length,  kind=CH_) :: surname_tmp
       character(initials_length, kind=CH_) :: initials_tmp
       character(kind=CH_)                  :: gender_tmp
       real(R_)                             :: avg_mark_tmp
 
       surname_tmp    = surnames(j+1)
       surnames(j+1)  = surnames(j)
       surnames(j)    = surname_tmp
 
       initials_tmp   = initials(j+1)
       initials(j+1)  = initials(j)
       initials(j)    = initials_tmp
 
       gender_tmp     = genders(j+1)
       genders(j+1)   = genders(j)
       genders(j)     = gender_tmp
 
       avg_mark_tmp   = avg_marks(j+1)
       avg_marks(j+1) = avg_marks(j)
       avg_marks(j)   = avg_mark_tmp
    end subroutine swap
 end program sort_students
 