module group_io
   use environment
   implicit none

   ! Определение параметров для работы с данными о студентах.
   integer, parameter :: students_count = 18, surname_length = 15, initials_length = 5
   ! Структура данных для хранения данных о студенте.
   type student
      character(surname_length,  kind=CH_) :: surname      = ""
      character(initials_length, kind=CH_) :: initials     = ""
      character(kind=CH_)                  :: sex          = ""
      character(kind=CH_)                  :: registration = ""
      real(R_)                             :: avg_mark     = 0
      type(student), pointer               :: next         => Null()
   end type student

contains
   ! Чтение списка класса: фамилии, инициалы, полы, прописки и оценки.
   function read_students_list(input_file) result(group)
      character(*), intent(in) :: input_file
      type(student), pointer   :: group
      integer                  :: in

      open(file=input_file, encoding=E_, newunit=in)
         group => read_student(in)
      close(in)
   end function read_students_list

   ! Чтение следующего студента.
   recursive function read_student(in) result(stud)
      type(student), pointer    :: stud
      integer, intent(in)       :: in
      integer                   :: io
      character(:), allocatable :: format

      allocate(stud)
      format = '(4(a, 1x), f5.2)'
      read(in, format, iostat=io) stud%surname, stud%initials, stud%sex, stud%registration, stud%avg_mark
      call handle_io_status(io, "reading line from file")
      if(io == 0) then
          stud%next => read_student(In)
      else
         deallocate(stud)
      end if
   end function read_student

   ! Вывод списка класса.
   subroutine output_students_list(output_file, group, list_name, position)
      character(*),  intent(in) :: output_file, list_name, position  ! Имена файла и списка, позиция.
      type(student), intent(in) :: group
      integer                   :: out

      ! Открытие файла на запись с указанием позиции курсора в файле.
      open(file=output_file, encoding=E_, position=position, newunit=out)
      ! Запись названия списка в файл.
      write(out, '(/a)') list_name
      call output_student(out, group)
      close(out)
   end subroutine output_students_list

   recursive subroutine output_student(out, stud)
      integer,       intent(in) :: out
      type(student), intent(in) :: stud
      integer                   :: io
      character(:), allocatable :: format

      ! Определение формата вывода данных о студенте.
      format = '(4(a, 1x), f5.2)'
      ! Запись данных о студенте в файл в соответствии с заданным форматом.
      write(out, format, iostat=io) stud%surname, stud%initials, stud%sex, stud%registration, stud%avg_mark
      ! Проверка статуса операции записи в файл.
      call handle_io_status(io, "writing student")
      if(associated(stud%next)) &
         call output_student(out, stud%next)
      ! Закрытие файла.
      close(out)
   end subroutine output_student
end module group_io
