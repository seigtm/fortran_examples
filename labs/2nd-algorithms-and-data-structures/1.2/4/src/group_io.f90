module group_io
   use environment
   implicit none

   ! Определение параметров для работы с данными о студентах.
   integer, parameter :: students_count = 18, surname_length = 15, initials_length = 5
   ! Структура данных для хранения данных о студентах.
   type students
      character(surname_length,  kind=CH_) :: surname     (students_count)
      character(initials_length, kind=CH_) :: initials    (students_count)
      character(kind=CH_)                  :: sex         (students_count)
      character(kind=CH_)                  :: registration(students_count)
      real(R_)                             :: avg_mark    (students_count)
   end type students

contains
   ! Создание неформатированного файла данных.
   subroutine create_data_file(input_file, data_file)
      character(*), intent(in)  :: input_file, data_file
      integer(I_)               :: in, out, io, i
      character(:), allocatable :: format
      type(students)            :: group

      open(file=input_file, encoding=E_, newunit=in)
      format = '(4(a, 1x), f5.2)'  ! Формат для чтения из файла.
      read(in, format, iostat=io) (group%surname(i), group%initials(i), group%sex(i), &
        group%registration(i), group%avg_mark(i), i = 1, students_count)
      call handle_io_status(io, "reading formatted class list, line " // i)
      close(in)

      open(file=data_file, form='unformatted', newunit=out, access='stream')
      write(out, iostat=io) group%surname, group%initials, group%sex, group%registration, group%avg_mark
      call handle_io_status(io, "creating unformatted file with class list")
      close(out)
   end subroutine create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы, прописки и оценки.
   type(students) function read_students_list(data_file) result(group)
      character(*), intent(in) :: data_file
      integer(I_)              :: in, io

      open(file=data_file, form='unformatted', newunit=in, access='stream')
      read(in, iostat=io) group%surname,      &
                          group%initials,     &
                          group%sex,          &
                          group%registration, &
                          group%avg_mark
      call handle_io_status(io, "reading unformatted class list")
      close(in)
   end function read_students_list

   ! Вывод списка класса.
   subroutine output_students_list(output_file, group, list_name, position, count)
      character(*),   intent(in) :: output_file, list_name, position  ! Имена файла и списка, позиция.
      type(students), intent(in) :: group
      integer(I_),    intent(in) :: count
      integer(I_)                :: out, io, i
      character(:), allocatable  :: format

      ! Открытие файла на запись с указанием позиции курсора в файле.
      open(file=output_file, encoding=E_, position=position, newunit=out)
      ! Запись названия списка в файл.
      write(out, '(/a)') list_name
      ! Определение формата вывода данных о студентах.
      format = '(4(a, 1x), f5.2)'
      ! Запись данных о студентах в файл в соответствии с заданным форматом.
      write(out, format, iostat=io) (group%surname(i),      &
                                     group%initials(i),     &
                                     group%sex(i),          &
                                     group%registration(i), &
                                     group%avg_mark(i),     &
                                     i = 1, count)
      call handle_io_status(io, "writing " // list_name)
      close(out)
   end subroutine output_students_list
end module group_io
