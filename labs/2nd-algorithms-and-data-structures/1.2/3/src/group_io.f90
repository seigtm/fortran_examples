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
   end type student

contains
   ! Создание неформатированного файла данных.
   subroutine create_data_file(input_file, data_file)
      character(*), intent(in)   :: input_file, data_file
      type(student)              :: stud
      integer                    :: in, out, io, i, record_length
      character(:), allocatable  :: format

      ! Открытие файла для чтения.
      open(file=input_file, encoding=E_, newunit=in)
      ! Размер записи данных в файле.
      record_length = (surname_length + initials_length + 2)*CH_ + R_
      ! Открытие файла для записи данных в неформатированном виде.
      open(file=data_file, form='unformatted', newunit=out, access='direct', recl=record_length)
      ! Формат для чтения из файла.
      format = '(4(a, 1x), f5.2)'
      ! Чтение данных из входного файла и запись в неформатированный файл.
      do i = 1, students_count
         read(in, format, iostat=io) stud
         call handle_io_status(io, "reading formatted class list, line " // i)
         write(out, iostat=io, rec=i) stud
         call handle_io_status(io, "creating unformatted file with class list, record " // i)
      end do
      ! Закрытие файлов.
      close(in)
      close(out)
   end subroutine create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы, прописки и оценки.
   function read_students_list(data_file) result(group)
      type(student)            :: group(students_count)  ! Массив студентов.
      character(*), intent(in) :: data_file              ! Имя файла.
      integer                  :: in, io, record_length

      ! Размер записи данных в файле.
      record_length = ((surname_length + initials_length + 2)*CH_ + R_) * students_count
      ! Открытие файла в двоичном режиме, с указанием размера записи и количества записей в файле.
      open(file=data_file, form='unformatted', newunit=in, access='direct', recl=record_length)
      ! Чтение записи из файла с указанным номером и сохранение ее в массиве group.
      read(in, iostat=io, rec=1) group
      ! Проверка статуса операции чтения из файл.
      call handle_io_status(io, "reading unformatted class list")
      ! Закрытие файла.
      close(in)
   end function read_students_list

   ! Вывод списка класса.
   subroutine output_students_list(output_file, group, list_name, position)
      character(*),  intent(in) :: output_file, list_name, position  ! Имена файла и списка, позиция.
      type(student), intent(in) :: group(:)  ! Массив студетов.
      integer                   :: out, io
      character(:), allocatable :: format

      ! Открытие файла на запись с указанием позиции курсора в файле.
      open(file=output_file, encoding=E_, position=position, newunit=out)
      ! Запись названия списка в файл.
      write(out, '(/a)') list_name
      ! Определение формата вывода данных о студентах.
      format = '(4(a, 1x), f5.2)'
      ! Запись данных о студентах в файл в соответствии с заданным форматом.
      write(out, format, iostat=io) group
      ! Проверка статуса операции записи в файл.
      call handle_io_status(io, "writing " // list_name)
      ! Закрытие файла.
      close(out)
   end subroutine output_students_list
end module group_io
