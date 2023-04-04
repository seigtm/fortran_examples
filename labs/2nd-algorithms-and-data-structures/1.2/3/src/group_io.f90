module group_io
   use environment
   implicit none

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

      open(file=input_file, encoding=E_, newunit=in)
      record_length = (surname_length + initials_length + 2)*CH_ + R_
      open(file=data_file, form='unformatted', newunit=out, access='direct', recl=record_length)
         format = '(4(a, 1x), f5.2)'
         do i = 1, students_count
            read(in, format, iostat=io) stud
            call handle_io_status(io, "reading formatted class list, line " // i)
            write(out, iostat=io, rec=i) stud
            call handle_io_status(io, "creating unformatted file with class list, record " // i)
         end do
      close(in)
      close(out)
   end subroutine create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы, прописки и оценки.
   function read_students_list(data_file) result(group)
      type(student)            :: group(students_count)
      character(*), intent(in) :: data_file
      integer                  :: in, io, record_length
      
      record_length = ((surname_length + initials_length + 2)*CH_ + R_) * students_count
      open(file=data_file, form='unformatted', newunit=in, access='direct', recl=record_length)
         read(in, iostat=io, rec=1) group
         call handle_io_status(io, "reading unformatted class list")
      close(in)
   end function read_students_list
 
   ! Вывод списка класса.
   subroutine output_students_list(output_file, group, list_name, position)
      character(*),  intent(in) :: output_file, list_name, position
      type(student), intent(in) :: group(:)
      integer                   :: out, io
      character(:), allocatable :: format

      open (file=output_file, encoding=E_, position=position, newunit=out)
         write(out, '(/a)') list_name
         format = '(4(a, 1x), f5.2)'
         write(out, format, iostat=io) group
         call handle_io_status(io, "writing " // list_name)
      close (out)
   end subroutine output_students_list
end module group_io
