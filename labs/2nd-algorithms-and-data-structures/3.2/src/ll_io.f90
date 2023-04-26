module ll_io
   use environment
   implicit none

   integer(I_),  parameter :: last_name_length = 10
   integer(I_),  parameter :: record_length    = last_name_length * CH_ + I_
   character(*), parameter :: person_format    = '(a, 1x, i4)'
   ! Структура данных для узла в списке людей.
   type person_t
      character(last_name_length, kind=CH_) :: last_name  = ''
      integer(I_)                           :: birth_year = 0
   end type person_t

   type, extends(person_t) :: person_node_t
      type(person_node_t), allocatable :: next
   end type person_node_t

contains
   ! Чтение списка.
   function read_list(input_file) result(list)
      type(person_node_t), allocatable :: list
      character(*),        intent(in)  :: input_file
      integer(I_)                      :: in

      open(file=input_file, newunit=in, encoding=E_)
      call read_value(in, list)
      close(in)
   end function read_list

   ! Чтение следующего значения.
   recursive subroutine read_value(in, elem)
      type(person_node_t), allocatable :: elem
      integer,             intent(in)  :: in
      integer                          :: io

      allocate(elem)
      read(in, person_format, iostat=io) &
         elem%last_name, elem%birth_year
      call handle_io_status(io, "reading value from file")
      if(io == 0) then
         call read_value(in, elem%next)
      else
         deallocate(elem)
      end if
   end subroutine read_value

   ! Создание неформатированного файла.
   subroutine create_data_file(list, data_file)
      character(*),        intent(in)  :: data_file
      type(person_node_t), allocatable :: list
      integer(I_)                      :: out

      open(file=data_file, form='unformatted', newunit=out, access='direct', recl=record_length)
      call write_element(out, list, 1)
      close(out)
   end subroutine create_data_file

   recursive subroutine write_element(out, list, i)
      integer,             intent(in) :: out, i
      type(person_node_t), intent(in) :: list
      integer(I_)                     :: io

      write(out, iostat=io, rec=i) list%last_name, list%birth_year
      call handle_io_status(io, "writing data")
      if(allocated(list%next)) &
         call write_element(out, list%next, i+1)
   end subroutine write_element

   ! Вывод списка.
   subroutine output(output_file, data_file, list_name)
      character(*), intent(in) :: output_file, data_file, list_name
      type(person_t)           :: elem
      integer(I_)              :: in, out, io, size, i

      open(file=data_file, form='unformatted', newunit=in, access='stream')
      inquire(unit=in, size=size)  ! Получаем размер файла.

      open(file=output_file, encoding=E_, position='rewind', newunit=out)
      write(out, '(a/)') list_name

      do i=1, size / record_length
         read(in, iostat=io) elem
         call handle_io_status(io, "reading line from the file")
         write(out, person_format, iostat=io) elem
         call handle_io_status(io, "writing line into the file")
      end do

      close(out)
      close(in)
   end subroutine output

   recursive subroutine deallocate_list(list)
      type(person_node_t), allocatable :: list

      if(allocated(list%next)) &
        call deallocate_list(list%next)
      deallocate(list)
    end subroutine deallocate_list
end module ll_io
