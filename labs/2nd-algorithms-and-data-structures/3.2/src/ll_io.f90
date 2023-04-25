module ll_io
   use environment
   implicit none

   integer(I_), parameter :: last_name_length = 10
   ! Структура данных для узла в списке людей.
   type person_node_t
      character(last_name_length, kind=CH_) :: last_name  = ''
      integer(I_)                           :: birth_year = 0
      type(person_node_t), allocatable      :: next
   end type person_node_t

contains
   ! Чтение списка.
   function read_list(input_file) result(list)
      type(person_node_t),   allocatable :: list
      character(*), intent(in)  :: input_file
      integer(I_)               :: in

      open(file=input_file, newunit=in, encoding=E_)
      call read_value(in, list)
      close(in)
   end function read_list

   ! Чтение следующего значения.
   recursive subroutine read_value(in, elem)
      type(person_node_t), allocatable :: elem
      integer, intent(in)     :: in
      integer                 :: io

      allocate(elem)
      read(in, '(a, 1x, i4)', iostat=io) &
         elem%last_name, elem%birth_year
      call handle_io_status(io, "reading value from file")
      if(io == 0) then
         call read_value(in, elem%next)
      else
         deallocate(elem)
      end if
   end subroutine read_value

   ! Вывод списка.
   subroutine output_list(output_file, list, list_name, position)
      character(*), intent(in)  :: output_file, list_name, position
      type(person_node_t),   allocatable :: list
      integer                   :: out

      open(file=output_file, position=position, newunit=out)
      write(out, '(/a)') list_name
      call output_value(out, list)
      close(out)
   end subroutine output_list

   recursive subroutine output_value(out, elem)
      integer,    intent(in)  :: out
      type(person_node_t), allocatable :: elem
      integer                 :: io

      if(allocated(elem)) then
         write(out, '(a, 1x, i4/)', advance='no', iostat=io) & 
            elem%last_name, elem%birth_year
         call handle_io_status(io, "writing list")
         call output_value(out, elem%next)
      end if
   end subroutine output_value
end module ll_io
