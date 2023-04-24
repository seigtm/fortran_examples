module ll_io
   use environment
   implicit none

   ! Структура данных для узла списка.
   type node
      character(kind=CH_)     :: value = ''
      type(node), allocatable :: next
   end type node

contains
   ! Чтение списка.
   function read_list(input_file) result(list)
      type(node),   allocatable :: list
      character(*), intent(in)  :: input_file
      integer(I_)               :: in

      open(file=input_file, newunit=in, encoding=E_)
      call read_value(in, list)
      close(in)
   end function read_list

   ! Чтение следующего значения.
   recursive subroutine read_value(in, elem)
      type(node), allocatable :: elem
      integer, intent(in)     :: in
      integer                 :: io

      allocate(elem)
      read(in, '(a1)', iostat=io, advance='no') elem%value
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
      type(node),   allocatable :: list
      integer                   :: out

      open(file=output_file, position=position, newunit=out)
      write(out, '(/a)') list_name
      call output_value(out, list)
      close(out)
   end subroutine output_list

   recursive subroutine output_value(out, elem)
      integer,    intent(in)  :: out
      type(node), allocatable :: elem
      integer                 :: io

      if(allocated(elem)) then
         write(out, '(a1)', advance='no', iostat=io) elem%value
         call handle_io_status(io, "writing list")
         call output_value(out, elem%next)
      end if
   end subroutine output_value

   subroutine output_result(output_file, message, position)
      character(*), intent(in) :: output_file, message, position
      integer                  :: out

      open(file=output_file, position=position, newunit=out)
      write(out, '(/a/)') message
      close(out)
   end subroutine output_result
end module ll_io
