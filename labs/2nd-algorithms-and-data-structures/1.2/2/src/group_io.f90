module group_io
   use environment
   implicit none

   integer, parameter :: students_count = 25500, surname_length = 15, initials_length = 5

contains
   subroutine read_students_list(input_file, surnames, initials, genders, registrations, avg_marks)
      character(*),        intent(in)  :: input_file
      character(kind=CH_), intent(out) :: surnames(:, :), initials(:, :), genders(:), registrations(:)
      real(R_),            intent(out) :: avg_marks(:)
      integer(I_)                      :: in, io, i
      character(:), allocatable        :: format

      ! Чтение списка класса: фамилии, инициалы, пол, прописка и средний балл.
      open(file=input_file, encoding=E_, newunit=in)
      format = '(' // surname_length // 'a1, 1x, ' // initials_length // 'a1, 1x, a, 1x, a, 1x, f5.2)'
      read(in, format, iostat=io) (surnames(i, :), initials(i, :), genders(i), registrations(i), avg_marks(i), &
         i = 1, students_count)
      call handle_io_status(io, "reading class list")
      close(in)
   end subroutine read_students_list

   subroutine output_students_list(output_file, surnames, initials, genders, registrations, avg_marks, list_name, position)
      character(*),        intent(in) :: output_file, list_name, position
      character(kind=CH_), intent(in) :: surnames(:, :), initials(:, :), genders(:), registrations(:)
      real(R_),            intent(in) :: avg_marks(:)
      integer(I_)                     :: out, io, i
      character(:), allocatable       :: format

      ! Чтение списка класса: фамилии, инициалы, пол, прописка и средний балл.
      open(file=output_file, encoding=E_, position=position, newunit=out)
      write(out, '(/a)') list_name
      format = '(' // surname_length // 'a1, 1x, ' // initials_length // 'a1, 1x, a, 1x, a, 1x, f5.2)'
      write(out, format, iostat=io) (surnames(i, :), initials(i, :), genders(i), registrations(i), avg_marks(i), &
         i = 1, UBound(avg_marks, 1))
      call handle_io_status(io, "writing " // list_name)
      close(out)
   end subroutine output_students_list
end module group_io
