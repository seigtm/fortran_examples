module group_process
   use environment
   use group_io
   implicit none

contains
   pure subroutine get_list_by_registration(surnames, initials, genders, registrations, avg_marks, &
      surnames_registration, initials_registration, genders_registration, avg_marks_registration, registration)
      character(kind=CH_),              intent(in)  :: surnames(:, :), initials(:, :), genders(:), registrations(:)
      real(R_),                         intent(in)  :: avg_marks(:)
      character(kind=CH_), allocatable, intent(out) :: surnames_registration(:, :), &
         initials_registration(:, :), &
         genders_registration(:)
      real(R_),            allocatable, intent(out) :: avg_marks_registration(:)
      character(kind=CH_),              intent(in)  :: registration

      logical, allocatable :: is_registration(:)
      integer, allocatable :: position_registration(:)
      integer(I_)          :: registration_count, i
      integer, parameter   :: indexes(*) = [(i, i = 1, students_count)]

      is_registration = registrations == registration
      registration_count = Count(is_registration)

      position_registration = Pack(indexes, is_registration)
      allocate(surnames_registration(registration_count, surname_length), &
         initials_registration(registration_count, initials_length), &
         genders_registration(registration_count), &
         avg_marks_registration(registration_count))

      do concurrent(i = 1:registration_count)
         surnames_registration(i, :) = surnames(position_registration(i), :)
         initials_registration(i, :) = initials(position_registration(i), :)
         genders_registration(i)     =  genders(position_registration(i))
         avg_marks_registration(i)   = avg_marks(i)
      end do
   end subroutine get_list_by_registration

   pure subroutine sort_students_list(surnames, initials, genders, avg_marks)
      character(kind=CH_), intent(inout) :: surnames(:, :), initials(:, :), genders(:)
      real(R_),            intent(inout) :: avg_marks(:)
      integer(I_)                        :: i, j

      do i = Size(avg_marks), 2, -1
         do j = 1, i-1
            if(should_swap(avg_marks, surnames, initials, j)) &
               call swap(surnames, initials, genders, avg_marks, j)
         end do
      end do
   end subroutine sort_students_list

   pure logical function should_swap(avg_marks, surnames, initials, j)
      character(kind=CH_), intent(in) :: surnames(:, :), initials(:, :)
      real(R_),            intent(in) :: avg_marks(:)
      integer(I_),         intent(in) :: j

      should_swap = .false.
      ! Проверка на то, стоит ли менять учащихся местами.
      if(avg_marks(j) < avg_marks(j+1)) then
         should_swap = .true.
      else if(avg_marks(j) == avg_marks(j+1)) then
         if (GT(surnames(j, :), surnames(j+1, :))) then
            should_swap = .true.
         else if(All(surnames(j, :) == surnames(j+1, :)) &
            .and. GT(initials(j, :), initials(j+1, :))) then
            should_swap = .true.
         end if
      end if
   end function should_swap

   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)
      integer                         :: i

      ! Поиск первого отличного символа или остановка на последнем символе.
      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      GT = arr1(i) > arr2(i)
   end function GT

   pure subroutine swap(surnames, initials, genders, avg_marks, j)
      character(kind=CH_), intent(inout) :: surnames(:, :), initials(:, :), genders(:)
      real(R_),            intent(inout) :: avg_marks(:)
      integer,             intent(in)    :: j
      character(kind=CH_)                :: surname_tmp(surname_length), &
         initials_tmp(initials_length), &
         genders_tmp
      real(R_)                           :: avg_mark_tmp

      surname_tmp = surnames(j+1, :)
      surnames(j+1, :) = surnames(j, :)
      surnames(j, :) = surname_tmp

      initials_tmp = initials(j+1, :)
      initials(j+1, :) = initials(j, :)
      initials(j, :) = initials_tmp

      genders_tmp = genders(j+1)
      genders(j+1) = genders(j)
      genders(j) = genders_tmp

      avg_mark_tmp = avg_marks(j+1)
      avg_marks(j+1) = avg_marks(j)
      avg_marks(j) = avg_mark_tmp
   end subroutine swap
end module group_process
