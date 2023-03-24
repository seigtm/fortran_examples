! Copyright 2015 Fyodorov S. A.

program reference_lab_1_2
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   character(:), allocatable        :: input_file, output_file

   ! Массивы фамилий, инициалов, полов, оценок и средних оценов.
   character(kind=CH_)  :: Surnames(STUD_AMOUNT, SURNAME_LEN)  = "", &
                           Initials(STUD_AMOUNT, INITIALS_LEN) = "", &
                           Genders(STUD_AMOUNT)                   = ""
   character(kind=CH_), allocatable :: Boys_Surnames(:, :), Girls_Surnames(:, :)
   character(kind=CH_), allocatable :: Boys_Initials(:, :), Girls_Initials(:, :)
   integer              :: Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0, i = 0
   integer, allocatable :: Boys_Marks(:, :), Girls_Marks(:, :)
   real(R_)             :: Aver_Marks(STUD_AMOUNT) = 0
   real(R_),allocatable :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file, Surnames, Initials, Genders, Marks, Aver_Marks)

   call Output_class_list(output_file, Surnames, Initials, Genders, Marks, Aver_Marks, &
      "Исходный список:", "rewind")
      
   call Get_list_by_gender(Surnames, Initials, Genders, Marks, Boys_Surnames, &
      Boys_Initials, Boys_Marks, Boys_Aver_Marks, MALE)
   call Get_list_by_gender(Surnames, Initials, Genders, Marks, Girls_Surnames, &
      Girls_Initials, Girls_Marks, Girls_Aver_Marks, FEMALE)
   ! Если сразу сортировать:
   ! N -- размер массива
   ! C*N^2 + A*N + B ~ O(N^2) -- вычислительная сложность
   ! Если сортировать после разделения по полу:
   ! N/2 -- размер каждого массива
   ! C*(N/2)^2 + A*(N/2) + B + C*(N/2)^2 + A*(N/2) + B =
   ! = C/2*N^2 + A*N + 2*B ~ O(N^2) -- вычислительная сложность
   !
   call Sort_class_list(Boys_Surnames, Boys_Initials, Boys_Marks, Boys_Aver_Marks)
   call Sort_class_list(Girls_Surnames, Girls_Initials, Girls_Marks, Girls_Aver_Marks)

   call Output_class_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i = 1, Size(Boys_Aver_Marks))], &
      Boys_Marks, Boys_Aver_Marks, "Успеваемость юношей:", "append")
   call Output_class_list(output_file, Girls_Surnames, Girls_Initials, [(FEMALE, i = 1, Size(Girls_Aver_Marks))], &
      Girls_Marks, Girls_Aver_Marks, "Успеваемость девушек:", "append")
contains
   ! Чтение списка класса: фамилии, инициалы, полы, оценки и средний.
   subroutine Read_class_list(Input_File, Surnames, Initials, Genders, Marks, Aver_Marks)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      integer              Marks(:, :)
      real(R_)             Aver_Marks(:)
      intent (in)          Input_File
      intent (out)         Surnames, Initials, Genders, Marks, Aver_Marks

      integer In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, ' // &
            MARKS_AMOUNT // 'i1, f5.2)'
         read (In, format, iostat=IO) (Surnames(i, :), Initials(i, :), Genders(i), Marks(i, :), Aver_Marks(i), &
            i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list

   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Surnames, Initials, Genders, Marks, Aver_Marks, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      integer              Marks(:, :)
      real(R_)             Aver_Marks(:)
      intent (in)          Output_File, Surnames, Initials, Genders, Marks, Aver_Marks, List_name, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, ' // &
            MARKS_AMOUNT // 'i1, f5.2)'
         write (Out, format, iostat=IO) &
            (Surnames(i, :), Initials(i, :), Genders(i), Marks(i, :), Aver_Marks(i), i = 1, UBound(Aver_Marks, 1))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list

   ! Получение списков по полу.
   pure subroutine Get_list_by_gender(Surnames, Initials, Genders, Marks, &
         Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks, Gender)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      integer              Marks(:, :)
      character(kind=CH_)  Gender_Surnames(:, :), Gender_Initials(:, :)
      integer              Gender_Marks(:, :)
      real(R_)             Gender_Aver_Marks(:)
      character(kind=CH_)  Gender
      intent(in)           Surnames, Initials, Genders, Marks, Gender
      intent(out)          Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks
      allocatable          Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks

      logical, allocatable :: Is_A_Gender(:)
      integer, allocatable :: Gender_Pos(:)
      integer              :: Gender_Amount, i
      integer, parameter   :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

      ! Составление логической маски, соответствующей полу.
      Is_A_Gender    = Genders == Gender
      Gender_Amount  = Count(Is_A_Gender)

      ! Получение массивов, связынных с заданным полом.
      Gender_Pos  = Pack(INDEXES, Is_A_Gender)
      allocate (Gender_Surnames(Gender_Amount, SURNAME_LEN), &
         Gender_Initials(Gender_Amount, INITIALS_LEN), Gender_Marks(Gender_Amount, MARKS_AMOUNT))
      ! Получение двумерных списков для пола.
      do concurrent (i = 1:Gender_Amount)
         Gender_Surnames(i, :)  = Surnames(Gender_Pos(i), :)
         Gender_Initials(i, :)  = Initials(Gender_Pos(i), :)
         Gender_Marks(i, :)  = Marks(Gender_Pos(i), :)
      end do

      ! Вычисление средней оценки для пола. Вне цикла для векторизации.
      Gender_Aver_Marks   = Sum(Gender_Marks, dim=2) / Real(MARKS_AMOUNT, R_)
   end subroutine Get_list_by_gender

   ! Сортировка списка класса по среднему баллу.
   pure subroutine Sort_class_list(Surnames, Initials, Marks, Aver_Marks)
      character(kind=CH_)  Surnames(:, :), Initials(:, :)
      integer              Marks(:, :)
      real(R_)             Aver_Marks(:)
      intent (inout)       Surnames, Initials, Marks, Aver_Marks

      integer              i, j
      
      ! Сортировка списка класса по среднему баллу методом пузырька.
      do i = Size(Aver_Marks), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять учащихся местами.
            if (Swap(Aver_Marks, Surnames, Initials, j)) &
               call Swap_from_current(Surnames, Initials, Marks, Aver_Marks, j)
         end do
      end do
   end subroutine Sort_class_list
      
   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Aver_Marks, Surnames, Initials, j)
      character(kind=CH_)  Surnames(:, :), Initials(:, :)
      real(R_)             Aver_Marks(:)
      integer              j
      intent (in) :: Surnames, Initials, Aver_Marks, j

      Swap = .false.
      if (Aver_Marks(j) < Aver_Marks(j+1)) then
         Swap = .true.
      else if (Aver_Marks(j) == Aver_Marks(j+1)) then
         if (GT(Surnames(j, :), Surnames(j+1, :))) then
            Swap = .true.
         else if (All(Surnames(j, :) == Surnames(j+1, :)) .and. GT(Initials(j, :), Initials(j+1, :))) then
            Swap = .true.
         end if
      end if
   end function Swap
   
   ! Функция операции > для массивов символов.
   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)

      integer :: i 

      ! Поиск первого отличного символа или остановка на последнем символе.
      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      GT = arr1(i) > arr2(i)  
   end function GT
   
   ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Surnames, Initials, Marks, Aver_Marks, j)
      character(kind=CH_)     Surnames(:, :), Initials(:, :)
      integer                 Marks(:, :)
      real(R_)                Aver_Marks(:)
      integer, intent(in)  :: j
      intent (inout)       :: Surnames, Initials, Marks, Aver_Marks
      
      character(kind=CH_)  tmpSurname(SURNAME_LEN), tmpInitials(INITIALS_LEN)
      integer              tmpMarks(MARKS_AMOUNT)
      real(R_)             tmpAverMark

      tmpSurname = Surnames(j+1, :)
      Surnames(j+1, :) = Surnames(j, :)
      Surnames(j, :) = tmpSurname
	  ! Surnames([j+1, j], :) = Surnames([j, j+1], :) ! XXX
	  ! Surnames(j+1:j:-1, :) = Surnames(j:j+1, :) ! VVV

      tmpInitials = Initials(j+1, :)
      Initials(j+1, :) = Initials(j, :)
      Initials(j, :) = tmpInitials

      tmpMarks = Marks(j+1, :)
      Marks(j+1, :) = Marks(j, :)
      Marks(j, :) = tmpMarks

      tmpAverMark = Aver_Marks(j+1)
      Aver_Marks(j+1) = Aver_Marks(j)
      Aver_Marks(j) = tmpAverMark
   end subroutine Swap_from_current

end program reference_lab_1_2
