! Copyright 2015 Fyodorov S. A

program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_) !CH__"\u1052" CH__"М"

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, инициалов, полов, оценок и средних оценов и временные
   ! переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: Boys_Surnames(:), Girls_Surnames(:)
   
   character(INITIALS_LEN, kind=CH_)               :: tmpInitials = "", Initials(STUD_AMOUNT) = ""
   character(INITIALS_LEN, kind=CH_), allocatable  :: Boys_Initials(:), Girls_Initials(:)
   
   character(kind=CH_)                             :: Gender(STUD_AMOUNT) = ""
   
   integer                                         :: tmpMarks(MARKS_AMOUNT) = 0, Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
   integer, allocatable                            :: Boys_Marks(:, :), Girls_Marks(:, :), Boys_Pos(:), Girls_Pos(:)
   
   real(R_)                                        :: tmpAverMark = 0, Aver_Marks(STUD_AMOUNT) = 0
   real(R_), allocatable                           :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)

   logical, allocatable                            :: Is_A_Boy(:), Is_A_Girl(:)
   integer                                         :: Boys_Amount = 0, Girls_Amount = 0

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   input_file = "../data/class.txt"
   output_file = "output.txt"
   ! Чтение списка класса: фамилии, инициалы, полы, оценки и средний.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
      read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (In)
   ! ПОЯСНЕНИЯ К СПОСОБАМ ЧТЕНИЯ:
   ! 1. можно программировать форматный список, не приравнивая к нему, а записывая в него -- так менее наглядно:
   !write (format, '(a, i0, a)') '(3(a, 1x), ', MARKS_AMOUNT, 'i1, f5.2)'
   !read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), i = 1, STUD_AMOUNT)
   !
   ! 2. Можно проводить запись без использования неявного цикла -- так не профессионально (больше ошибок):
   !do i = 1, STUD_AMOUNT
   !   read (In, format, iostat=IO) surnames(i), Initials(i), Gender(i), Marks(i, :)
   !end do
   !
   ! 3. Можно записать без использования именованной константы -- так не профессионально (сложнее поддерживать код):
   ! поступать:
   !read (In, '(3(a, 1x), 5i1, f5.2)', iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), i = 1, STUD_AMOUNT)
   !
   ! 4. Можно записать без использования неявного цикла и констант -- так не профессионально (больше ошибок и сложнее поддерживать код):
   !do i = 1, STUD_AMOUNT
   !   read (In, '(3(a, 1x), 5i1, f5.2 )', iostat=IO) Surnames(i), Initials(i), Gender(i), Marks(i, :) 
   !end do
   
   ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", IO
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Вывод списка класса.
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      ! Пояснения к записи те же, что и к чтению.
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

   ! Составление логической маски, соответствующей юношам.
   Is_A_Boy       = Gender == MALE ! Gender == CH__"М" в некоторых компиляторах может пока не поддерживаться.
   Boys_Amount    = Count(Is_A_Boy)
   
   ! Получение массивов, связынных с юношами.
   ! 1-ый способ. Использование массива номеров юношей в списке.
   Boys_Pos   = Pack(INDEXES, Is_A_Boy) ! == [1, 2, 3]
   allocate (Boys_Surnames(Boys_Amount), Boys_Initials(Boys_Amount), &
      Boys_Marks(Boys_Amount, MARKS_AMOUNT))
   do concurrent (i = 1:Boys_Amount)
      ! Получение списков юношей.
      Boys_Surnames(i)  = Surnames(Boys_Pos(i))
      Boys_Initials(i)  = Initials(Boys_Pos(i))
      Boys_Marks(i, :)  = Marks(Boys_Pos(i), :)
   end do
   
   ! 2-ой способ. Использование двумерной маски, накладываемой на массив оценок.
   ! ! Получение списков юношей.
   ! Boys_Surnames  = Pack(Surnames, Is_A_Boy)
   ! Boys_Initials  = Pack(Initials, Is_A_Boy)
   ! ! Получение двумерного списка оценок юношей:
   ! ! 1. Для получение двумерного списка оценок необходима двумерная маска, захватывающая все оценки юношей:
   ! ! Spread(Is_A_Boy, 2, MARKS_AMOUNT)
   ! ! 2. По такой маске Pack вернёт одномерный массив со всеми оценками юношей, который
   ! ! необходимо будет переформировать в двумерный массив размером: [Boys_amount, MARKS_AMOUNT].
   ! Boys_Marks     = Reshape( Pack(Marks, Spread(Is_A_Boy, 2, MARKS_AMOUNT)), [Boys_amount, MARKS_AMOUNT])
   
   ! Вычисление средней оценки для юношей. Вне цикла для векторизации.
   Boys_Aver_Marks   = Sum(Boys_Marks, dim=2) / Real(MARKS_AMOUNT, R_)

   Is_A_Girl      = .not. Is_A_Boy
   Girls_Amount   = STUD_AMOUNT - Boys_Amount
   
   ! Получение массивов, связынных с девушками.
   Girls_Pos   = Pack(INDEXES, Is_A_Girl) ! == [4, 5]
   allocate (Girls_Surnames(Girls_Amount), Girls_Initials(Girls_Amount), &
      Girls_Marks(Girls_Amount, MARKS_AMOUNT))
   do concurrent (i = 1:Girls_Amount)
      ! Получение списков девушек.
      Girls_Surnames(i)  = Surnames(Girls_Pos(i))
      Girls_Initials(i)  = Initials(Girls_Pos(i))
      Girls_Marks(i, :)  = Marks(Girls_Pos(i), :)
   end do
      
   ! Вычисление средней оценки для девушек. Вне цикла для векторизации.
   Girls_Aver_Marks = Sum(Girls_Marks, dim=2) / Real(MARKS_AMOUNT, R_)

   ! Сортировка списка юношей по среднему баллу методом пузырька.
   do i = Boys_amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Boys_Aver_Marks(j) < Boys_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Boys_Aver_Marks(j) == Boys_Aver_Marks(j+1)) then
            if (Boys_Surnames(j) > Boys_Surnames(j+1)) then
               Swap = .true.
            else if (Boys_Surnames(j)==Boys_Surnames(j+1) .and. Boys_Initials(j)>Boys_Initials(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname           = Boys_Surnames(j+1)
            Boys_Surnames(j+1)   = Boys_Surnames(j)
            Boys_Surnames(j)     = tmpSurname
			! Boys_Surnames(j+1:j:-1) = Boys_Surnames(j:j+1)
			! Boys_Surnames([j, j+1]) = Boys_Surnames([j+1, j])

            tmpInitials          = Boys_Initials(j+1)
            Boys_Initials(j+1)   = Boys_Initials(j)
            Boys_Initials(j)     = tmpInitials

            tmpMarks             = Boys_Marks(j+1, :)
            Boys_Marks(j+1, :)   = Boys_Marks(j, :)
            Boys_Marks(j, :)     = tmpMarks

            tmpAverMark          = Boys_Aver_Marks(j+1)
            Boys_Aver_Marks(j+1) = Boys_Aver_Marks(j)
            Boys_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Сортировка списка девушек по среднему баллу методом пузырька.
   do i = Girls_Amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Girls_Aver_Marks(j) < Girls_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Girls_Aver_Marks(j) == Girls_Aver_Marks(j+1)) then
            if (Girls_Surnames(j) > Girls_Surnames(j+1)) then
               Swap = .true.
            else if (Girls_Surnames(j)==Girls_Surnames(j+1) .and. Girls_Initials(j)>Girls_Initials(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname           = Girls_Surnames(j+1)
            Girls_Surnames(j+1)   = Girls_Surnames(j)
            Girls_Surnames(j)     = tmpSurname

            tmpInitials          = Girls_Initials(j+1)
            Girls_Initials(j+1)   = Girls_Initials(j)
            Girls_Initials(j)     = tmpInitials

            tmpMarks             = Girls_Marks(j+1, :)
            Girls_Marks(j+1, :)   = Girls_Marks(j, :)
            Girls_Marks(j, :)     = tmpMarks

            tmpAverMark          = Girls_Aver_Marks(j+1)
            Girls_Aver_Marks(j+1) = Girls_Aver_Marks(j)
            Girls_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Вывод отсортированного списка юношей со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость юношей:"
      write (Out, format, iostat=IO) &
         (Boys_Surnames(i), Boys_Initials(i), "М", Boys_Marks(i, :), Boys_Aver_Marks(i), i = 1, Boys_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select
   
      ! Вывод отсортированного списка девушек со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость девушек:"
      write (Out, format, iostat=IO) (Girls_Surnames(i), Girls_Initials(i), "Ж", Girls_Marks(i, :), &
         Girls_Aver_Marks(i), i = 1, Girls_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted girls list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted girls list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
   end select

end program reference_lab_1_1
