! Copyright 2015 Fyodorov S. A.
  
program reference_lab_1_4
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   
   type(student)              :: Group(STUD_AMOUNT)
   type(student), allocatable :: Boys(:), Girls(:)
   integer                    :: i ! По стандарту можно заводить прямо в do concurrent.

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Boys  = Pack(Group, Group%Sex == MALE)
   Girls = Pack(Group, Group%Sex == FEMALE)

   ! Вычисление средней оценки для каждого юноши.
   do concurrent (i = 1:Size(Boys))
      Boys(i)%Aver_mark = Real(Sum(Boys(i)%Marks), R_) / MARKS_AMOUNT
   end do
   ! Переменную i можно заводить прямо тут.
   !do concurrent (integer :: i = 1:Size(Boys))
   
   ! Вычисление средней оценки для каждой левушки.
   do concurrent (i = 1:Size(Girls))
      Girls(i)%Aver_mark = Real(Sum(Girls(i)%Marks), R_) / MARKS_AMOUNT
   end do
   
   call Sort_class_list(Boys, Size(Boys))
   call Sort_class_list(Girls, Size(Girls))

   call Output_class_list(output_file, Boys, "Успеваемость юношей:", "append")
   call Output_class_list(output_file, Girls, "Успеваемость девушек:", "append")

end program reference_lab_1_4
