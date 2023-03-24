! Copyright 2015 Fyodorov S. A.

module List_IO
   use Environment

   implicit none

   ! Структура данных для узла списка.
   ! Инициализация обязательна!
   type node
      integer              :: value   = 0
      type(node), pointer  :: next  => Null()
   end type node

   ! Структура данных для узла списка.
   ! Инициализация обязательна!
   type node2
      integer              :: value   = 0
      type(node2), pointer  :: next  => Null(), prev => Null()
   end type node2

   ! Структура данных для узла списка.
   ! Инициализация обязательна!
   type sorted_node
      integer              :: value   = 0
      type(sorted_node), pointer  :: next  => Null()
      type(sorted_node), pointer  :: sorted_next => Null()
   end type sorted_node

   type node_tree 
      integer              :: value   = 0
      type(node_tree), pointer  :: left   => Null()
      type(node_tree), pointer  :: right  => Null()
   end type node_tree
contains
   function Read_tree(Input_File) result(tree)
      type(node_tree), pointer        :: tree
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
         tree => Null()
         call Read_node_tree(In, tree)
      close (In)
   end function Read_tree
   
   recursive subroutine Read_node_tree(In, tree)
      type(node_tree), pointer, intent(inout)  :: tree
      integer, intent(in)     :: In
      integer  :: IO, value = 0
     
      read (In, '(i2)', iostat=IO, advance='no') value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
         call Put_tree(value, tree)
         call Read_node_tree(In, tree)
      end if
   end subroutine Read_node_tree

   recursive subroutine Put_tree(value, current)
      type(node_tree), pointer, intent(inout)  :: current
      integer, intent(in)     :: value

      If (.not. Associated(current)) then
         allocate (current, source=node_tree(value))
         !allocate (current, source=node_tree(value, Null(), Null()))
      else if (value < current%value) then
         call Put_tree(value, current%left)
      else if (value > current%value) then
         call Put_tree(value, current%right)
      end if
   end subroutine Put_tree
      
   ! Чтение списка.
   function Read_list(Input_File) result(List)
      type(node), pointer        :: List
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
        List => Read_value(In)
      close (In)
   end function Read_list

   ! Чтение списка.
   subroutine Read_list2(Input_File , List, Tail)
      type(node2), pointer        :: List, Tail
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
	    Tail => Null()
        call Read_value2(In, List, Tail)
      close (In)
   end subroutine Read_list2

   ! Чтение списка.
   subroutine Read_sorted_list(Input_File , List, Sorted_list)
      type(sorted_node), pointer        :: List, Sorted_list
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
        call Read_sorted_value(In, List, Sorted_list)
      close (In)
   end subroutine Read_sorted_list

   ! Чтение следующего значения.
   recursive function Read_value(In) result(Elem)
      type(node), pointer  :: Elem
      integer, intent(in)     :: In
      integer  IO
      
      allocate (Elem)
      read (In, '(i2)', iostat=IO, advance='no') Elem%value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
          Elem%next => Read_value(In)
      else
         deallocate (Elem)
         !nullify (Elem)
      end if
   end function Read_value

   ! Чтение следующего значения.
   recursive subroutine Read_value2(In, current, Tail)
      type(node2), pointer  :: current, Tail
      integer, intent(in)     :: In
      integer  IO
      
      allocate (Current)
      read (In, '(i2)', iostat=IO, advance='no') current%value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
         current%prev   => Tail
         Tail           => current 
         call Read_value2(In, current%next, Tail)
      else
         deallocate (current)
         !inullify (current)
      end if
   end subroutine Read_value2

   ! Чтение следующего значения.
   recursive subroutine Read_sorted_value(In, current, Sorted_list)
      type(sorted_node), pointer  :: current, Sorted_list
      integer, intent(in)     :: In
      integer  IO
      
      allocate (Current)
      read (In, '(i2)', iostat=IO, advance='no') current%value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
         call Sorted_put(Sorted_list, current) 
         call Read_sorted_value(In, current%next, Sorted_list)
      else
         deallocate (current)
      end if
   end subroutine Read_sorted_value

   recursive subroutine Sorted_put(Sorted_current, current)
      type(sorted_node), pointer  :: Sorted_current
      type(sorted_node), target   :: current
      type(sorted_node), pointer  :: tmp 

      if (.not. Associated(Sorted_current)) then
         ! Либо голова пока никуда не ссылается,
         ! либо дошли до поледнего элемента.
         Sorted_current => current
         !Sorted_current%sorted_next => Null() 
      else if (current%value >= Sorted_current%value) then
         call Sorted_put(Sorted_current%sorted_next, current)
      else
         !tmp => Sorted_current
         !Sorted_current => current
         !Sorted_current%sorted_next => tmp

         current%sorted_next => Sorted_current
         Sorted_current => current
      end if
   end subroutine Sorted_put
   
   ! Вывод списка.
   subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(node), pointer        :: List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      type(node), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(i0, 1x)', advance='no', iostat=IO) Elem%value 
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value
   
   ! Вывод списка.
   subroutine Output_list2(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(node2), pointer        :: List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value2(Out, List)
      close (Out)
   end subroutine Output_list2

   recursive subroutine Output_value2(Out, Elem)
      integer, intent(in)     :: Out
      type(node2), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(i0, 1x)', advance='no', iostat=IO) Elem%value 
         call Handle_IO_status(IO, "writing list")
         call Output_value2(Out, Elem%next)
      end if
   end subroutine Output_value2
   
   ! Вывод списка.
   subroutine Output_ordered_list(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(sorted_node), pointer        :: List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_ordered_value(Out, List)
      close (Out)
   end subroutine Output_ordered_list

   recursive subroutine Output_ordered_value(Out, Elem)
      integer, intent(in)     :: Out
      type(sorted_node), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(i0, 1x)', advance='no', iostat=IO) Elem%value 
         call Handle_IO_status(IO, "writing list")
         call Output_ordered_value(Out, Elem%next)
      end if
   end subroutine Output_ordered_value
   
   ! Вывод списка.
   subroutine Output_sorted_list(Output_File, Sorted_list, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(sorted_node), pointer        :: Sorted_list
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_sorted_value(Out, Sorted_list)
      close (Out)
   end subroutine Output_sorted_list

   subroutine Output_tree(Output_File, tree, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(node_tree), pointer        :: tree
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         if (Associated(tree)) &
            call Output_tree_node(Out, tree)
      close (Out)
   end subroutine Output_tree

   recursive subroutine Output_tree_node(Out, current)
      integer, intent(in)     :: Out
      type(node_tree),intent(in)   :: current
      
      integer  :: IO

      if (Associated(current%left)) &
         call Output_tree_node(Out, current%left)

      write (Out, '(i0, 1x)', advance='no', iostat=IO) current%value 
      call Handle_IO_status(IO, "writing tree")
   
      if (Associated(current%right)) &
         call Output_tree_node(Out, current%right)
   end subroutine Output_tree_node
   
   recursive subroutine Output_sorted_value(Out, Elem)
      integer, intent(in)     :: Out
      type(sorted_node), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(i0, 1x)', advance='no', iostat=IO) Elem%value 
         call Handle_IO_status(IO, "writing list")
         call Output_sorted_value(Out, Elem%sorted_next)
      end if
   end subroutine Output_sorted_value
end module List_IO 
