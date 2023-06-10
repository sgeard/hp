module linked_list
    public
    
    type llist
        private
        type(llist_node), pointer :: begin => null()
        type(llist_node), pointer :: end => null()
    contains
        procedure                 :: iterate => iterate_ll
    end type llist
  
    type llist_node
        private
        character(len=:), allocatable :: data
        type(llist_node), pointer :: next => null()
    end type llist_node

    ! Interface for functions being applied to each list element in turn
    ! when iterating
    abstract interface
        subroutine command_fun(command, ok)
            character(*), intent(in) :: command
            logical, intent(out)     :: ok ! Exit the loop if not true
        end subroutine command_fun
    end interface

    
contains

    function iterate_ll(this, f) result(r)
        class(llist), intent(inout), target :: this
        procedure(command_fun)              :: f
        logical :: r
        type(llist_node), pointer :: token
        token => this%begin
        do
            if (.not. associated(token)) exit
            call f(trim(token%data), r)
            if (.not. r) then
                exit
            end if
            token => token%next
        end do
    end function iterate_ll
       
    subroutine append(lst, data)
      type(llist), intent(inout) :: lst
      character(*), intent(in)    :: data
      if (.not. associated(lst%begin)) then
         allocate(lst%begin)
         lst%begin%data = data
         lst%end => lst%begin
      else
         allocate(lst%end%next)
         lst%end%next%data = data
         lst%end => lst%end%next
      end if
    end subroutine append

    subroutine print(lst)
      type(llist), intent(in) :: lst
      type(llist_node), pointer :: next
      write(*,'(a)') 'Tokens:'
      next => lst%begin
      do
         if (.not. associated(next)) exit
         write(*,'(4x,a)') next%data
         next => next%next
      end do
    end subroutine print

    integer function size(lst)
      type(llist), intent(inout) :: lst
      type(llist_node), pointer  :: this
      size = 0
      this => lst%begin
      do
         if (.not. associated(this)) exit
         size = size + 1
         this => this%next
      end do
    end function size

    subroutine clear(lst)
      type(llist), intent(inout) :: lst
      type(llist_node), pointer  :: this, next
      this => lst%begin
      do
         if (.not. associated(this)) exit
         next => this%next
         deallocate(this)
         this => next
      end do
      nullify(lst%end)
      nullify(lst%begin)
    end subroutine clear
end module linked_list
