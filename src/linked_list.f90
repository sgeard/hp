module linked_list
    public
    
    type llist
        private
        type(llist_node), pointer :: begin => null()
        type(llist_node), pointer :: end => null()
    contains
        procedure, public :: iterate => iterate_ll
        procedure, public :: print => print_ll
        procedure, public :: append => append_ll
        procedure, public :: size => size_ll
        procedure, public :: clear => clear_ll
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

    interface
        module function iterate_ll(this, f) result(r)
            class(llist), intent(inout), target :: this
            procedure(command_fun)              :: f
            logical :: r
        end function iterate_ll

        module subroutine print_ll(lst)
            class(llist), intent(in) :: lst
        end subroutine print_ll

        module subroutine append_ll(lst, data)
            class(llist), intent(inout) :: lst
            class(*), intent(in)   :: data
        end subroutine append_ll

        module integer function size_ll(lst)
            class(llist), intent(inout) :: lst
        end function size_ll

        module subroutine clear_ll(lst)
            class(llist), intent(inout) :: lst
        end subroutine clear_ll

    end interface

end module linked_list
