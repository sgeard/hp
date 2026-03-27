submodule (linked_list) linked_list_sm
    implicit none

contains

    module function iterate_ll(this, f) result(r)
        class(llist), intent(inout), target :: this
        procedure(command_fun)              :: f
        logical :: r
        type(llist_node), pointer :: token
        r = .true.
        token => this%begin
        do
            if (.not. associated(token)) exit
            call f(token%data, r)
            if (.not. r) then
                exit
            end if
            token => token%next
        end do
    end function iterate_ll
       
    module subroutine append_ll(lst, data)
      class(llist), intent(inout) :: lst
      class(*), intent(in)   :: data
      if (.not. associated(lst%begin)) then
         allocate(lst%begin)
         select type(data)
         type is (character(*))
            lst%begin%data = data
         end select
         lst%end => lst%begin
      else
         allocate(lst%end%next)
         select type(data)
         type is (character(*))
            lst%end%next%data = data
         end select
         lst%end => lst%end%next
      end if
    end subroutine append_ll

    module subroutine print_ll(lst)
        class(llist), intent(in) :: lst
        type(llist_node), pointer :: next
        write(*,'(a)') 'Tokens:'
        next => lst%begin
        if (.not. associated(next)) then
            write(*,'(a)') ' *** none found ***'
            return
        end if
        do
            if (.not. associated(next)) exit
            write(*,'(4x,a)') next%data
            next => next%next
        end do
    end subroutine print_ll

    module integer function size_ll(lst)
      class(llist), intent(inout) :: lst
      type(llist_node), pointer  :: this
      size_ll = 0
      this => lst%begin
      do
         if (.not. associated(this)) exit
         size_ll = size_ll + 1
         this => this%next
      end do
    end function size_ll

    module subroutine clear_ll(lst)
      class(llist), intent(inout) :: lst
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
    end subroutine clear_ll

end submodule linked_list_sm
