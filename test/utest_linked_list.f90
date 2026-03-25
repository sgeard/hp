program utest_linked_list

    use linked_list

    implicit none
    
    type(llist)      :: tokens
    character(len=2) :: tdata
    integer          :: i
        
    ! Add five examples
    do i=1,5
        write(tdata,'(a1,i1)') 'd',i
        call tokens%append(tdata)
    end do
    
    ! Print
    call tokens%print

end program utest_linked_list
