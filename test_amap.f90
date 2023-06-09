program test_amap
    use amap
    implicit none
    
    type(amap_t) :: my_amap
    type(value_t) :: x
    
    call my_amap%set('one',1.0d0)
    call my_amap%set('two',2.d0)
    call my_amap%set('three',3.0d0)
    call my_amap%set('four',4.0d0)
    call my_amap%set('five',5.0d0)
    call my_amap%print
    
    x = my_amap%get('four')
    write(6,'(f0.6)') my_amap%get_value('four')
    print *,my_amap%contains('one'),my_amap%contains('ten')
end program test_amap
