! Associative map string -> real(real64)
module amap
    use iso_fortran_env, only: real64
    implicit none
    
    ! The key
    type key_t
        private
        character(len=16)  :: k = '-'
    contains
        procedure, private :: equals_key_t
        generic, public    :: operator(==) => equals_key_t
        procedure, private :: write_key_t
        generic, public    :: write(formatted) => write_key_t
        procedure, private :: set_to_key_t
        generic, public    :: assignment(=) => set_to_key_t
    end type key_t

    ! The value
    type value_t
        private
        real(real64) :: v = huge(0.0d0) ! An out-of-band value
    contains
        procedure, private :: write_value_t
        generic, public    :: write(formatted) => write_value_t
        procedure, private :: set_to_value_t
        generic, public    :: assignment(=) => set_to_value_t
        procedure, private :: equals_value_t
        generic, public    :: operator(==) => equals_value_t
    end type value_t
    
    ! Map elements are (key,value) pairs
    type pair_t
        type(key_t)   :: k = key_t()
        type(value_t) :: v = value_t()
    end type pair_t
    
    ! The map
    type amap_t
        private
        type(pair_t), allocatable :: pairs(:)
        integer, private :: extent = 10
        integer, private :: high_water = 0
    contains
        procedure, public  :: get => get_amap_t
        procedure, public  :: get_value => get_value_amap_t
        procedure, public  :: set => set_amap_t
        procedure, public  :: find => find_amap_t
        procedure, public  :: print => print_amap_t
        procedure, public  :: clear => clear_amap_t
        procedure, public  :: size => size_amap_t
        procedure, private :: is_key_kt
        procedure, private :: is_key_kvt
        generic, public    :: contains => is_key_kt, is_key_kvt
    end type amap_t

contains

    function size_amap_t(this) result(r)
        class(amap_t), intent(in) :: this
        integer :: r
        r = this%high_water
    end function size_amap_t
    
    subroutine clear_amap_t(this)
        class(amap_t), intent(inout)  :: this
        if (allocated(this%pairs)) then
            deallocate(this%pairs)
        end if
        this%high_water = 0
    end subroutine clear_amap_t
    
    subroutine set_to_key_t(this, k)
        class(key_t), intent(inout)  :: this
        character(len=*), intent(in) :: k
        this%k = adjustl(k)
    end subroutine set_to_key_t
    
    subroutine set_to_value_t(this, v)
        class(value_t), intent(inout)  :: this
        real(real64), intent(in) :: v
        this%v = v
    end subroutine set_to_value_t

    subroutine print_amap_t(this)
        class(amap_t), intent(in) :: this
        integer :: i
        write(6,'(a,i0,a)') 'Map has ',this%high_water,' elements'
        do i=1,this%high_water
            write(6,'(4x,dt,a,dt)') this%pairs(i)%k,' -> ',this%pairs(i)%v
        end do
    end subroutine print_amap_t
    
    subroutine set_amap_t(this,kv,vv)
        class(amap_t), intent(inout) :: this
        character(len=*), intent(in) :: kv
        real(real64), intent(in)          :: vv
        type(pair_t), allocatable :: tmp_pairs(:)
        type(key_t)               :: k
        type(value_t)             :: v
        integer                   :: idx
        
        k = kv
        v = vv
        if (.not. allocated(this%pairs)) then
            allocate(this%pairs(this%extent))
        end if    
        
        idx = this%find(k)
        if (idx > 0) then
            this%pairs(idx) = pair_t(k,v)
            return
        end if
        
        if (this%high_water == size(this%pairs)) then
            allocate(tmp_pairs(size(this%pairs)+this%extent))
            tmp_pairs(1:this%high_water) = this%pairs
            call move_alloc(tmp_pairs, this%pairs)
        end if
        this%high_water = this%high_water + 1
        this%pairs(this%high_water) = pair_t(k,v)
        
    end subroutine set_amap_t
    
    function find_amap_t(this, k) result(r)
        class(amap_t), intent(in) :: this
        type(key_t), intent(in) :: k
        integer :: r
        do r = 1, this%high_water
            if (this%pairs(r)%k == k) then
                return
            end if
        end do
        r = 0
    end function find_amap_t
    
    function get_amap_t(this, kv) result(r)
        class(amap_t), intent(in)    :: this
        character(len=*), intent(in) :: kv
        type(value_t) :: r
        type(key_t) :: k
        integer :: idx
        k = kv
        idx = this%find(k)
        if (idx > 0) then
            r = this%pairs(idx)%v
        else
            r = value_t()
        end if
    end function get_amap_t
    
    function get_value_amap_t(this, kv) result(r)
        class(amap_t), intent(in)    :: this
        character(len=*), intent(in) :: kv
        real(real64) :: r
        type(value_t) :: s
        s = this%get(kv)
        r = s%v
    end function get_value_amap_t
    
    function is_key_kt(this, k) result(r)
        class(amap_t), intent(in) :: this
        type(key_t), intent(in) :: k
        logical :: r
        r = this%find(k) > 0
    end function is_key_kt
    
    function is_key_kvt(this, kv) result(r)
        class(amap_t), intent(in) :: this
        character(len=*), intent(in) :: kv
        logical :: r
        r = this%find(key_t(kv)) > 0
    end function is_key_kvt
    
    function equals_key_t(this, k) result(r)
        class(key_t), intent(in) :: this
        type(key_t), intent(in) :: k
        logical :: r
        r = trim(adjustl(this%k)) == trim(adjustl(k%k))
    end function equals_key_t
    
    function equals_value_t(this, v) result(r)
        class(value_t), intent(in) :: this
        real(real64), intent(in) :: v
        logical :: r
        r = this%v == v
    end function equals_value_t
    
    subroutine write_key_t(key, unit, iotype, v_list, iostat, iomsg)
        class(key_t), intent(in)    :: key
        integer, intent(in)         :: unit
        character(*), intent(in)    :: iotype
        integer, intent(in)         :: v_list(:)
        integer, intent(out)        :: iostat
        character(*), intent(inout) :: iomsg
        iostat = 0
        !iomsg = ""
        write(6,'(a)', iostat=iostat, iomsg=iomsg) trim(adjustl(key%k))
    end subroutine write_key_t

    subroutine write_value_t(value, unit, iotype, v_list, iostat, iomsg)
        class(value_t), intent(in)    :: value
        integer, intent(in)         :: unit
        character(*), intent(in)    :: iotype
        integer, intent(in)         :: v_list(:)
        integer, intent(out)        :: iostat
        character(*), intent(inout) :: iomsg
        iostat = 0
        !iomsg = ""
        write(6,'(f0.6)', iostat=iostat, iomsg=iomsg) value%v
    end subroutine write_value_t
end module amap
