program knapsack_greedy
    implicit none

    type :: item_type
        integer :: weight
        integer :: value
        real :: ratio
    end type item_type

    integer :: num_items, capacity, i
    type(item_type), allocatable :: items(:)
    real :: total_value
    character(len=100) :: filename
    integer :: io_status

    call get_command_argument(1, filename)
    open(unit=10, file=filename, status='old', action='read')
    read(10, *) num_items, capacity
    allocate(items(num_items))

    do i = 1, num_items
        read(10, *, iostat=io_status) items(i)%weight, items(i)%value
        items(i)%ratio = real(items(i)%value) / real(items(i)%weight)
    end do
    close(10)

    call quicksort(items, 1, num_items)

    total_value = 0.0
    do i = 1, num_items
        if (capacity >= items(i)%weight) then
            capacity = capacity - items(i)%weight
            total_value = total_value + items(i)%value
        end if
    end do

    print *, total_value

    deallocate(items)

contains
    ! QuickSort implementation
    recursive subroutine quicksort(arr, low, high)
        type(item_type), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: i, j
        type(item_type) :: pivot, temp

        if (low < high) then
            pivot = arr(low)
            i = low
            j = high
            do
                do while (arr(i)%ratio > pivot%ratio)
                    i = i + 1
                end do
                do while (arr(j)%ratio < pivot%ratio)
                    j = j - 1
                end do
                if (i <= j) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                    i = i + 1
                    j = j - 1
                end if
                if (i > j) exit
            end do
            call quicksort(arr, low, j)
            call quicksort(arr, i, high)
        end if
    end subroutine quicksort
end program knapsack_greedy
