program knapsack_problem
    use iso_fortran_env, only: error_unit
    implicit none
    integer :: num_items, i, io_status, w
    integer :: capacity, weight, value
    integer, allocatable :: dp(:)
    character(len=100) :: filename

    ! obtém o nome do arquivo do primeiro argumento
    call get_command_argument(1, filename)
    ! abre o arquivo
    open(unit=10, file=filename, status='old', action='read')


    ! lê a capacidade da mochila
    read(10, *) num_items, capacity

    ! aloca dinamicamente o array dp
    allocate(dp(capacity+1))


    dp=0
    do i = 1, num_items
        read(10, *, iostat=io_status) weight, value
        do w = capacity, weight, -1
            dp(w) = max(dp(w), dp(w - weight) + value)
        end do
    end do
    close(10)

    print *, dp(capacity)
    deallocate( dp)
end program knapsack_problem
