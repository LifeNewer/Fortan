program task7_1v
    ! отсортировать элементы списка так что |a(i)| <= |a(i+1)|
    use Environment

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                         :: In=0, Out=0, massive_count=0
    integer(I_), allocatable, target    :: massive(:) ! массив не аллоцируемый
    integer(I_), pointer                :: ptr(:) ! указатель на массив
    real(R_), allocatable               :: r_num_massive(:) ! массив не аллоцируемый

    open (file=input_file, newunit=In)
        read (In, *) massive_count ! считываем количество цифр в массиве в файле

        ! Создание памяти массивов дин. длины
        allocate(massive(massive_count)) ! создаем основной массив
        allocate(r_num_massive(massive_count)) ! создаем массив для рандомизации чисел

        ! рандомные значения для массива дипазона от -50 до 50
        call random_number(r_num_massive)
        massive = int(r_num_massive * 100)-50 ! таким способом в основном массиве окажутся элементы от -50 до 50
        
        ! указатель на массив
        ptr => massive
    close (In)

    ! вызов сортировки пузырьком
    call quicksort(ptr, massive_count)

    open (file=output_file, newunit=Out)
        write(Out, "(a14,"//massive_count//"i4)")   "massive = ",     massive 
    close(Out)

contains
    subroutine quicksort(ptr, size)
        integer(I_), intent(in)     :: size
        integer(I_), intent(out)    :: ptr(:)
        integer(I_)                 :: i, j, t


        do i=size-1, 1, -1 ! с шагом в -1 с конца до начала
            do j=1, i ! с шагом 1 (задан по умолчанию), с начала до i
                if (abs(ptr(j)) <= abs(ptr(j+1))) then
                    ! реализация перестановки местами элементов при помощи переменной t
                    t = ptr(j)
                    ptr(j) = ptr(j+1)
                    ptr(j+1) = t
                end if
            end do
        end do
    end subroutine quicksort
end program task7_1v
