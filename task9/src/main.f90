program task7_17v
    ! найти элемент в массиве, являющийся наибольшим по модулю и вывести его и его позицию
    use Environment

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                         :: In=1, Out=0, matrix_rank=0, max_abs_num=-1, i ,j
    integer(I_), allocatable, target    :: matrix(:,:)
    integer(I_), pointer                :: ptr(:,:)
    real(R_), allocatable               :: r_num_massive(:,:)


    open (file=input_file, newunit=In)
        read(In, *) matrix_rank ! считываем размерность матрицы

        ! размещаем массивы в памяти, первый - основной, второй - для рандомизации
        allocate(matrix(matrix_rank, matrix_rank))
        allocate(r_num_massive(matrix_rank, matrix_rank))
    close (In)

    ! рандомные значения для массива от -5 до 4
    call random_number(r_num_massive)
    matrix = int(r_num_massive * 10)-5

    ! указатель на массив
    ptr => matrix

    ! указатель на массив и переменную для минимальнго элемента по модулю в массиве
    call find_val(ptr, max_abs_num)

    open (file=output_file, newunit=Out)
        ! выводим матрицу
        write(Out, *) "-------------MATRIX-----------"
        write(Out, "("//matrix_rank//"i3)") matrix
        write(Out, *) "--------------END-------------"

        !выводим максимальный по модулю элемент
        write(Out, "(a21,i1)") "max abs elem = ", max_abs_num

        ! ищем по всей матрицы индексы этих элементов по модулю и выводим их позицию
        ! спецификация фортрана заставляет изменить порядок исчисления массива, так как считается по столбцам
        ! поэтому счетчики вместо i,j будут j,i, чтобы идти как прежде, по строкам
        do i = 1, matrix_rank
            do j = 1, matrix_rank
                if (abs(matrix(j,i)) == max_abs_num) then
                    write(Out,"(a5,i2,a1,i2,a1)") "pos=(", i,":", j, ")"
                end if
            end do
        end do
    close (Out)

contains
    subroutine find_val(row_matrix, return_num)
        integer(I_), intent(inout)      :: row_matrix(:,:)
        integer(I_), intent(out)        :: return_num ! найденный элемент вернем обратно
        integer(I_)                     :: min_pos_num, max_neg_num ! числа в двух массивах, что будет сравнивать для определения наибольшего по модулю
        Integer(I_), allocatable        :: neg_mas(:), pos_mas(:) ! для разбивки массива на негативные числа и позитивные

        neg_mas = pack(row_matrix, row_matrix < 0) ! массив негативных чисел (будем искать минимальное число в нем)
        pos_mas = pack(row_matrix, row_matrix >= 0) ! массив положительных чисел, будем искать максимальное число

        ! получения значений для сравнения
        min_pos_num = maxval(pos_mas)
        max_neg_num = abs(minval(neg_mas))

        ! сверяем чье больше
        if (min_pos_num < max_neg_num) then
            min_pos_num = max_neg_num
        end if

        return_num = min_pos_num

    end subroutine find_val
end program task7_17v
