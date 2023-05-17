program task7_8
    ! вычислить сумму элементов a(i,j) * x(i) * y(j), i=1,размерность массива, j=1,размерность массива, где
    ! a(n,n) - матрица
    ! x(n), y(n) - векторы
    ! n - задается через файл
    use Environment

    character(*), parameter                 :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                             :: In=1, Out=0, massive_count=0, global_sum = 0, i, j
    integer(I_), allocatable                :: matrix(:,:), vector_x(:), vector_y(:)
    real(R_), allocatable                   :: num_massive(:,:)

    open (file=input_file, newunit=In)
        read(In, *) massive_count

        ! размещаем массивы в памяти после того как узнали их размер, считав число с файла
        allocate(matrix(massive_count, massive_count))
        allocate(num_massive(massive_count, massive_count))
        allocate(vector_x(massive_count))
        allocate(vector_y(massive_count))
    close (In)

    ! рандомные значения для массива
    call random_number(num_massive)
    matrix = int(num_massive * 10)
    call random_number(num_massive) ! чтобы обновились значения и векотры не содержали строки или столбцы матрицы
    vector_x = int(num_massive(1,:) * 10) ! вектор x будет содержать всю первую строку нового рандомного массива
    vector_y = int(num_massive(2,:) * 10) ! вектор y будет содержать всю вторую строку нового рандомного массива



    ! ввиду того, что в фортране двумерные и далее массивы считаются по столбцам
    ! инвертируем порядок считывания
    ! было:                             a(i,j) * x(i) * y(j)
    ! из-за особенности фортрана стало: a(j,i) * x(j) * y(i)
    do i=1, massive_count
        do j=1, massive_count
            global_sum = global_sum + matrix(j, i) * vector_x(j) * vector_y(i)
        end do
    end do

    open (file=output_file, newunit=Out)
        write(Out, *) "-------------MATRIX-----------"
        write(Out, "("//massive_count//"i3)") matrix
        write(Out, *) "--------------END-------------"
        write(Out, *) "global sum: ", global_sum
    close (Out)
end program task7_8
