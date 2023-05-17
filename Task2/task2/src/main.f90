program task2_3
    ! Даны 4 вещественных числа, определить сколько из них положительных
    use Environment

    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                 :: In=1, Out=0, positive=0, negative=0
    real(R_)                    :: num_massive(4)

    open (file=input_file, newunit=In)
        read (In, *) num_massive
    close (In)

    ! создаем из массива массив с указанным условием
    ! считываем его размер
    positive = size(pack(num_massive, num_massive > 0))
    negative = size(pack(num_massive, num_massive < 0))

    open (file=output_file, newunit=Out)
        write(Out, *) "Позитивных чисел:", positive
        write(Out, *) "Негативных чисел:", negative
    close (Out)

end program task2_3
