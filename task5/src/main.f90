program task5_2
    ! в таблице из 1000 значений различных немонотонной функции f(x), найти и напечатать
    ! локальный минимум и максимум и номера этих значений
    use Environment

    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                 :: In=0, Out=0
    ! будем считать, что в таблице 20 разных значений функции, значения возьмем из output.txt 4го задания
    ! так же считаем, что 1 строка - 1 число функции
    real(R_)                    :: num_massive(20)

    ! считывание всех чисел
    open (file=input_file, newunit=In)
        read (In, *) num_massive
    close (In)

    open (file=output_file, newunit=Out)
        write(Out, *) "min = ", minval(num_massive), "position: ", minloc(num_massive)
        write(Out, *) "max = ", maxval(num_massive), "position: ", maxloc(num_massive)
    close (Out)

end program task5_2
