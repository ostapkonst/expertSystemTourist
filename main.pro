% Copyright

implement main
    open core, console, string, list, file, std, math, expSubsystem

constants
    fileName = "tours.txt".

class facts - db
    categ : (integer Категория, string Наименование).
    cond : (integer Условие, integer Категория, string Параметр).
    tour : (integer Тур, string Название, integer* Вопросы).

class predicates
    intro : ().
    maindialog : ().
    act : (string НомерДействия) determ.
    printTours : ().
    printToursMini : ().
    printCategorys : ().
    addTour : (string Название).
    deleteTour : (string Тур).
    toList : () -> myTerm*.

clauses
    intro() :-
        write("Не знаете какую путевку выбрать?\n", "Эта экспертная система поможет Вам определиться с выбором.\n").

    maindialog() :-
        repeat(),
        write("\nВыберите один из пунктов меню:\n", "\t1. Экспертная подборка тура\n", "\t2. Просмотр базы знаний\n",
            "\t3. Редактирование базы знаний\n", "\t4. Служебные функции\n", "\t5. Выход\n\n", "Пункт: "),
        X = readLine(),
        act(X),
        !.
    maindialog().

    toList() =
        [ Z ||
            categ(A, B),
            Z = categG(A, B)
            or
            cond(C, D, E),
            Z = condG(C, D, E)
            or
            tour(F, G, H),
            Z = tourG(F, G, H)
        ].

    act("1") :-
        !,
        write("\nЭкспертная подборка тура. Выберите действие:\n", "\t1.1 В виде вопросов\n", "\t1.2 В виде меню\n\n", "Пункт: "),
        X = readLine(),
        act(X).

    act("1.1") :-
        !,
        write("Вопросы для выбора тура:\n"),
        Q = expSubQuestion::new(toList()),
        Q:expert(),
        fail.

    act("1.2") :-
        !,
        write("Меню для выбота тура (несколько значений через пробел):\n"),
        M = expSubMenu::new(toList()),
        M:expert(),
        fail.

    act("2") :-
        !,
        write("\nПросмотр базы знаний. Выберите действие:\n", "\t2.1 Просмотр туров\n", "\t2.2 Просмотр категорий и их возможных значений\n\n",
            "Пункт: "),
        X = readLine(),
        act(X).

    act("2.1") :-
        !,
        write("Все туры в экспертной системе:\n"),
        printTours(),
        fail.

    act("2.2") :-
        !,
        write("Все категории туров:\n"),
        printCategorys(),
        fail.

    act("3") :-
        !,
        write("\nРедактирование базы знаний. Выберите действие:\n", "\t3.1 Добавление тура\n", "\t3.2 Удаление тура\n\n", "Пункт: "),
        X = readLine(),
        act(X).

    act("3.1") :-
        !,
        write("Добавление тура. Введите название тура.\n"),
        write("Название тура: "),
        addTour(readLine()),
        fail.

    act("3.2") :-
        !,
        write("Уделение тура. Выберите тур для удаления:\n"),
        printToursMini(),
        write("Удаляемый тур: "),
        deleteTour(readLine()),
        fail.

    act("4") :-
        !,
        write("\nСлужебные функции. Выберите действие:\n", "\t4.1 Сохранить базу (автоматически сохраняется)\n", "\t4.2 Отменить изменения\n\n",
            "Пункт: "),
        X = readLine(),
        act(X).

    act("4.1") :-
        !,
        save(fileName, db),
        write("БД сохранена\n"),
        fail.

    act("4.2") :-
        !,
        retractFactDb(db),
        consult(fileName, db),
        write("Изменения отменены\n"),
        fail.

    act("5") :-
        !,
        write("Выход.\n").

    act(_) :-
        write("Неккоректный ввод, повторите: "),
        X = readLine(),
        act(X).

    printTours() :-
        tour(_, N, P),
        writef("%\n", N),
        F =
            { (I) :-
                cond(I, X, Y),
                categ(X, Z),
                !,
                writef("\t% - %\n", Z, Y)
                or
                succeed
            },
        forAll(P, F),
        fail
        or
        succeed.

    printToursMini() :-
        tour(I, N, _),
        writef("\t% - %\n", I, N),
        fail
        or
        succeed.

    printCategorys() :-
        categ(I, X),
        writef("%\n", X),
        cond(N, I, Y),
        writef("\t% - %\n", N, Y),
        fail
        or
        succeed.

    deleteTour(X) :-
        I = tryToTerm(integer, X),
        retract(tour(I, N, _)),
        !,
        writef("Тур '% - %' удален\n", I, N)
        or
        writef("Тура с № % нет в базе данных\n", X).

    addTour(N) :-
        trim(N) = "",
        !,
        writef("Тур '%' должен содержать минимум один значащий символ\n", N)
        or
        tour(_, N, _),
        !,
        writef("Тур с именем '%' уже есть в базе данных\n", N)
        or
        I = maximum([ A || tour(A, _, _) ]) + 1,
        write("Список всех категорий:\n"),
        printCategorys(),
        write("Выберите пункт/пункты: "),
        G = split_delimiter(readLine(), " "),
        S = removeDuplicatesEq({ (A, B) :- A = B }, G),
        R =
            filteredMap(S,
                { (C) = D :-
                    D = tryToTerm(integer, C),
                    cond(D, _, _),
                    !
                }),
        R <> [],
        assert(tour(I, N, R)),
        !,
        writef("Тур '% - %' с пунктами: % добавлен\n", I, N, R)
        or
        writef("Тур '%' должен содержать минимум один параметр\n", N).

    run() :-
        setConsoleTitle("Экспертная система по подбору туристической путевки"),
        consult(fileName, db),
        intro(),
        maindialog(),
        save(fileName, db),
        _ = readLine().

end implement main

goal
    console::run(main::run).
