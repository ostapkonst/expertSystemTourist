% Copyright

implement expSubMenu
    open core, console, string, list, file, std, math

facts - db
    categ : (integer Категория, string Наименование).
    cond : (integer Условие, integer Категория, string Параметр).
    tour : (integer Тур, string Название, integer* Вопросы).

facts - expans
    answer : list{integer} := [].

predicates
    load : (myTerm).
    expertMenuItem : (integer Категория) determ.
    calcualteResult : () -> tuple{integer, integer}*.

clauses
    load(categG(Категория, Наименование)) :-
        assert(categ(Категория, Наименование)).

    load(condG(Условие, Категория, Параметр)) :-
        assert(cond(Условие, Категория, Параметр)).

    load(tourG(Тур, Название, Вопросы)) :-
        assert(tour(Тур, Название, Вопросы)).

    new(Terms) :-
        forAll(Terms, { (X) :- load(X) }).

    expert() :-
        categ(I, _),
        expertMenuItem(I),
        fail
        or
        if answer = [] then
            write("Требуется выбрать хотябы один параметр\n")
        else
            A = calcualteResult(),
            if A = [] then
                write("Подходящих туров не найдено\n")
            else
                writef("Искомые пункты: %\nПодобранные туры:\n", answer),
                L = sortBy({ (tuple(_, R1), tuple(_, R2)) = compare(R1, R2) }, A, descending), % сортировка результата
                forAll(L,
                    { (I) :-
                        tuple(F, K) = I,
                        tour(F, N, _),
                        !,
                        writef("% - c совпадением в % процентов\n", N, K)
                        or
                        succeed
                    })
            end if
        end if.

    % составление меню для одной категории
    expertMenuItem(I) :-
        categ(I, X),
        !,
        writef("Критерий - %\n", X), % составление текстового меню
        (cond(N, I, Y) and writef("\t% - %\n", N, Y) and fail or succeed),
        write("Выберите пункт/пункты: "),
        G = split_delimiter(readLine(), " "),
        A = [ N || cond(N, I, _) ], % список возможных параметров категории
        !,
        % можно было посчитать по другому: tryToTerm(list{integer}, format("[%]", readLine()))
        forAll(G,
            { (C) :-
                D = tryToTerm(integer, C),
                D in A,
                not(D in answer), % удаление дубликатов
                answer := [D | answer],
                !
                or
                succeed
            }),
        answer := reverse(answer).  % восстановление порядка ввода

    % расчет коэффициента релевантности
    calcualteResult() =
        [ T ||
            tour(I, _, P),
            T1 = list::length(intersectionEq({ (A, B) :- A = B }, P, answer)),
            R = round(T1 / list::length(answer) * 100),
            R >= 25, % фильтрация результата по проценту релевантности
            T = tuple(I, R)
        ].

end implement expSubMenu
