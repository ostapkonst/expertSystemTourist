% Copyright

implement expSubQuestion
    open core, console, string, list, file, std

domains
    answer = да; нет.

facts - db
    categ : (integer Категория, string Наименование).
    cond : (integer Условие, integer Категория, string Параметр).
    tour : (integer Тур, string Название, integer* Вопросы).

facts - expans
    answer : (integer Условия, answer).

predicates
    ask : (integer Условие, integer* УсловияТура) determ.
    ask : (answer, integer Условие, integer* УсловияТура) determ.
    ans : (string СтрокаОтвета) -> answer multi.
    correct : (integer* УсловияТура) determ.
    load : (myTerm).

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
        tour(K, T, Q),
        ask(K, Q),
        correct(Q),
        !,
        writef("Заключение: %.", T).

    expert() :-
        writef("Не хватает сведений, чтобы определить тур.\n").

    % проверка если признак отсутствует, то тур не подходит
    ask(_, [N | _]) :-
        answer(N, нет),
        !,
        fail.

    % если признак присутствует в туре, то задается следующий вопрос
    ask(K, [N | Q]) :-
        answer(N, да),
        !,
        ask(K, Q).

    ask(K, [N | Q]) :-
        % задание вопроса о содержания признака в туре
        cond(N, C, Y),
        categ(C, X),
        writef("Верно ли, что % - %? (да/нет): > ", X, Y),
        A = ans(readLine()), % проверка введенного значания: 1 - да, инече - нет
        !,
        ask(A, K, [N | Q]).

    % условие окончания рекурсии
    ask(_, []).

    % ответ пользвателя запоминается в БД
    ask(A, _, [N | _]) :-
        assert(answer(N, A)),
        fail.

    % если ответ положительный, то опрос продолжается
    ask(да, K, [_ | Q]) :-
        ask(K, Q).

    % если ввели "да"
    ans("да") = да.

    % если ввели "нет" или ничего не ввели
    ans(_) = нет.

    correct(Q) :-
        answer(N, да), % если есть признак, которго не может быть в туре, то тур не подходит
        not(isMember(N, Q)),
        !,
        fail
        or
        succeed.

end implement expSubQuestion
