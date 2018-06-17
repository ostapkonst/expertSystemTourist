% Copyright

interface expSubsystem
    open core

domains
    myTerm =
        categG(integer Категория, string Наименование);
        condG(integer Условие, integer Категория, string Параметр);
        tourG(integer Тур, string Название, integer* Вопросы).

predicates
    expert : ().

end interface expSubsystem
