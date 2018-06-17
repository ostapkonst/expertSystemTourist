% Copyright

class expertSubsystem : expertSubsystem
    open core

domains
    myTerm=
        categG(integer Категория, string Наименование);
        condG(integer Условие, integer Категория, string Параметр);
        tourG(integer Тур, string Название, integer* Вопросы).

predicates
    expert: (myTerm*).

end class expertSubsystem