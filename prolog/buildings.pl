building(empire_state,        [yes, no,  no,  no,  yes, no]).
building(burj_khalifa,       [yes, no,  no,  yes, yes, yes]).  
building(cottage_19c,        [no,  yes, yes, no,  no,  no]).   
building(modern_villa,       [no,  yes, no,  yes, yes, yes]).  
building(brick_warehouse,    [no,  no,  yes, no,  no,  no]).   
building(glass_office,       [yes, no,  no,  yes, yes, yes]).  
building(panel_5story,       [no,  no,  no,  no,  no,  no]).   
building(loft_studio,        [no,  no,  yes, yes, no,  yes]).  
building(historic_library,   [no,  no,  yes, no,  no,  no]).   
building(suburban_house,     [no,  yes, no,  yes, no,  yes]).  
building(modular_hospital,   [no,  no,  no,  yes, yes, yes]). 
building(stalin_skyscraper,  [yes, no,  yes, no,  yes, no]).   
building(wooden_cottage,     [no,  yes, no,  no,  no,  no]).   
building(shopping_mall,      [no,  no,  no,  yes, yes, yes]).  
building(underground_garage, [no,  no,  no,  no,  yes, yes]).
building(glass_house,        [no,  yes, no,  no,  no,  yes]).
building(brick_school,       [no,  no,  yes, yes, no,  no]).
building(container_home,     [no,  yes, no,  yes, no,  yes]). 
building(medieval_castle,    [no,  no,  yes, no,  no,  no]).   
building(prefab_house,       [no,  yes, no,  yes, no,  yes]).  
building(eco_dome,           [no,  yes, no,  no,  no,  yes]). 
building(art_deco_theater,   [no,  no,  no,  no,  yes, no]).  
building(floating_house,     [no,  yes, no,  yes, no,  yes]).  
building(underground_bunker, [no,  no,  yes, no,  yes, no]).  
building(futuristic_tower,   [yes, no,  no,  yes, yes, yes]). 
building(rural_church,       [no,  no,  yes, no,  no,  no]).   
building(mobile_home,        [no,  yes, no,  yes, no,  yes]).  
building(industrial_plant,   [no,  no,  no,  no,  yes, yes]). 
building(space_station,      [yes, no,  no,  yes, yes, yes]).

match_building(Building, Answers) :-
    building(Building, BuildingAnswers),
    check_answers(BuildingAnswers, Answers).

check_answers([], []).
check_answers([H|T1], [H|T2]) :- check_answers(T1, T2).

start_building :-
    write('Ответьте на 6 вопросов (yes/no):'), nl,
    ask_questions(Answers),
    findall(B, match_building(B, Answers), Results),
    show_results(Results).

ask_questions([Q1,Q2,Q3,Q4,Q5,Q6]) :-
    ask('1. Это высотное здание (более 10 этажей)? ', Q1),
    ask('2. Это частный жилой дом? ', Q2),
    ask('3. Основной материал стен - кирпич? ', Q3),
    ask('4. Используется гипсокартон в отделке? ', Q4),
    ask('5. Наличие подземной парковки? ', Q5),
    ask('6. Здание построено после 2000 года? ', Q6).

ask(Question, Answer) :-
    write(Question), read(Answer),
    (valid_answer(Answer) -> true ; 
        write('Некорректный ответ. Используйте yes/no.'), nl, ask(Question, Answer)).

valid_answer(yes). valid_answer(no).

show_results([]) :- write('Здание не найдено в базе.'), nl.
show_results([H|T]) :- 
    write('Возможные варианты: '), nl,
    write(H), nl,
    show_remaining(T).

show_remaining([]).
show_remaining([H|T]) :- write(H), nl, show_remaining(T).