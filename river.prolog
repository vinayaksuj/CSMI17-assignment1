max_weight(100).

weight(man, 80).
weight(woman, 80).
weight(child1, 30).
weight(child2, 30).

initial_state(state(0, 0, 0, 0, 0)).
goal_state(state(1, 1, 1, 1, 1)).

valid_state(state(M, W, C1, C2, Boat)) :-
    member(M, [0, 1]),
    member(W, [0, 1]),
    member(C1, [0, 1]),
    member(C2, [0, 1]),
    member(Boat, [0, 1]).

move(state(M1, W1, C1_1, C2_1, 0), state(M2, W2, C1_2, C2_2, 1), People) :-
    boat_weight(People, Weight),
    max_weight(MaxWeight),
    Weight =< MaxWeight,
    move_people(state(M1, W1, C1_1, C2_1, 0), state(M2, W2, C1_2, C2_2, 1), People).

move(state(M1, W1, C1_1, C2_1, 1), state(M2, W2, C1_2, C2_2, 0), People) :-
    boat_weight(People, Weight),
    max_weight(MaxWeight),
    Weight =< MaxWeight,
    move_people(state(M1, W1, C1_1, C2_1, 1), state(M2, W2, C1_2, C2_2, 0), People).

move_people(state(M1, W1, C1_1, C2_1, Boat1), state(M2, W2, C1_2, C2_2, Boat2), People) :-
    move_one_or_two(People),
    update_state(state(M1, W1, C1_1, C2_1, Boat1), state(M2, W2, C1_2, C2_2, Boat2), People).

move_one_or_two([man]).
move_one_or_two([woman]).
move_one_or_two([child1]).
move_one_or_two([child2]).
move_one_or_two([child1, child2]).
move_one_or_two([man, child1]).
move_one_or_two([man, child2]).
move_one_or_two([woman, child1]).
move_one_or_two([woman, child2]).

boat_weight([], 0).
boat_weight([Person | Rest], Weight) :-
    weight(Person, W),
    boat_weight(Rest, RestWeight),
    Weight is W + RestWeight.

update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [man]) :-
    M2 is 1 - M1, W2 = W1, C1_2 = C1_1, C2_2 = C2_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [woman]) :-
    W2 is 1 - W1, M2 = M1, C1_2 = C1_1, C2_2 = C2_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [child1]) :-
    C1_2 is 1 - C1_1, M2 = M1, W2 = W1, C2_2 = C2_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [child2]) :-
    C2_2 is 1 - C2_1, M2 = M1, W2 = W1, C1_2 = C1_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [child1, child2]) :-
    C1_2 is 1 - C1_1, C2_2 is 1 - C2_1, M2 = M1, W2 = W1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [man, child1]) :-
    M2 is 1 - M1, C1_2 is 1 - C1_1, W2 = W1, C2_2 = C2_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [man, child2]) :-
    M2 is 1 - M1, C2_2 is 1 - C2_1, W2 = W1, C1_2 = C1_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [woman, child1]) :-
    W2 is 1 - W1, C1_2 is 1 - C1_1, M2 = M1, C2_2 = C2_1.
update_state(state(M1, W1, C1_1, C2_1, _), state(M2, W2, C1_2, C2_2, Boat), [woman, child2]) :-
    W2 is 1 - W1, C2_2 is 1 - C2_1, M2 = M1, C1_2 = C1_1.

solve_crossing :-
    initial_state(StartState),
    goal_state(GoalState),
    cross_river(StartState, GoalState, [StartState]).

cross_river(GoalState, GoalState, _VisitedStates) :-
    write('All crossed the river successfully!'), nl.

cross_river(CurrentState, GoalState, VisitedStates) :-
    move(CurrentState, NextState, People),
    valid_state(NextState),
    \+ member(NextState, VisitedStates),
    write('Move: '), write(People), write(' -> '), write(NextState), nl,
    cross_river(NextState, GoalState, [NextState | VisitedStates]).
