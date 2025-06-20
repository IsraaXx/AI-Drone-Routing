                   % Grid Size
grid(5,5).

% Obstacles (O)
o(0, 4).
o(1, 1).
o(2, 2).
o(3, 1).
o(4, 3).

% Delivery points (P)
p(0, 2).
p(1, 4).
p(2, 3).
p(3, 0).
p(4, 2).



%----------------Movement rules: up, down, left, right--------------------

move((X,Y), (R,Y)) :- R is X-1.  % Up
move((X,Y), (R,Y)) :- R is X+1.  % Down
move((X,Y), (X,C)) :- C is Y-1.  % Left
move((X,Y), (X,C)) :- C is Y+1.  % Right

% Valid move: inside grid and not obstacle
validMove((X,Y)) :-
    grid(Rows, Cols),
    X >= 0, X < Rows,
    Y >= 0, Y < Cols,
    \+ o(X,Y).

% To Check if current position is a delivery point
checkDP((X,Y)) :- p(X,Y).

% ---------------- DFS Search -------------------------------------------

% Base case: No more (Valid and unvisited) moves
% and still need to make sure if the current position is a delivery point or not
dfs(Current, Visited, Delivery, [Current], Count) :-
    \+ ( move(Current, Next),
    validMove(Next),
    \+ member(Next, Visited) ),
    (checkDP(Current), \+ member(Current, Delivery) -> Count = 1 ; Count = 0).

% Recursive case: current is a delivery point take it and move to next
dfs(Current, Visited, Delivery, [Current|Path], Count) :-
    checkDP(Current),
    \+ member(Current, Delivery),
    append(Delivery, [Current], NewDelivery),
    move(Current, Next),
    validMove(Next),
    \+ member(Next, Visited),
    append(Visited, [Next], NewVisited),
    dfs(Next, NewVisited, NewDelivery, Path, NewCount),
    Count is NewCount + 1.

% Recursive case: not a delivery point but still need to move to next
dfs(Current, Visited, Delivery, [Current|Path], Count) :-
    \+ checkDP(Current),
    move(Current, Next),
    validMove(Next),
    \+ member(Next, Visited),
    append(Visited, [Next], NewVisited),
    dfs(Next, NewVisited, Delivery, Path, Count).

% Start search
search(Start, Path, Count) :-
    dfs(Start, [Start], [], Path, Count).

%----------------------------Finding best path------------------------------------------
% Find all paths from (0,0)
find_all_paths(Paths) :-
    findall((Path, Count), search((0,0), Path, Count), Paths).

% Find the path with maximum delivery points
find_best_path(BestPath, MaxCount) :-
    find_all_paths(Paths),
    find_max_path(Paths, ([], 0), (BestPath, MaxCount)).

% base case: when no more paths, the current best is the final best.
find_max_path([], Best, Best).

% Recursive case: if current path has greater count, update the best.
find_max_path([(Path, Count)|Rest], (_, CurrentBestCount), Best) :-
    Count > CurrentBestCount,
    !,  % Cut: if Count > CurrentBestCount is true, dont try next clauses
    find_max_path(Rest, (Path, Count), Best).

% Recursive case: if not greater, continue with current best.
find_max_path([_|Rest], BestSoFar, Best) :-
    find_max_path(Rest, BestSoFar, Best).

%---------------printing BestPath---------------------------------------
print_best_path :-
    find_best_path(BestPath, MaxCount),
    write('Best path :'), nl,
    print_path(BestPath),
    nl,
    write('Number of delivery points visited: '), write(MaxCount), nl.

% baseCase
print_path([]):- !.

% This handles the last point in the path (X,Y).
print_path([(X,Y)]) :-
    write('('), write(X), write(','), write(Y), write(')'), !.

% This handles the general case: when the path has at least two points.
print_path([(X,Y)|Rest]) :-
    write('('), write(X), write(','), write(Y), write(')'),
    write(' -> '),
    print_path(Rest), !.
% ------------ Drawing Functions ---------------------------------------

draw_grid(Current, Visited) :-
    grid(Rows, Cols),
    RowsRange is Rows - 1,
    ColsRange is Cols - 1,
    between(0, RowsRange, Row),    % range from 0 to 4
    between(0, ColsRange, Col),
    draw_cell(Row, Col, Current, Visited),
    (Col =:= ColsRange -> nl ; true),  % After printing a full row (last column), do a newline nl to move to the next row.
    fail.  % forces Prolog to backtrack, meaning generate next Row and Col and keep drawing until all grid is printed.

draw_grid(_, _).

draw_cell(Row, Col, (Row,Col), _) :- write('D '), !.
draw_cell(Row, Col, _, Visited) :- member((Row,Col), Visited), write('* '), !.
draw_cell(Row, Col, _, _) :- o(Row,Col), write('O '), !.
draw_cell(Row, Col, _, _) :- p(Row,Col), write('P '), !.
draw_cell(_, _, _, _) :- write('- ').

% Draw Each Step
% Base case: No more steps to draw
draw_steps([], _).
draw_steps([Current|Rest], Visited) :-
    nl, write('Step: '), nl,
    draw_grid(Current, Visited),
    draw_steps(Rest, [Current|Visited]).

% --- Main Runner ----------------
solve :-
    find_best_path(Path, Count),
    nl,write('Drone Route Steps: '), nl,
    draw_steps(Path, []),
    nl, write('Final Grid: '), nl,
    last(Path, LastPos),
    draw_grid(LastPos, Path),
    nl, write('Number of Delivery Points Visited: '), write(Count).

%-----TO RUN CODE-----------------
% print_best_path.
% solve.

% -------------------------------A* Search------------------------------------
% Drone start position (D)
start_pos(0, 0).

% Check valid position and handle recharging
valid_pos((X,Y), E, MaxEnergy, NewE) :-
    grid(Rows, Cols),
    X >= 0, X < Rows,
    Y >= 0, Y < Cols,
    \+ o(X, Y),
    (recharge_station(X, Y) -> NewE = MaxEnergy ; NewE is E - 1),
    NewE >= 0.

% Recharge station (R)
recharge_station(3, 3).

% Get initial energy from the user
get_initial_energy(Energy) :-
    write('Enter initial energy : '),
    read(Energy),
    Energy > 0.

% Manhattan distance heuristic
heuristic((X,Y), DeliveryPoints, H) :-
    findall(Dist, (member((PX,PY), DeliveryPoints), Dist is abs(X-PX) + abs(Y-PY)), Dists),
    (Dists = [] -> H = 0 ; min_list(Dists, H)).

% Print the initial grid
print_initial_grid :-
    grid(Rows, Cols),
    MaxRow is Rows - 1,
    MaxCol is Cols - 1,
    start_pos(SX, SY),
    forall(between(0, MaxRow, X), (
        forall(between(0, MaxCol, Y), (
            (o(X, Y) -> write(' ??');
             p(X, Y) -> write(' ?? ');
             recharge_station(X, Y) -> write(' ?? ');
             X =:= SX, Y =:= SY -> write(' ?? ');
             true -> write(' _ '))
        )),
        nl
    )).

% A* Search with enhanced output
astar :-
    format('=== Initial Grid ===~n', []),
    print_initial_grid,
    get_initial_energy(MaxE),
    start_pos(SX, SY),
    findall((X,Y), p(X,Y), DeliveryPoints),
    heuristic((SX,SY), DeliveryPoints, H),
    format('~nStarting A* Search with initial energy: ~w~n', [MaxE]),
    format('Initial position: (~w,~w)~n', [SX, SY]),
    format('Delivery points: ~w~n~n', [DeliveryPoints]),
    search([[[(SX,SY)|DeliveryPoints], null, 0, H, H, MaxE]], [], DeliveryPoints, MaxE).


% base case: no more packages left to collect
search(Open, Closed, _, _) :-
    get_best(Open, Current, _),
    Current = [State, _, G, _, _, E],
    State = [Pos|Remaining],
    Remaining = [],
    format('=== SOLUTION STEPS ===~n', []),
    printSolution(Current, Closed),
    format('~n~n=== FINAL STATE ===~n', []),
    format('?? All packages collected! Final position: ~w~n', [Pos]),
    format('Total path cost: ~w~n', [G]),
    format('Remaining energy: ~w~n~n', [E]),!.

% recursive case: get the best child in open list then get its children and so on...
search(Open, Closed, Goals, MaxE) :-
    get_best(Open, Current, RestOpen),
    get_children(Current, RestOpen, Closed, Goals, Children, MaxE),
    add_children(Children, RestOpen, NewOpen), % add children to open list
    append(Closed, [Current], NewClosed), % explored
    (NewOpen = [] -> % in case no valid children could be obtained
        Current = [_, _, G, _, _, _],
        format(' Printing partial solution...~n', []),
        printSolution(Current, Closed),
        format('Total path cost: ~w~n', [G]),
        format('~n ?? No more valid moves. ~n', []),!
    ;
        search(NewOpen, NewClosed, Goals, MaxE)
    ).

get_best(Open, BestChild, Rest) :-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

get_children(Node, Open, Closed, _, Children, MaxE) :-
    Node = [[Pos|Remaining], ParentState, G, _, _, E], % state representation
    findall(Child, (
        move(Pos, NewPos),
        valid_pos(NewPos, E, MaxE, NewE),
        \+ (ParentState = [NewPos|_]),
        (member(NewPos, Remaining) ->
            select(NewPos, Remaining, NewRemaining)
        ;
            NewRemaining = Remaining
        ),
        NewG is G + 1,
        heuristic(NewPos, NewRemaining, NewH),
        NewF is NewG + NewH,
        Child = [[NewPos|NewRemaining], [Pos|Remaining], NewG, NewH, NewF, NewE],
        \+ worse_or_equal_node([NewPos|NewRemaining], NewF, Open),
        \+ worse_or_equal_node([NewPos|NewRemaining], NewF, Closed)
    ), Children).

add_children(Children, Open, NewOpen) :-
    append(Open, Children, NewOpen).

findMin([X], X) :- !.

findMin([Head|T], Min) :-
    findMin(T, TmpMin),
    Head = [_,_,_,_,HeadF,_],
    TmpMin = [_,_,_,_,TmpF,_],
    (TmpF =< HeadF -> Min = TmpMin ; Min = Head).

worse_or_equal_node(State, F, List) :-
    member([State, _, _, _, ExistingF, _], List),
    ExistingF =< F.

printSolution([State, null, _G, _H, _F, E], _) :-
    State = [(X,Y)|_],
    format('?? Start at (~w,~w) - Energy: ~w~n', [X, Y, E]).

printSolution([State, Parent, _, _H, _F, E], Closed) :-
    member([Parent, GrandParent, PrevG, _, _, PE], Closed),
    printSolution([Parent, GrandParent, PrevG, _, _, PE], Closed),
    State = [(X,Y)|Remaining],
    Parent = [(_PX,_PY)|ParentRemaining],
    (member((X,Y), ParentRemaining), \+ member((X,Y), Remaining) ->
        format('?? Position: (~w,~w) - ?? Delivery collected! - Energy: ~w~n', [X, Y, E])
      ; recharge_station(X, Y) ->
        format('?? Position: (~w,~w) - ?? Recharged! - Energy: ~w~n', [X, Y, E])
      ; format('?? Position: (~w,~w) - Energy: ~w~n', [X, Y, E])
    ).

