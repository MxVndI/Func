prim(Graph, MST) :-
    Graph = [FirstEdge|_], 
    FirstEdge = edge(V1, V2, _),
    Visited = [V1, V2],
    MST0 = [FirstEdge],
    prim_loop(Graph, Visited, MST0, MST).

prim_loop(_, Visited, MST, MST) :-
    all_vertices_visited(Visited), !.

prim_loop(Graph, Visited, CurrentMST, MST) :-
    find_min_edge(Graph, Visited, MinEdge),
    MinEdge = edge(V1, V2, _),
    (member(V1, Visited) -> NewVisited = [V2|Visited] ; NewVisited = [V1|Visited]),
    append(CurrentMST, [MinEdge], NewMST),
    prim_loop(Graph, NewVisited, NewMST, MST).

all_vertices_visited(Visited) :-
    findall(V, (edge(V, _, _); edge(_, V, _)), AllVertices),
    sort(AllVertices, SortedVertices),
    sort(Visited, SortedVisited),
    SortedVertices == SortedVisited.

find_min_edge(Graph, Visited, MinEdge) :-
    findall(Edge, 
            (member(Edge, Graph),
             Edge = edge(V1, V2, _),
             (member(V1, Visited), \+ member(V2, Visited)) ;
             (member(V2, Visited), \+ member(V1, Visited))),
            Edges),
    find_min_weight(Edges, MinEdge).

find_min_weight([Edge], Edge) :- !.
find_min_weight([Edge1|Rest], MinEdge) :-
    find_min_weight(Rest, Edge2),
    Edge1 = edge(_, _, W1),
    Edge2 = edge(_, _, W2),
    (W1 < W2 -> MinEdge = Edge1 ; MinEdge = Edge2).

graph([
    edge(a, b, 4),
    edge(a, h, 8),
    edge(b, c, 8),
    edge(b, h, 11),
    edge(c, d, 7),
    edge(c, f, 4),
    edge(c, i, 2),
    edge(d, e, 9),
    edge(d, f, 14),
    edge(e, f, 10),
    edge(f, g, 2),
    edge(g, h, 1),
    edge(g, i, 6),
    edge(h, i, 7)
]).


%для запуска graph(G), prim(G, MST).