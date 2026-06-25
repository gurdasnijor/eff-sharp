module GraphSpec

open Effect
open Effect.Vitest

let private edge s t d : Edge<int> = { Source = s; Target = t; Data = d }

let private same actual expected =
    toBe (actual = expected) true

describe "Graph" (fun () ->
    test "constructs empty graphs, renders, and iterates node entries" (fun () ->
        let directed = Graph.directed<string, int> None
        toEqual (Graph.kind directed) Directed
        toBe (Graph.nodeCount directed) 0
        toBe (Graph.edgeCount directed) 0

        let undirected = Graph.undirected<string, int> None
        toEqual (Graph.kind undirected) Undirected
        toBe (Graph.nodeCount undirected) 0
        toBe (Graph.edgeCount undirected) 0

        let rendered =
            Graph.directed<int, int> (
                Some(fun m ->
                    let a = Graph.addNode m 0
                    let b = Graph.addNode m 0
                    Graph.addEdge m a b 1 |> ignore)
            )

        toBe (Graph.render rendered) "Graph(directed, 2, 1)"

        let graph =
            Graph.directed<string, int> (
                Some(fun m ->
                    Graph.addNode m "Node A" |> ignore
                    Graph.addNode m "Node B" |> ignore
                    Graph.addNode m "Node C" |> ignore)
            )

        same (Graph.toNodeEntries graph |> List.toArray) [| (0, "Node A"); (1, "Node B"); (2, "Node C") |])

    test "mutation lifecycle and node queries work" (fun () ->
        let graph = Graph.directed<string, int> None
        let m = Graph.beginMutation graph
        toEqual (Graph.kind m) Directed
        toBe (Graph.nodeCount graph) (Graph.nodeCount m)

        let result = Graph.endMutation m
        toEqual (Graph.kind result) Directed
        toBe (Graph.nodeCount result) 0

        let noop = Graph.mutate graph (fun _ -> ())
        same noop graph

        let withNode =
            Graph.mutate (Graph.directed<string, int> None) (fun m -> Graph.addNode m "Node A" |> ignore)

        toBe (Graph.nodeCount withNode) 1
        same (Graph.getNode withNode 0) (Some "Node A")

        let queried =
            Graph.directed<string, int> (
                Some(fun m ->
                    Graph.addNode m "Start A" |> ignore
                    Graph.addNode m "Node B" |> ignore
                    Graph.addNode m "Start C" |> ignore
                    Graph.addNode m "Start D" |> ignore)
            )

        toBe (Graph.hasNode queried 0) true
        toBe (Graph.hasNode queried 999) false
        same (Graph.findNode queried (fun d -> d.StartsWith "Start")) (Some 0)
        same (Graph.findNode queried (fun d -> d = "missing")) None
        same (Graph.findNodes queried (fun d -> d.StartsWith "Start")) [ 0; 2; 3 ])

    test "edge queries updates maps and reverse work" (fun () ->
        let graph =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b 10 |> ignore
                    Graph.addEdge m b c 20 |> ignore
                    Graph.addEdge m c a 30 |> ignore
                    Graph.addEdge m a c 25 |> ignore)
            )

        same (Graph.findEdge graph (fun d -> d = 20)) (Some 1)
        same (Graph.findEdge graph (fun d -> d = 99)) None
        same (Graph.findEdges graph (fun d -> d >= 20)) [ 1; 2; 3 ]

        let updated =
            Graph.mutate graph (fun m ->
                Graph.updateNode m 0 (fun d -> d.ToLower())
                Graph.updateEdge m 1 (fun d -> d * 2)
                Graph.updateEdge m 999 (fun d -> d * 2))

        same (Graph.getNode updated 0) (Some "a")
        same (Graph.getEdge updated 1) (Some(edge 1 2 40))

        let mapped =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "node a"
                    let b = Graph.addNode m "node b"
                    Graph.addEdge m a b 10 |> ignore
                    Graph.mapNodes m (fun d -> d.ToUpper())
                    Graph.mapEdges m (fun d -> d * 2))
            )

        same (Graph.getNode mapped 0) (Some "NODE A")
        same (Graph.getEdge mapped 0) (Some(edge 0 1 20))

        let reversed =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b 1 |> ignore
                    Graph.addEdge m b c 2 |> ignore
                    Graph.addEdge m c a 3 |> ignore
                    Graph.reverse m)
            )

        same (Graph.getEdge reversed 0) (Some(edge 1 0 1))
        same (Graph.getEdge reversed 1) (Some(edge 2 1 2))
        same (Graph.getEdge reversed 2) (Some(edge 0 2 3))
        same (Graph.neighbors reversed 0) [ 2 ]
        same (Graph.neighbors reversed 1) [ 0 ])

    test "filters nodes and edges while maintaining adjacency" (fun () ->
        let filterMapped =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "keep"
                    let b = Graph.addNode m "remove"
                    let c = Graph.addNode m "keep"
                    Graph.addEdge m a b 1 |> ignore
                    Graph.addEdge m b c 2 |> ignore
                    Graph.addEdge m a c 3 |> ignore
                    Graph.filterMapNodes m (fun d -> if d = "keep" then Some d else None))
            )

        toBe (Graph.nodeCount filterMapped) 2
        toBe (Graph.edgeCount filterMapped) 1
        same (Graph.getEdge filterMapped 2) (Some(edge 0 2 3))
        same (Graph.getEdge filterMapped 0) None

        let edgeFiltered =
            Graph.directed<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b "primary" |> ignore
                    Graph.addEdge m a c "secondary" |> ignore
                    Graph.addEdge m b c "primary" |> ignore
                    Graph.filterEdges m (fun d -> d = "primary"))
            )

        toBe (Graph.edgeCount edgeFiltered) 2
        same (Graph.neighbors edgeFiltered 0) [ 1 ]
        same (Graph.neighbors edgeFiltered 1) [ 2 ]
        same (Graph.neighbors edgeFiltered 2) [])

    test "edge CRUD and adjacency queries work for directed and undirected graphs" (fun () ->
        let graph =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    Graph.addNode m "C" |> ignore
                    Graph.addEdge m a b 42 |> ignore)
            )

        same (Graph.getEdge graph 0) (Some(edge 0 1 42))
        same (Graph.getEdge graph 999) None
        toBe (Graph.hasEdge graph 0 1) true
        toBe (Graph.hasEdge graph 0 2) false
        same (Graph.neighbors graph 0) [ 1 ]
        same (Graph.neighbors graph 1) []
        same (Graph.successors graph 0) [ 1 ]
        same (Graph.predecessors graph 1) [ 0 ]
        same (Graph.neighborsDirected graph 1 Incoming) [ 0 ]
        same (Graph.neighborsDirected graph 0 Outgoing) [ 1 ]
        same (Graph.nodes graph) [ 0; 1; 2 ]
        same (Graph.edges graph) [ 0 ]

        let undirected =
            Graph.undirected<int, unit> (
                Some(fun m ->
                    Graph.addNode m 0 |> ignore
                    Graph.addNode m 1 |> ignore
                    Graph.addNode m 2 |> ignore
                    Graph.addEdge m 0 1 () |> ignore
                    Graph.addEdge m 0 1 () |> ignore
                    Graph.addEdge m 1 2 () |> ignore)
            )

        toBe (Graph.hasEdge undirected 0 1) true
        toBe (Graph.hasEdge undirected 1 0) true
        same (Graph.neighbors undirected 0) [ 1 ]
        same (Graph.neighbors undirected 1 |> List.sort) [ 0; 2 ]
        same (Graph.neighbors undirected 2) [ 1 ]
        toThrow (fun () -> Graph.successors undirected 0 |> ignore)
        toThrow (fun () -> Graph.predecessors undirected 0 |> ignore)
        toThrow (fun () -> Graph.neighborsDirected undirected 0 Outgoing |> ignore))

    test "throws for invalid edge operations and removes nodes and edges" (fun () ->
        toThrow (fun () ->
            Graph.directed<string, int> (
                Some(fun m ->
                    let b = Graph.addNode m "B"
                    Graph.addEdge m 999 b 42 |> ignore)
            )
            |> ignore)

        let removedNode =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b 10 |> ignore
                    Graph.addEdge m b c 20 |> ignore
                    Graph.addEdge m c a 30 |> ignore
                    Graph.removeNode m b)
            )

        toBe (Graph.nodeCount removedNode) 2
        toBe (Graph.edgeCount removedNode) 1

        let removedEdge =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let e = Graph.addEdge m a b 42
                    Graph.removeEdge m e
                    Graph.removeEdge m 999)
            )

        toBe (Graph.edgeCount removedEdge) 0)

    test "detects acyclic and bipartite graphs" (fun () ->
        let dag =
            Graph.directed<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    let d = Graph.addNode m "D"
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m a c "" |> ignore
                    Graph.addEdge m b d "" |> ignore
                    Graph.addEdge m c d "" |> ignore)
            )

        toBe (Graph.isAcyclic dag) true

        let cycle =
            Graph.directed<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m b c "" |> ignore
                    Graph.addEdge m c a "" |> ignore)
            )

        toBe (Graph.isAcyclic cycle) false

        let square =
            Graph.undirected<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    let d = Graph.addNode m "D"
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m b c "" |> ignore
                    Graph.addEdge m c d "" |> ignore
                    Graph.addEdge m d a "" |> ignore)
            )

        toBe (Graph.isBipartite square) true

        let triangle =
            Graph.undirected<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m b c "" |> ignore
                    Graph.addEdge m c a "" |> ignore)
            )

        toBe (Graph.isAcyclic triangle) false
        toBe (Graph.isBipartite triangle) false)

    test "finds connected and strongly connected components" (fun () ->
        let graph =
            Graph.undirected<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    let d = Graph.addNode m "D"
                    Graph.addNode m "E" |> ignore
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m c d "" |> ignore)
            )

        let components =
            Graph.connectedComponents graph
            |> List.sortBy (fun c -> (List.length c, List.head c))

        toBe (List.length components) 3
        same components.[0] [ 4 ]
        toBe (List.length components.[1]) 2
        toBe (List.length components.[2]) 2

        let directed =
            Graph.directed<string, string> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    let d = Graph.addNode m "D"
                    Graph.addEdge m a b "" |> ignore
                    Graph.addEdge m b c "" |> ignore
                    Graph.addEdge m c a "" |> ignore
                    Graph.addEdge m b d "" |> ignore)
            )

        let sccs = Graph.stronglyConnectedComponents directed |> List.sortBy List.length
        toBe (List.length sccs) 2
        same sccs.[0] [ 3 ]
        same (List.sort sccs.[1]) [ 0; 1; 2 ]
        toThrow (fun () -> Graph.stronglyConnectedComponents graph |> ignore))

    test "dijkstra finds paths and validates input" (fun () ->
        let graph =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b 5 |> ignore
                    Graph.addEdge m a c 10 |> ignore
                    Graph.addEdge m b c 2 |> ignore)
            )

        same
            (Graph.dijkstra graph 0 2 float)
            (Some
                { Path = [ 0; 1; 2 ]
                  Distance = 7.0
                  Costs = [ 5; 2 ] })

        same (Graph.dijkstra graph 2 0 float) None
        same
            (Graph.dijkstra graph 0 0 float)
            (Some
                { Path = [ 0 ]
                  Distance = 0.0
                  Costs = [] })

        let negative =
            Graph.directed<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    Graph.addEdge m a b -1 |> ignore)
            )

        toThrow (fun () -> Graph.dijkstra negative 0 1 float |> ignore)
        toThrow (fun () -> Graph.dijkstra (Graph.directed<string, int> None) 0 1 float |> ignore)

        let undirected =
            Graph.undirected<string, int> (
                Some(fun m ->
                    let a = Graph.addNode m "A"
                    let b = Graph.addNode m "B"
                    let c = Graph.addNode m "C"
                    Graph.addEdge m a b 1 |> ignore
                    Graph.addEdge m c b 1 |> ignore)
            )

        same
            (Graph.dijkstra undirected 0 2 float)
            (Some
                { Path = [ 0; 1; 2 ]
                  Distance = 2.0
                  Costs = [ 1; 1 ] })))
