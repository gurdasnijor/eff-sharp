module Effect.Tests.GraphTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Graph.test.ts
//
// Behaviour, not JS phrasing. This ports the core surface + the algorithms
// isAcyclic / isBipartite / connectedComponents / stronglyConnectedComponents /
// dijkstra. Visualisation (toGraphViz/toMermaid), the lazy traversal iterators
// (dfs/bfs/topo/dfsPostOrder/externals/Walker), the extra path-finders
// (astar/bellmanFord/floydWarshall), the JS `isGraph` guard / toJSON / inspect,
// and the JS `undefined`-data tests are intentionally not ported (see Graph.fs).

let private edge s t d : Edge<int> = { Source = s; Target = t; Data = d }

// --- constructors ---

[<Fact>]
let ``empty directed graph`` () =
    let graph = Graph.directed<string, int> None
    Assert.Equal(Directed, Graph.kind graph)
    Assert.Equal(0, Graph.nodeCount graph)
    Assert.Equal(0, Graph.edgeCount graph)

[<Fact>]
let ``empty undirected graph`` () =
    let graph = Graph.undirected<string, int> None
    Assert.Equal(Undirected, Graph.kind graph)
    Assert.Equal(0, Graph.nodeCount graph)
    Assert.Equal(0, Graph.edgeCount graph)

[<Fact>]
let ``toString`` () =
    let graph =
        Graph.directed<int, int> (Some(fun m ->
            let a = Graph.addNode m 0
            let b = Graph.addNode m 0
            Graph.addEdge m a b 1 |> ignore))
    Assert.Equal("Graph(directed, 2, 1)", Graph.render graph)

[<Fact>]
let ``iterates node entries in index order`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "Node A" |> ignore
            Graph.addNode m "Node B" |> ignore
            Graph.addNode m "Node C" |> ignore))
    Assert.Equal<(int * string)[]>(
        [| (0, "Node A"); (1, "Node B"); (2, "Node C") |], Graph.toNodeEntries graph |> List.toArray)

// --- mutation lifecycle ---

[<Fact>]
let ``beginMutation copies node and edge counts`` () =
    let graph = Graph.directed<string, int> None
    let m = Graph.beginMutation graph
    Assert.Equal(Directed, Graph.kind m)
    Assert.Equal(Graph.nodeCount graph, Graph.nodeCount m)

[<Fact>]
let ``endMutation converts back to immutable`` () =
    let graph = Graph.directed<string, int> None
    let m = Graph.beginMutation graph
    let result = Graph.endMutation m
    Assert.Equal(Directed, Graph.kind result)
    Assert.Equal(0, Graph.nodeCount result)

[<Fact>]
let ``mutate creates a new equal graph for a no-op`` () =
    let graph = Graph.directed<string, int> None
    let result = Graph.mutate graph (fun _ -> ())
    Assert.True((result = graph))
    Assert.Equal(0, Graph.nodeCount result)

// --- node CRUD / queries ---

[<Fact>]
let ``addNode returns index and stores data`` () =
    let result = Graph.mutate (Graph.directed<string, int> None) (fun m -> Graph.addNode m "Node A" |> ignore)
    Assert.Equal(1, Graph.nodeCount result)
    Assert.Equal(Some "Node A", Graph.getNode result 0)

[<Fact>]
let ``getNode returns data or None`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "Node A" |> ignore
            Graph.addNode m "Node B" |> ignore))
    Assert.Equal(Some "Node A", Graph.getNode graph 0)
    Assert.Equal(Some "Node B", Graph.getNode graph 1)
    Assert.Equal(None, Graph.getNode graph 999)

[<Fact>]
let ``hasNode`` () =
    let graph = Graph.directed<string, int> (Some(fun m -> Graph.addNode m "Node A" |> ignore))
    Assert.True(Graph.hasNode graph 0)
    Assert.False(Graph.hasNode graph 999)
    Assert.False(Graph.hasNode graph -1)

[<Fact>]
let ``nodeCount tracks additions inside the builder`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Assert.Equal(0, Graph.nodeCount m)
            Graph.addNode m "A" |> ignore
            Assert.Equal(1, Graph.nodeCount m)
            Graph.addNode m "B" |> ignore
            Assert.Equal(2, Graph.nodeCount m)))
    Assert.Equal(2, Graph.nodeCount graph)

[<Fact>]
let ``findNode and findNodes`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "Start A" |> ignore
            Graph.addNode m "Node B" |> ignore
            Graph.addNode m "Start C" |> ignore
            Graph.addNode m "Start D" |> ignore))
    Assert.Equal(Some 0, Graph.findNode graph (fun d -> d.StartsWith "Start"))
    Assert.Equal(None, Graph.findNode graph (fun d -> d = "missing"))
    Assert.Equal<int list>([ 0; 2; 3 ], Graph.findNodes graph (fun d -> d.StartsWith "Start"))

[<Fact>]
let ``findEdge and findEdges`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 10 |> ignore
            Graph.addEdge m b c 20 |> ignore
            Graph.addEdge m c a 30 |> ignore
            Graph.addEdge m a c 25 |> ignore))
    Assert.Equal(Some 1, Graph.findEdge graph (fun d -> d = 20))
    Assert.Equal(None, Graph.findEdge graph (fun d -> d = 99))
    Assert.Equal<int list>([ 1; 2; 3 ], Graph.findEdges graph (fun d -> d >= 20))

[<Fact>]
let ``updateNode updates or ignores`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "Node A" |> ignore
            Graph.addNode m "Node B" |> ignore
            Graph.updateNode m 0 (fun d -> d.ToUpper())
            Graph.updateNode m 999 (fun d -> d.ToUpper())))
    Assert.Equal(Some "NODE A", Graph.getNode graph 0)
    Assert.Equal(Some "Node B", Graph.getNode graph 1)

[<Fact>]
let ``updateEdge updates or ignores`` () =
    let graph =
        Graph.mutate (Graph.directed<string, int> None) (fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let e = Graph.addEdge m a b 10
            Graph.updateEdge m e (fun d -> d * 2)
            Graph.updateEdge m 999 (fun d -> d * 2))
    Assert.Equal(Some(edge 0 1 20), Graph.getEdge graph 0)

[<Fact>]
let ``mapNodes transforms every node`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "node a" |> ignore
            Graph.addNode m "node b" |> ignore
            Graph.addNode m "node c" |> ignore
            Graph.mapNodes m (fun d -> d.ToUpper())))
    Assert.Equal(Some "NODE A", Graph.getNode graph 0)
    Assert.Equal(Some "NODE B", Graph.getNode graph 1)
    Assert.Equal(Some "NODE C", Graph.getNode graph 2)

[<Fact>]
let ``mapEdges transforms every edge`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 10 |> ignore
            Graph.addEdge m b c 20 |> ignore
            Graph.addEdge m c a 30 |> ignore
            Graph.mapEdges m (fun d -> d * 2)))
    Assert.Equal(20, (Graph.getEdge graph 0).Value.Data)
    Assert.Equal(40, (Graph.getEdge graph 1).Value.Data)
    Assert.Equal(60, (Graph.getEdge graph 2).Value.Data)

// --- reverse ---

[<Fact>]
let ``reverse flips edge directions`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m b c 2 |> ignore
            Graph.addEdge m c a 3 |> ignore
            Graph.reverse m))
    Assert.Equal(Some(edge 1 0 1), Graph.getEdge graph 0)
    Assert.Equal(Some(edge 2 1 2), Graph.getEdge graph 1)
    Assert.Equal(Some(edge 0 2 3), Graph.getEdge graph 2)

[<Fact>]
let ``reverse updates adjacency lists`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m a c 2 |> ignore
            Graph.reverse m))
    Assert.Equal<int list>([], Graph.neighbors graph 0)
    Assert.Equal<int list>([ 0 ], Graph.neighbors graph 1)
    Assert.Equal<int list>([ 0 ], Graph.neighbors graph 2)

[<Fact>]
let ``reverse preserves adjacency when adding edges afterwards`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 1 |> ignore
            Graph.reverse m
            Graph.addEdge m a b 2 |> ignore))
    Assert.Equal(2, Graph.edgeCount graph)
    Assert.Equal<int list>([ 1 ], Graph.neighbors graph 0)
    Assert.True(Graph.hasEdge graph 0 1)
    Assert.True(Graph.hasEdge graph 1 0)

[<Fact>]
let ``reverse is a no-op for undirected graphs`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 1 |> ignore
            Graph.reverse m))
    Assert.Equal<int list>([ 1 ], Graph.neighbors graph 0)
    Assert.Equal<int list>([ 0 ], Graph.neighbors graph 1)

// --- filter / filterMap ---

[<Fact>]
let ``filterMapNodes filters and transforms, removing connected edges`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "keep"
            let b = Graph.addNode m "remove"
            let c = Graph.addNode m "keep"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m b c 2 |> ignore
            Graph.addEdge m a c 3 |> ignore
            Graph.filterMapNodes m (fun d -> if d = "keep" then Some d else None)))
    Assert.Equal(2, Graph.nodeCount graph)
    Assert.Equal(1, Graph.edgeCount graph)
    Assert.Equal(Some(edge 0 2 3), Graph.getEdge graph 2)
    Assert.Equal(None, Graph.getEdge graph 0)
    Assert.Equal(None, Graph.getEdge graph 1)

[<Fact>]
let ``filterMapEdges filters and transforms`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 5 |> ignore
            Graph.addEdge m b c 15 |> ignore
            Graph.addEdge m c a 25 |> ignore
            Graph.filterMapEdges m (fun d -> if d >= 10 then Some(d * 2) else None)))
    Assert.Equal(2, Graph.edgeCount graph)
    Assert.Equal(3, Graph.nodeCount graph)
    Assert.Equal(30, (Graph.getEdge graph 1).Value.Data)
    Assert.Equal(50, (Graph.getEdge graph 2).Value.Data)
    Assert.Equal(None, Graph.getEdge graph 0)

[<Fact>]
let ``filterNodes removes connected edges`` () =
    let graph =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "keep"
            let b = Graph.addNode m "remove"
            let c = Graph.addNode m "keep"
            Graph.addEdge m a b "A-B" |> ignore
            Graph.addEdge m b c "B-C" |> ignore
            Graph.addEdge m a c "A-C" |> ignore
            Graph.filterNodes m (fun d -> d = "keep")))
    Assert.Equal(2, Graph.nodeCount graph)
    Assert.Equal(1, Graph.edgeCount graph)
    Assert.Equal(Some { Source = 0; Target = 2; Data = "A-C" }, Graph.getEdge graph 2)

[<Fact>]
let ``filterEdges updates adjacency lists`` () =
    let graph =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b "primary" |> ignore
            Graph.addEdge m a c "secondary" |> ignore
            Graph.addEdge m b c "primary" |> ignore
            Graph.filterEdges m (fun d -> d = "primary")))
    Assert.Equal(2, Graph.edgeCount graph)
    Assert.Equal<int list>([ 1 ], Graph.neighbors graph 0)
    Assert.Equal<int list>([ 2 ], Graph.neighbors graph 1)
    Assert.Equal<int list>([], Graph.neighbors graph 2)

// --- edge CRUD ---

[<Fact>]
let ``addEdge returns sequential indices`` () =
    let mutable indices = []
    Graph.directed<string, int> (Some(fun m ->
        let a = Graph.addNode m "A"
        let b = Graph.addNode m "B"
        let c = Graph.addNode m "C"
        indices <- [ Graph.addEdge m a b 10; Graph.addEdge m b c 20; Graph.addEdge m a c 30 ]))
    |> ignore
    Assert.Equal<int list>([ 0; 1; 2 ], indices)

[<Fact>]
let ``addEdge throws for missing endpoints`` () =
    let ex1 =
        Assert.Throws<GraphError>(fun () ->
            Graph.directed<string, int> (Some(fun m ->
                let b = Graph.addNode m "B"
                Graph.addEdge m 999 b 42 |> ignore))
            |> ignore)
    Assert.Equal("Node 999 does not exist", ex1.Message)
    Assert.Throws<GraphError>(fun () ->
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            Graph.addEdge m a 999 42 |> ignore))
        |> ignore)
    |> ignore

[<Fact>]
let ``removeNode removes incident edges`` () =
    let result =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 10 |> ignore
            Graph.addEdge m b c 20 |> ignore
            Graph.addEdge m c a 30 |> ignore
            Graph.removeNode m b))
    Assert.Equal(2, Graph.nodeCount result)
    Assert.Equal(1, Graph.edgeCount result)

[<Fact>]
let ``removeNode of a missing node is a no-op`` () =
    let result =
        Graph.directed<string, int> (Some(fun m ->
            Graph.addNode m "A" |> ignore
            Graph.removeNode m 999))
    Assert.Equal(1, Graph.nodeCount result)

[<Fact>]
let ``removeEdge`` () =
    let result =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let e = Graph.addEdge m a b 42
            Graph.removeEdge m e))
    Assert.Equal(0, Graph.edgeCount result)

[<Fact>]
let ``removeEdge of a missing edge is a no-op`` () =
    let result =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 42 |> ignore
            Graph.removeEdge m 999))
    Assert.Equal(1, Graph.edgeCount result)

[<Fact>]
let ``getEdge and hasEdge`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addNode m "C" |> ignore
            Graph.addEdge m a b 42 |> ignore))
    Assert.Equal(Some(edge 0 1 42), Graph.getEdge graph 0)
    Assert.Equal(None, Graph.getEdge graph 999)
    Assert.True(Graph.hasEdge graph 0 1)
    Assert.False(Graph.hasEdge graph 0 2)
    Assert.False(Graph.hasEdge (Graph.directed<string, int> None) 0 1)

[<Fact>]
let ``hasEdge is symmetric for undirected graphs`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 42 |> ignore))
    Assert.True(Graph.hasEdge graph 0 1)
    Assert.True(Graph.hasEdge graph 1 0)

// --- neighbors ---

[<Fact>]
let ``neighbors for directed graph`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m a c 2 |> ignore))
    Assert.Equal<int list>([ 1; 2 ], Graph.neighbors graph 0)
    Assert.Equal<int list>([], Graph.neighbors graph 1)

[<Fact>]
let ``neighbors for undirected graph`` () =
    let graph =
        Graph.undirected<int, unit> (Some(fun m ->
            Graph.addNode m 0 |> ignore
            Graph.addNode m 1 |> ignore
            Graph.addNode m 2 |> ignore
            Graph.addEdge m 0 1 () |> ignore
            Graph.addEdge m 1 2 () |> ignore))
    Assert.Equal<int list>([ 1 ], Graph.neighbors graph 0)
    Assert.Equal<int list>([ 0; 2 ], Graph.neighbors graph 1 |> List.sort)
    Assert.Equal<int list>([ 1 ], Graph.neighbors graph 2)

[<Fact>]
let ``undirected neighbors dedupe multi-edges and handle self-loops`` () =
    let multi =
        Graph.undirected<int, unit> (Some(fun m ->
            Graph.addNode m 0 |> ignore
            Graph.addNode m 1 |> ignore
            Graph.addEdge m 0 1 () |> ignore
            Graph.addEdge m 0 1 () |> ignore))
    Assert.Equal<int list>([ 1 ], Graph.neighbors multi 0)
    let loop =
        Graph.undirected<int, unit> (Some(fun m ->
            Graph.addNode m 0 |> ignore
            Graph.addEdge m 0 0 () |> ignore))
    Assert.Equal<int list>([ 0 ], Graph.neighbors loop 0)

[<Fact>]
let ``successors and predecessors`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m c b 2 |> ignore))
    Assert.Equal<int list>([ 1 ], Graph.successors graph 0)
    Assert.Equal<int list>([ 0; 2 ], Graph.predecessors graph 1 |> List.sort)

[<Fact>]
let ``successors and predecessors throw for undirected graphs`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 1 |> ignore))
    Assert.Throws<GraphError>(fun () -> Graph.successors graph 0 |> ignore) |> ignore
    Assert.Throws<GraphError>(fun () -> Graph.predecessors graph 0 |> ignore) |> ignore

[<Fact>]
let ``neighborsDirected`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m c b 2 |> ignore))
    Assert.Equal<int list>([ 0; 2 ], Graph.neighborsDirected graph 1 Incoming |> List.sort)
    Assert.Equal<int list>([], Graph.neighborsDirected graph 0 Incoming)
    Assert.Equal<int list>([ 1 ], Graph.neighborsDirected graph 0 Outgoing)

[<Fact>]
let ``neighborsDirected throws for undirected graphs`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 1 |> ignore))
    Assert.Throws<GraphError>(fun () -> Graph.neighborsDirected graph 0 Outgoing |> ignore) |> ignore

// --- element iteration ---

[<Fact>]
let ``nodes and edges iterate indices`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m b c 2 |> ignore))
    Assert.Equal<int list>([ 0; 1; 2 ], Graph.nodes graph)
    Assert.Equal<int list>([ 0; 1 ], Graph.edges graph)

// --- algorithms ---

[<Fact>]
let ``isAcyclic detects DAGs and cycles`` () =
    let dag =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            let d = Graph.addNode m "D"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m a c "" |> ignore
            Graph.addEdge m b d "" |> ignore
            Graph.addEdge m c d "" |> ignore))
    Assert.True(Graph.isAcyclic dag)
    let cyclic =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore
            Graph.addEdge m c a "" |> ignore))
    Assert.False(Graph.isAcyclic cyclic)

[<Fact>]
let ``isAcyclic over disconnected and undirected graphs`` () =
    let disconnected =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            let d = Graph.addNode m "D"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m c d "" |> ignore))
    Assert.True(Graph.isAcyclic disconnected)
    // reversed-storage undirected chain A-B, C-B is acyclic
    let chain =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m c b 1 |> ignore))
    Assert.True(Graph.isAcyclic chain)
    let triangle =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m b c 1 |> ignore
            Graph.addEdge m c a 1 |> ignore))
    Assert.False(Graph.isAcyclic triangle)

[<Fact>]
let ``isBipartite`` () =
    let bipartite =
        Graph.undirected<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            let d = Graph.addNode m "D"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore
            Graph.addEdge m c d "" |> ignore
            Graph.addEdge m d a "" |> ignore))
    Assert.True(Graph.isBipartite bipartite)
    let triangle =
        Graph.undirected<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore
            Graph.addEdge m c a "" |> ignore))
    Assert.False(Graph.isBipartite triangle)

[<Fact>]
let ``connectedComponents`` () =
    let graph =
        Graph.undirected<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            let d = Graph.addNode m "D"
            Graph.addNode m "E" |> ignore
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m c d "" |> ignore))
    let components = Graph.connectedComponents graph |> List.sortBy (fun c -> (List.length c, List.head c))
    Assert.Equal(3, List.length components)
    Assert.Equal<int list>([ 4 ], components.[0])
    Assert.Equal(2, List.length components.[1])
    Assert.Equal(2, List.length components.[2])

[<Fact>]
let ``connectedComponents fully connected`` () =
    let graph =
        Graph.undirected<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore
            Graph.addEdge m c a "" |> ignore))
    let components = Graph.connectedComponents graph
    Assert.Equal(1, List.length components)
    Assert.Equal<int list>([ 0; 1; 2 ], List.sort components.[0])

[<Fact>]
let ``stronglyConnectedComponents`` () =
    let graph =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            let d = Graph.addNode m "D"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore
            Graph.addEdge m c a "" |> ignore
            Graph.addEdge m b d "" |> ignore))
    let sccs = Graph.stronglyConnectedComponents graph |> List.sortBy List.length
    Assert.Equal(2, List.length sccs)
    Assert.Equal<int list>([ 3 ], sccs.[0])
    Assert.Equal<int list>([ 0; 1; 2 ], List.sort sccs.[1])

[<Fact>]
let ``stronglyConnectedComponents of a DAG are singletons`` () =
    let dag =
        Graph.directed<string, string> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b "" |> ignore
            Graph.addEdge m b c "" |> ignore))
    let sccs = Graph.stronglyConnectedComponents dag
    Assert.Equal(3, List.length sccs)
    Assert.All(sccs, fun scc -> Assert.Equal(1, List.length scc))

[<Fact>]
let ``stronglyConnectedComponents throws for undirected graphs`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b 1 |> ignore))
    Assert.Throws<GraphError>(fun () -> Graph.stronglyConnectedComponents graph |> ignore) |> ignore

// --- dijkstra ---

[<Fact>]
let ``dijkstra finds shortest path`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 5 |> ignore
            Graph.addEdge m a c 10 |> ignore
            Graph.addEdge m b c 2 |> ignore))
    let result = Graph.dijkstra graph 0 2 float
    Assert.Equal(Some { Path = [ 0; 1; 2 ]; Distance = 7.0; Costs = [ 5; 2 ] }, result)

[<Fact>]
let ``dijkstra returns None for unreachable target`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addNode m "C" |> ignore
            Graph.addEdge m a b 1 |> ignore))
    Assert.Equal(None, Graph.dijkstra graph 0 2 float)

[<Fact>]
let ``dijkstra same source and target`` () =
    let graph = Graph.directed<string, int> (Some(fun m -> Graph.addNode m "A" |> ignore))
    Assert.Equal(Some { Path = [ 0 ]; Distance = 0.0; Costs = [] }, Graph.dijkstra graph 0 0 float)

[<Fact>]
let ``dijkstra throws for negative weights`` () =
    let graph =
        Graph.directed<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            Graph.addEdge m a b -1 |> ignore))
    let ex = Assert.Throws<GraphError>(fun () -> Graph.dijkstra graph 0 1 float |> ignore)
    Assert.Equal("Dijkstra's algorithm requires non-negative edge weights", ex.Message)

[<Fact>]
let ``dijkstra throws for non-existent nodes`` () =
    let graph = Graph.directed<string, int> None
    let ex = Assert.Throws<GraphError>(fun () -> Graph.dijkstra graph 0 1 float |> ignore)
    Assert.Equal("Node 0 does not exist", ex.Message)

[<Fact>]
let ``dijkstra traverses undirected edges in reverse storage direction`` () =
    let graph =
        Graph.undirected<string, int> (Some(fun m ->
            let a = Graph.addNode m "A"
            let b = Graph.addNode m "B"
            let c = Graph.addNode m "C"
            Graph.addEdge m a b 1 |> ignore
            Graph.addEdge m c b 1 |> ignore))
    Assert.Equal(Some { Path = [ 0; 1; 2 ]; Distance = 2.0; Costs = [ 1; 1 ] }, Graph.dijkstra graph 0 2 float)
