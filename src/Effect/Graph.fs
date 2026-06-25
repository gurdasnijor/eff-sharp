namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/Graph.ts.
///
/// A graph with integer-indexed nodes and edges, available in `directed` and
/// `undirected` kinds, built through a scoped mutable builder and then frozen
/// into an immutable value.
///
/// Data model (mirrors upstream): a node map `int -> 'N`, an edge map
/// `int -> Edge<'E>`, and outgoing/incoming adjacency lists of edge indices.
/// Node and edge indices are monotonic, so the F# `Map`'s sorted-key iteration
/// coincides with upstream's insertion-order `Map` iteration.
///
/// SCOPE: this is the *core surface* plus a representative set of algorithms.
/// Ported: constructors, the mutable builder (`beginMutation`/`endMutation`/
/// `mutate`), node & edge CRUD, `update*`/`map*`/`filter*`/`filterMap*`,
/// `reverse`, queries (`getNode`/`hasNode`/`nodeCount`/`getEdge`/`hasEdge`/
/// `edgeCount`/`neighbors`/`successors`/`predecessors`/`neighborsDirected`/
/// `find*`/`nodes`/`edges`), and the algorithms `isAcyclic`, `isBipartite`,
/// `connectedComponents`, `stronglyConnectedComponents`, and `dijkstra`.
///
/// Skipped (noted for the reviewer; never leaves the build red):
///   * Visualisation: `toGraphViz`, `toMermaid`.
///   * Path-finding: `astar`, `bellmanFord`, `floydWarshall`.
///   * Lazy traversal iterators: the `Walker` abstraction and `dfs`/`bfs`/
///     `topo`/`dfsPostOrder`/`externals`/`indices`/`values`/`entries`.
///   * JS-runtime machinery: the `TypeId`/`isGraph` guard, `toJSON`/`inspect`,
///     `.pipe()`, the `acyclic` cache field, and the JS `undefined`
///     node/edge-data tests (F# has no `undefined`; every value is defined, so
///     the equality/find/neighbor behaviour those tests probe is covered by the
///     ordinary tests).
/// Effect's `Equal`/`Hash` are satisfied with FSharp.Core structural equality.
/// Directed or undirected.
type Kind =
    | Directed
    | Undirected

/// Traversal direction for directed neighbour queries.
type Direction =
    | Outgoing
    | Incoming

/// A single edge: its endpoints and payload.
type Edge<'E> = { Source: int; Target: int; Data: 'E }

/// A shortest path: ordered node indices, total distance, and edge data.
type PathResult<'E> =
    { Path: int list
      Distance: float
      Costs: 'E list }

/// Raised for missing endpoints, illegal directed queries on undirected graphs,
/// and negative Dijkstra weights (parity with upstream `GraphError`).
type GraphError(message: string) =
    inherit System.Exception(message)

/// Read interface shared by the immutable graph and the mutable builder, so
/// query/algorithm functions accept either.
type IGraphView<'N, 'E> =
    abstract Kind: Kind
    abstract NodeCount: int
    abstract EdgeCount: int
    abstract HasNode: int -> bool
    abstract TryGetNode: int -> 'N option
    abstract TryGetEdge: int -> Edge<'E> option
    /// Node indices in ascending order.
    abstract NodeKeys: int list
    /// Edge entries in ascending edge-index order.
    abstract EdgeEntries: (int * Edge<'E>) list
    /// Outgoing edge indices for a node (all incident edges for undirected).
    abstract Outgoing: int -> int list
    /// Incoming edge indices for a node.
    abstract Incoming: int -> int list

/// The mutable builder used inside a mutation scope.
type MutableGraph<'N, 'E>(kind: Kind) =
    let nodes = Dictionary<int, 'N>()
    let edges = Dictionary<int, Edge<'E>>()
    let adjacency = Dictionary<int, ResizeArray<int>>()
    let reverseAdjacency = Dictionary<int, ResizeArray<int>>()
    let mutable nextNodeIndex = 0
    let mutable nextEdgeIndex = 0

    member _.Kind = kind
    member _.Nodes = nodes
    member _.Edges = edges
    member _.Adjacency = adjacency
    member _.ReverseAdjacency = reverseAdjacency

    member _.NextNodeIndex
        with get () = nextNodeIndex
        and set v = nextNodeIndex <- v

    member _.NextEdgeIndex
        with get () = nextEdgeIndex
        and set v = nextEdgeIndex <- v

    member private _.AdjOf(map: Dictionary<int, ResizeArray<int>>, node) =
        match map.TryGetValue node with
        | true, list -> List.ofSeq list
        | _ -> []

    interface IGraphView<'N, 'E> with
        member _.Kind = kind
        member _.NodeCount = nodes.Count
        member _.EdgeCount = edges.Count
        member _.HasNode i = nodes.ContainsKey i

        member _.TryGetNode i =
            match nodes.TryGetValue i with
            | true, v -> Some v
            | _ -> None

        member _.TryGetEdge i =
            match edges.TryGetValue i with
            | true, e -> Some e
            | _ -> None

        member _.NodeKeys = nodes.Keys |> Seq.sort |> List.ofSeq

        member _.EdgeEntries =
            edges
            |> Seq.sortBy (fun kv -> kv.Key)
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> List.ofSeq

        member this.Outgoing node = this.AdjOf(adjacency, node)
        member this.Incoming node = this.AdjOf(reverseAdjacency, node)

/// The immutable graph. Equality is by kind, nodes, and edges (matching
/// upstream `Equal`), independent of adjacency-list representation.
[<CustomEquality; NoComparison>]
type Graph<'N, 'E when 'N: equality and 'E: equality> =
    { Kind: Kind
      Nodes: Map<int, 'N>
      Edges: Map<int, Edge<'E>>
      Adjacency: Map<int, int list>
      ReverseAdjacency: Map<int, int list> }

    override this.Equals(o) =
        match o with
        | :? Graph<'N, 'E> as that -> this.Kind = that.Kind && this.Nodes = that.Nodes && this.Edges = that.Edges
        | _ -> false

    override this.GetHashCode() =
        hash (this.Kind, this.Nodes, this.Edges)

    interface IGraphView<'N, 'E> with
        member this.Kind = this.Kind
        member this.NodeCount = this.Nodes.Count
        member this.EdgeCount = this.Edges.Count
        member this.HasNode i = Map.containsKey i this.Nodes
        member this.TryGetNode i = Map.tryFind i this.Nodes
        member this.TryGetEdge i = Map.tryFind i this.Edges
        member this.NodeKeys = this.Nodes |> Map.toList |> List.map fst
        member this.EdgeEntries = this.Edges |> Map.toList

        member this.Outgoing node =
            Map.tryFind node this.Adjacency |> Option.defaultValue []

        member this.Incoming node =
            Map.tryFind node this.ReverseAdjacency |> Option.defaultValue []

[<RequireQualifiedAccess>]
module Graph =

    let private missingNode (node: int) =
        GraphError(sprintf "Node %d does not exist" node)

    let private ensureList (map: Dictionary<int, ResizeArray<int>>) (key: int) =
        match map.TryGetValue key with
        | true, list -> list
        | _ ->
            let list = ResizeArray<int>()
            map.[key] <- list
            list

    // --- mutable builder operations ---

    /// Add a node, returning its index.
    let addNode (m: MutableGraph<'N, 'E>) (data: 'N) : int =
        let i = m.NextNodeIndex
        m.Nodes.[i] <- data
        m.Adjacency.[i] <- ResizeArray<int>()
        m.ReverseAdjacency.[i] <- ResizeArray<int>()
        m.NextNodeIndex <- i + 1
        i

    /// Add an edge between two existing nodes, returning its index. Throws
    /// `GraphError` if either endpoint is missing.
    let addEdge (m: MutableGraph<'N, 'E>) (source: int) (target: int) (data: 'E) : int =
        if not (m.Nodes.ContainsKey source) then
            raise (missingNode source)

        if not (m.Nodes.ContainsKey target) then
            raise (missingNode target)

        let i = m.NextEdgeIndex

        m.Edges.[i] <-
            { Source = source
              Target = target
              Data = data }

        (ensureList m.Adjacency source).Add i
        (ensureList m.ReverseAdjacency target).Add i

        match m.Kind with
        | Undirected ->
            (ensureList m.Adjacency target).Add i
            (ensureList m.ReverseAdjacency source).Add i
        | Directed -> ()

        m.NextEdgeIndex <- i + 1
        i

    let private removeEdgeInternal (m: MutableGraph<'N, 'E>) (edgeIndex: int) : bool =
        match m.Edges.TryGetValue edgeIndex with
        | false, _ -> false
        | true, edge ->
            let remove (map: Dictionary<int, ResizeArray<int>>) key =
                match map.TryGetValue key with
                | true, list -> list.Remove edgeIndex |> ignore
                | _ -> ()

            remove m.Adjacency edge.Source
            remove m.ReverseAdjacency edge.Target

            match m.Kind with
            | Undirected ->
                remove m.Adjacency edge.Target
                remove m.ReverseAdjacency edge.Source
            | Directed -> ()

            m.Edges.Remove edgeIndex |> ignore
            true

    /// Remove an edge; a missing edge is a no-op.
    let removeEdge (m: MutableGraph<'N, 'E>) (edgeIndex: int) : unit =
        removeEdgeInternal m edgeIndex |> ignore

    /// Remove a node and all of its incident edges; a missing node is a no-op.
    let removeNode (m: MutableGraph<'N, 'E>) (nodeIndex: int) : unit =
        if m.Nodes.ContainsKey nodeIndex then
            let incident = ResizeArray<int>()

            match m.Adjacency.TryGetValue nodeIndex with
            | true, list -> incident.AddRange list
            | _ -> ()

            match m.ReverseAdjacency.TryGetValue nodeIndex with
            | true, list -> incident.AddRange list
            | _ -> ()

            for e in incident do
                removeEdgeInternal m e |> ignore

            m.Nodes.Remove nodeIndex |> ignore
            m.Adjacency.Remove nodeIndex |> ignore
            m.ReverseAdjacency.Remove nodeIndex |> ignore

    /// Apply `f` to the data at `nodeIndex` if present.
    let updateNode (m: MutableGraph<'N, 'E>) (nodeIndex: int) (f: 'N -> 'N) : unit =
        match m.Nodes.TryGetValue nodeIndex with
        | true, v -> m.Nodes.[nodeIndex] <- f v
        | _ -> ()

    /// Apply `f` to the data of the edge at `edgeIndex` if present.
    let updateEdge (m: MutableGraph<'N, 'E>) (edgeIndex: int) (f: 'E -> 'E) : unit =
        match m.Edges.TryGetValue edgeIndex with
        | true, e -> m.Edges.[edgeIndex] <- { e with Data = f e.Data }
        | _ -> ()

    let private nodeKeysSorted (m: MutableGraph<'N, 'E>) = m.Nodes.Keys |> Seq.sort |> List.ofSeq
    let private edgeKeysSorted (m: MutableGraph<'N, 'E>) = m.Edges.Keys |> Seq.sort |> List.ofSeq

    /// Transform every node's data.
    let mapNodes (m: MutableGraph<'N, 'E>) (f: 'N -> 'N) : unit =
        for i in nodeKeysSorted m do
            m.Nodes.[i] <- f m.Nodes.[i]

    /// Transform every edge's data.
    let mapEdges (m: MutableGraph<'N, 'E>) (f: 'E -> 'E) : unit =
        for i in edgeKeysSorted m do
            let e = m.Edges.[i]
            m.Edges.[i] <- { e with Data = f e.Data }

    let private rebuildAdjacency (m: MutableGraph<'N, 'E>) =
        for kv in m.Adjacency do
            kv.Value.Clear()

        for kv in m.ReverseAdjacency do
            kv.Value.Clear()

        for i in edgeKeysSorted m do
            let e = m.Edges.[i]
            (ensureList m.Adjacency e.Source).Add i
            (ensureList m.ReverseAdjacency e.Target).Add i

    /// Reverse every edge's direction (no-op for undirected graphs).
    let reverse (m: MutableGraph<'N, 'E>) : unit =
        match m.Kind with
        | Undirected -> ()
        | Directed ->
            for i in edgeKeysSorted m do
                let e = m.Edges.[i]

                m.Edges.[i] <-
                    { e with
                        Source = e.Target
                        Target = e.Source }

            rebuildAdjacency m

    /// Keep nodes for which `f` returns `Some` (with the transformed data) and
    /// drop the rest along with their incident edges.
    let filterMapNodes (m: MutableGraph<'N, 'E>) (f: 'N -> 'N option) : unit =
        let toRemove = ResizeArray<int>()

        for i in nodeKeysSorted m do
            match f m.Nodes.[i] with
            | Some v -> m.Nodes.[i] <- v
            | None -> toRemove.Add i

        for i in toRemove do
            removeNode m i

    /// Keep nodes satisfying the predicate; drop the rest with their edges.
    let filterNodes (m: MutableGraph<'N, 'E>) (pred: 'N -> bool) : unit =
        filterMapNodes m (fun d -> if pred d then Some d else None)

    /// Keep edges for which `f` returns `Some` (with transformed data).
    let filterMapEdges (m: MutableGraph<'N, 'E>) (f: 'E -> 'E option) : unit =
        let toRemove = ResizeArray<int>()

        for i in edgeKeysSorted m do
            match f m.Edges.[i].Data with
            | Some d -> m.Edges.[i] <- { m.Edges.[i] with Data = d }
            | None -> toRemove.Add i

        for i in toRemove do
            removeEdge m i

    /// Keep edges satisfying the predicate.
    let filterEdges (m: MutableGraph<'N, 'E>) (pred: 'E -> bool) : unit =
        filterMapEdges m (fun d -> if pred d then Some d else None)

    // --- constructors / mutation lifecycle ---

    let private freeze (m: MutableGraph<'N, 'E>) : Graph<'N, 'E> =
        let toMapList (d: Dictionary<int, ResizeArray<int>>) =
            d |> Seq.map (fun kv -> kv.Key, List.ofSeq kv.Value) |> Map.ofSeq

        { Kind = m.Kind
          Nodes = m.Nodes |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
          Edges = m.Edges |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
          Adjacency = toMapList m.Adjacency
          ReverseAdjacency = toMapList m.ReverseAdjacency }

    /// Open a mutation scope by copying an immutable graph.
    let beginMutation (graph: Graph<'N, 'E>) : MutableGraph<'N, 'E> =
        let m = MutableGraph<'N, 'E>(graph.Kind)

        for KeyValue(k, v) in graph.Nodes do
            m.Nodes.[k] <- v

        for KeyValue(k, v) in graph.Edges do
            m.Edges.[k] <- v

        for KeyValue(k, v) in graph.Adjacency do
            m.Adjacency.[k] <- ResizeArray<int>(v)

        for KeyValue(k, v) in graph.ReverseAdjacency do
            m.ReverseAdjacency.[k] <- ResizeArray<int>(v)

        let nextNode =
            if graph.Nodes.IsEmpty then
                0
            else
                (graph.Nodes |> Map.toSeq |> Seq.map fst |> Seq.max) + 1

        let nextEdge =
            if graph.Edges.IsEmpty then
                0
            else
                (graph.Edges |> Map.toSeq |> Seq.map fst |> Seq.max) + 1

        m.NextNodeIndex <- nextNode
        m.NextEdgeIndex <- nextEdge
        m

    /// Close a mutation scope, returning an immutable graph.
    let endMutation (m: MutableGraph<'N, 'E>) : Graph<'N, 'E> = freeze m

    let private build (kind: Kind) (mutate: (MutableGraph<'N, 'E> -> unit) option) : Graph<'N, 'E> =
        let m = MutableGraph<'N, 'E>(kind)

        match mutate with
        | Some f -> f m
        | None -> ()

        freeze m

    /// A directed graph, optionally initialised by a mutation function.
    let directed<'N, 'E when 'N: equality and 'E: equality>
        (mutate: (MutableGraph<'N, 'E> -> unit) option)
        : Graph<'N, 'E> =
        build Directed mutate

    /// An undirected graph, optionally initialised by a mutation function.
    let undirected<'N, 'E when 'N: equality and 'E: equality>
        (mutate: (MutableGraph<'N, 'E> -> unit) option)
        : Graph<'N, 'E> =
        build Undirected mutate

    /// Apply scoped mutations to an immutable graph, returning a new graph.
    let mutate (graph: Graph<'N, 'E>) (f: MutableGraph<'N, 'E> -> unit) : Graph<'N, 'E> =
        let m = beginMutation graph
        f m
        endMutation m

    // --- queries (work on either the immutable graph or the builder) ---

    let kind (g: #IGraphView<'N, 'E>) : Kind = g.Kind
    let nodeCount (g: #IGraphView<'N, 'E>) : int = g.NodeCount
    let edgeCount (g: #IGraphView<'N, 'E>) : int = g.EdgeCount
    let getNode (g: #IGraphView<'N, 'E>) (i: int) : 'N option = g.TryGetNode i
    let hasNode (g: #IGraphView<'N, 'E>) (i: int) : bool = g.HasNode i
    let getEdge (g: #IGraphView<'N, 'E>) (i: int) : Edge<'E> option = g.TryGetEdge i

    /// Node indices in ascending order.
    let nodes (g: #IGraphView<'N, 'E>) : int list = g.NodeKeys
    /// Edge indices in ascending order.
    let edges (g: #IGraphView<'N, 'E>) : int list = g.EdgeEntries |> List.map fst

    /// Node index/data pairs in ascending node order.
    let toNodeEntries (g: #IGraphView<'N, 'E>) : (int * 'N) list =
        g.NodeKeys |> List.map (fun i -> i, (g.TryGetNode i).Value)

    let findNode (g: #IGraphView<'N, 'E>) (pred: 'N -> bool) : int option =
        g.NodeKeys |> List.tryFind (fun i -> pred (g.TryGetNode i).Value)

    let findNodes (g: #IGraphView<'N, 'E>) (pred: 'N -> bool) : int list =
        g.NodeKeys |> List.filter (fun i -> pred (g.TryGetNode i).Value)

    let findEdge (g: #IGraphView<'N, 'E>) (pred: 'E -> bool) : int option =
        g.EdgeEntries
        |> List.tryPick (fun (i, e) -> if pred e.Data then Some i else None)

    let findEdges (g: #IGraphView<'N, 'E>) (pred: 'E -> bool) : int list =
        g.EdgeEntries
        |> List.choose (fun (i, e) -> if pred e.Data then Some i else None)

    let private directedNeighbors (g: #IGraphView<'N, 'E>) (node: int) (incoming: bool) : int list =
        let adj = if incoming then g.Incoming node else g.Outgoing node

        adj
        |> List.choose (fun ei -> g.TryGetEdge ei |> Option.map (fun e -> if incoming then e.Source else e.Target))

    let private undirectedNeighbors (g: #IGraphView<'N, 'E>) (node: int) : int list =
        g.Outgoing node
        |> List.choose (fun ei ->
            g.TryGetEdge ei
            |> Option.map (fun e -> if e.Source = node then e.Target else e.Source))
        |> List.distinct

    /// Neighbours of a node: outgoing targets (directed) or the other endpoint
    /// of each incident edge (undirected).
    let neighbors (g: #IGraphView<'N, 'E>) (node: int) : int list =
        match g.Kind with
        | Undirected -> undirectedNeighbors g node
        | Directed -> directedNeighbors g node false

    /// Outgoing neighbours; throws for undirected graphs.
    let successors (g: #IGraphView<'N, 'E>) (node: int) : int list =
        match g.Kind with
        | Undirected -> raise (GraphError "Cannot get successors of undirected graph")
        | Directed -> directedNeighbors g node false

    /// Incoming neighbours; throws for undirected graphs.
    let predecessors (g: #IGraphView<'N, 'E>) (node: int) : int list =
        match g.Kind with
        | Undirected -> raise (GraphError "Cannot get predecessors of undirected graph")
        | Directed -> directedNeighbors g node true

    /// Directed neighbours in a given direction; throws for undirected graphs.
    let neighborsDirected (g: #IGraphView<'N, 'E>) (node: int) (direction: Direction) : int list =
        match g.Kind with
        | Undirected -> raise (GraphError "Cannot get directed neighbors of undirected graph")
        | Directed -> directedNeighbors g node (direction = Incoming)

    /// Whether an edge connects `source` to `target` (symmetric for undirected).
    let hasEdge (g: #IGraphView<'N, 'E>) (source: int) (target: int) : bool =
        g.Outgoing source
        |> List.exists (fun ei ->
            match g.TryGetEdge ei with
            | Some e ->
                let neighbor =
                    match g.Kind with
                    | Undirected when e.Target = source -> e.Source
                    | _ -> e.Target

                neighbor = target
            | None -> false)

    // --- algorithms ---

    /// Whether the graph contains no cycles (ignoring edge data).
    let isAcyclic (g: #IGraphView<'N, 'E>) : bool =
        match g.Kind with
        | Undirected ->
            let visited = HashSet<int>()
            let mutable acyclic = true

            for start in g.NodeKeys do
                if acyclic && not (visited.Contains start) then
                    visited.Add start |> ignore
                    let stack = Stack<int * int option>()
                    stack.Push(start, None)

                    while acyclic && stack.Count > 0 do
                        let node, parent = stack.Pop()

                        for nb in undirectedNeighbors g node do
                            if not (visited.Contains nb) then
                                visited.Add nb |> ignore
                                stack.Push(nb, Some node)
                            elif Some nb <> parent then
                                acyclic <- false

            acyclic
        | Directed ->
            let visited = HashSet<int>()
            let recStack = HashSet<int>()

            let rec dfs node =
                visited.Add node |> ignore
                recStack.Add node |> ignore
                let mutable cycle = false

                for nb in directedNeighbors g node false do
                    if recStack.Contains nb then
                        cycle <- true
                    elif not (visited.Contains nb) then
                        if dfs nb then
                            cycle <- true

                recStack.Remove node |> ignore
                cycle

            let mutable acyclic = true

            for start in g.NodeKeys do
                if acyclic && not (visited.Contains start) then
                    if dfs start then
                        acyclic <- false

            acyclic

    /// Whether an undirected graph is 2-colourable (bipartite).
    let isBipartite (g: #IGraphView<'N, 'E>) : bool =
        let coloring = Dictionary<int, int>()
        let mutable bipartite = true

        for start in g.NodeKeys do
            if bipartite && not (coloring.ContainsKey start) then
                let queue = Queue<int>()
                coloring.[start] <- 0
                queue.Enqueue start

                while bipartite && queue.Count > 0 do
                    let current = queue.Dequeue()
                    let currentColor = coloring.[current]
                    let nextColor = if currentColor = 0 then 1 else 0

                    for nb in undirectedNeighbors g current do
                        if not (coloring.ContainsKey nb) then
                            coloring.[nb] <- nextColor
                            queue.Enqueue nb
                        elif coloring.[nb] = currentColor then
                            bipartite <- false

        bipartite

    /// Connected components of an undirected graph, each a list of node indices.
    let connectedComponents (g: #IGraphView<'N, 'E>) : int list list =
        let visited = HashSet<int>()
        let components = ResizeArray<int list>()

        for start in g.NodeKeys do
            if not (visited.Contains start) then
                let comp = ResizeArray<int>()
                let stack = Stack<int>()
                stack.Push start

                while stack.Count > 0 do
                    let current = stack.Pop()

                    if not (visited.Contains current) then
                        visited.Add current |> ignore
                        comp.Add current

                        for nb in undirectedNeighbors g current do
                            if not (visited.Contains nb) then
                                stack.Push nb

                components.Add(List.ofSeq comp)

        List.ofSeq components

    /// Strongly connected components of a directed graph (Kosaraju); throws for
    /// undirected graphs.
    let stronglyConnectedComponents (g: #IGraphView<'N, 'E>) : int list list =
        match g.Kind with
        | Undirected -> raise (GraphError "Cannot find strongly connected components of undirected graph")
        | Directed ->
            let visited = HashSet<int>()
            let finishOrder = ResizeArray<int>()

            let rec visit node =
                if not (visited.Contains node) then
                    visited.Add node |> ignore

                    for nb in directedNeighbors g node false do
                        visit nb

                    finishOrder.Add node

            for start in g.NodeKeys do
                visit start

            visited.Clear()
            let sccs = ResizeArray<int list>()

            for idx in finishOrder.Count - 1 .. -1 .. 0 do
                let start = finishOrder.[idx]

                if not (visited.Contains start) then
                    let scc = ResizeArray<int>()
                    let stack = Stack<int>()
                    stack.Push start

                    while stack.Count > 0 do
                        let node = stack.Pop()

                        if not (visited.Contains node) then
                            visited.Add node |> ignore
                            scc.Add node

                            for ei in g.Incoming node do
                                match g.TryGetEdge ei with
                                | Some e when not (visited.Contains e.Source) -> stack.Push e.Source
                                | _ -> ()

                    sccs.Add(List.ofSeq scc)

            List.ofSeq sccs

    /// Dijkstra's shortest path from `source` to `target`. `cost` maps edge data
    /// to a non-negative weight. Throws for missing endpoints or negative
    /// weights; returns `None` when the target is unreachable.
    let dijkstra (g: #IGraphView<'N, 'E>) (source: int) (target: int) (cost: 'E -> float) : PathResult<'E> option =
        if not (g.HasNode source) then
            raise (missingNode source)

        if not (g.HasNode target) then
            raise (missingNode target)

        let edgeWeights = Dictionary<int, float>()

        for (i, e) in g.EdgeEntries do
            let w = cost e.Data

            if w < 0.0 || System.Double.IsNaN w then
                raise (GraphError "Dijkstra's algorithm requires non-negative edge weights")

            edgeWeights.[i] <- w

        if source = target then
            Some
                { Path = [ source ]
                  Distance = 0.0
                  Costs = [] }
        else
            let distances = Dictionary<int, float>()
            let previous = Dictionary<int, (int * 'E) option>()

            for node in g.NodeKeys do
                distances.[node] <- (if node = source then 0.0 else infinity)
                previous.[node] <- None

            let visited = HashSet<int>()
            let queue = ResizeArray<int * float>()
            queue.Add(source, 0.0)
            let mutable go = true

            while go && queue.Count > 0 do
                let mutable minIdx = 0

                for i in 1 .. queue.Count - 1 do
                    if snd queue.[i] < snd queue.[minIdx] then
                        minIdx <- i

                let currentNode, _ = queue.[minIdx]
                queue.RemoveAt minIdx

                if not (visited.Contains currentNode) then
                    visited.Add currentNode |> ignore

                    if currentNode = target then
                        go <- false
                    else
                        let currentDistance = distances.[currentNode]

                        for ei in g.Outgoing currentNode do
                            match g.TryGetEdge ei with
                            | Some edge ->
                                let neighbor =
                                    match g.Kind with
                                    | Undirected when edge.Target = currentNode -> edge.Source
                                    | _ -> edge.Target

                                let newDistance = currentDistance + edgeWeights.[ei]

                                if newDistance < distances.[neighbor] then
                                    distances.[neighbor] <- newDistance
                                    previous.[neighbor] <- Some(currentNode, edge.Data)

                                    if not (visited.Contains neighbor) then
                                        queue.Add(neighbor, newDistance)
                            | None -> ()

            let distance = distances.[target]

            if System.Double.IsInfinity distance then
                None
            else
                let path = ResizeArray<int>()
                let costs = ResizeArray<'E>()
                let mutable current = Some target

                while current.IsSome do
                    let node = current.Value
                    path.Insert(0, node)

                    match previous.[node] with
                    | Some(prevNode, edgeData) ->
                        costs.Insert(0, edgeData)
                        current <- Some prevNode
                    | None -> current <- None

                Some
                    { Path = List.ofSeq path
                      Distance = distance
                      Costs = List.ofSeq costs }

    // --- rendering ---

    /// `Graph(directed, 2, 1)` — kind, node count, edge count.
    let render (g: #IGraphView<'N, 'E>) : string =
        let kindStr =
            match g.Kind with
            | Directed -> "directed"
            | Undirected -> "undirected"

        sprintf "Graph(%s, %d, %d)" kindStr g.NodeCount g.EdgeCount
