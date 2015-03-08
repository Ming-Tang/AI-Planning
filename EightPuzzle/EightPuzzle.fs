module EightPuzzle

open System
open System.Collections.Generic

/// A tile in eight puzzle, or Blank for no tile
type Tile =
| Blank = 0
| T1 = 1
| T2 = 2
| T3 = 3
| T4 = 4
| T5 = 5
| T6 = 6
| T7 = 7
| T8 = 8

/// An eight puzzle board
type Board = { T11 : Tile
               T12 : Tile
               T13 : Tile

               T21 : Tile
               T22 : Tile
               T23 : Tile

               T31 : Tile
               T32 : Tile
               T33 : Tile }

/// Locates a square on the board. Tij means row i col j
type Index =
| T11 = 0 | T12 = 1 | T13 = 2
| T21 = 3 | T22 = 4 | T23 = 5
| T31 = 6 | T32 = 7 | T33 = 8

/// A move. Tij means move tile at (i,j) to the nearest blank spot
type Move = Index

(* -------------------------------------------------------------------------------- *)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tile =
  /// Create a Tile from a character. 0 or space for blank.
  let fromChar c =
    match c with
    | '0' | ' ' -> Tile.Blank
    | '1' -> Tile.T1
    | '2' -> Tile.T2
    | '3' -> Tile.T3
    | '4' -> Tile.T4
    | '5' -> Tile.T5
    | '6' -> Tile.T6
    | '7' -> Tile.T7
    | '8' -> Tile.T8
    | _ -> failwithf "Invalid tile: %c" c

  let toChar t =
    match t with
    | Tile.Blank -> ' '
    | Tile.T1 -> '1'
    | Tile.T2 -> '2'
    | Tile.T3 -> '3'
    | Tile.T4 -> '4'
    | Tile.T5 -> '5'
    | Tile.T6 -> '6'
    | Tile.T7 -> '7'
    | Tile.T8 -> '8'
    | _ -> failwithf "Invalid tile: %A" t

  let all = [| Tile.Blank; Tile.T1; Tile.T2; Tile.T3; Tile.T4; Tile.T5; Tile.T6; Tile.T7; Tile.T8 |]

  let inline forAll f =
    [| f Tile.Blank
       f Tile.T1
       f Tile.T2
       f Tile.T3
       f Tile.T4
       f Tile.T5
       f Tile.T6
       f Tile.T7
       f Tile.T8 |]

let inline (|BoardTiles|) ({ T11 = a; T12 = b; T13 = c
                             T21 = d; T22 = e; T23 = f
                             T31 = g; T32 = h; T33 = i } : Board) =
  (a, b, c, d, e, f, g, h, i)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =
  /// Create a index based on integer coordinates.
  let inline fromPos (i, j) =
    match i, j with
    | 1, 1 -> Index.T11 | 1, 2 -> Index.T12 | 1, 3 -> Index.T13
    | 2, 1 -> Index.T21 | 2, 2 -> Index.T22 | 2, 3 -> Index.T23
    | 3, 1 -> Index.T31 | 3, 2 -> Index.T32 | 3, 3 -> Index.T33
    | _ -> failwithf "Invalid pos: %A" (i, j)

  /// Get the position corresponding to the index
  let inline pos t =
    match t with
    | Index.T11 -> 1, 1 | Index.T12 -> 1, 2 | Index.T13 -> 1, 3
    | Index.T21 -> 2, 1 | Index.T22 -> 2, 2 | Index.T23 -> 2, 3
    | Index.T31 -> 3, 1 | Index.T32 -> 3, 2 | Index.T33 -> 3, 3
    | _ -> failwithf "Invalid index: %A" t

  /// Get indices adjacent to the given index
  let inline adjacents t =
    match t with
    | Index.T11 -> [ Index.T21; Index.T12 ]
    | Index.T12 -> [ Index.T22; Index.T11; Index.T13 ]
    | Index.T13 -> [ Index.T23; Index.T12 ]
    | Index.T21 -> [ Index.T11; Index.T31; Index.T22 ]
    | Index.T22 -> [ Index.T12; Index.T32; Index.T21; Index.T23 ]
    | Index.T23 -> [ Index.T13; Index.T33; Index.T22 ]
    | Index.T31 -> [ Index.T21; Index.T32 ]
    | Index.T32 -> [ Index.T22; Index.T31; Index.T33 ]
    | Index.T33 -> [ Index.T23; Index.T32 ]
    | _ -> failwithf "Invalid index: %A" t

  let all = [| Index.T11; Index.T12; Index.T13; Index.T21; Index.T22
               Index.T23; Index.T31; Index.T32; Index.T33 |]

  let inline forAll f =
    [| f Index.T11
       f Index.T12
       f Index.T13
       f Index.T21
       f Index.T22
       f Index.T23
       f Index.T31
       f Index.T32
       f Index.T33 |]

  let inline sumAll f =
     f Index.T11
     + f Index.T12
     + f Index.T13
     + f Index.T21
     + f Index.T22
     + f Index.T23
     + f Index.T31
     + f Index.T32
     + f Index.T33

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Board =
  let create(a, b, c, d, e, f, g, h, i) =
    let s = [| a; b; c; d; e; f; g; h; i |]

    let bCount = s |> Seq.filter ((=) Tile.Blank) |> Seq.length
    if bCount = 0 then failwith "Board must have one blank tile"
    let dCount = Set.ofSeq s |> Set.count
    if dCount <> 9 then failwith "All tiles must be distinct"

    { T11 = a; T12 = b; T13 = c
      T21 = d; T22 = e; T23 = f
      T31 = g; T32 = h; T33 = i }

  let makeBoard s =
    let s = Seq.cache s
    match Seq.toList s with
    | [ a; b; c; d; e; f; g; h; i ] ->
      create(a, b, c, d, e, f, g, h, i)
    | _ -> failwith "Board representation must be a sequence of nine Tiles."

  let fromString s =
    (s : string).Replace("\n", "")
    |> Seq.map Tile.fromChar
    |> makeBoard

  let toString b =
    let ({ T11 = a; T12 = b; T13 = c
           T21 = d; T22 = e; T23 = f
           T31 = g; T32 = h; T33 = i } : Board) = b
    new string
      [| yield! List.map Tile.toChar [ a; b; c ]
         yield '\n'
         yield! List.map Tile.toChar [ d; e; f ]
         yield '\n'
         yield! List.map Tile.toChar [ g; h; i ] |]

  /// Get the tile from the baord, at the position corresponding to the index
  let inline access i b =
    match i with
    | Index.T11 -> b.T11 | Index.T12 -> b.T12 | Index.T13 -> b.T13
    | Index.T21 -> b.T21 | Index.T22 -> b.T22 | Index.T23 -> b.T23
    | Index.T31 -> b.T31 | Index.T32 -> b.T32 | Index.T33 -> b.T33
    | _ -> failwithf "Invalid index: %A" i

  /// Create a new board with the tile at the specified index replaced
  let set i t b : Board =
    match i with
    | Index.T11 -> { b with T11 = t }
    | Index.T12 -> { b with T12 = t }
    | Index.T13 -> { b with T13 = t }
    | Index.T21 -> { b with T21 = t }
    | Index.T22 -> { b with T22 = t }
    | Index.T23 -> { b with T23 = t }
    | Index.T31 -> { b with T31 = t }
    | Index.T32 -> { b with T32 = t }
    | Index.T33 -> { b with T33 = t }
    | _ -> failwithf "Invalid index: %A" i

  let inline indexOf (t : Tile) (b : Board) =
    Index.all
    |> Seq.find (fun i -> t = access i b)

  /// Find the index of blank on the board
  let findBlank b =
    let candidates =
      Index.all
      |> Seq.filter (fun i -> Tile.Blank = access i b)
      |> List.ofSeq
    match candidates with
    | [] -> failwith "Incorrect board: No blanks found."
    | [i] -> i
    | _ -> failwith "Invalid board: Multiple blanks found."

  /// Get the possible moves that can be made on the board
  let possibleMoves b : Move list =
    findBlank b
    |> Index.adjacents

  /// Make a move at the specified position on the board
  let makeMove (m : Move) b =
    let t = access m b
    if t = Tile.Blank then failwith "Cannot move a blank tile."

    let candidates =
      Index.adjacents m
      |> List.filter (fun i -> Tile.Blank = access i b)

    match candidates with
    | [] -> failwith "Cannot find a blank tile to move to."
    | _::_::_ -> failwith "Invalid board: Too many blank tiles."
    | [i] ->
      b
      |> set i t
      |> set m Tile.Blank

module Searching =
  type Graph = IDictionary<Board, ISet<Board>>
  type Heuristic = Board -> int
  type Value = { mutable Visited : bool
                 mutable Value : int
                 mutable Cost : int
                 mutable Prev : Board option }
  type SearchResults = IDictionary<Board, Value>
  let inf = 10000000

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Heuristic =
    let inline sumAll f =
      f Tile.T1
      + f Tile.T2
      + f Tile.T3
      + f Tile.T4
      + f Tile.T5
      + f Tile.T6
      + f Tile.T7
      + f Tile.T8

    let misplacedTiles (goal : Board) : Heuristic = fun (board : Board) ->
      Index.sumAll (fun i -> if Board.access i goal = Board.access i board then 0 else 1)

    let editingDistance (goal : Board) : Heuristic = fun (board : Board) ->
      sumAll (fun t ->
        if t = Tile.Blank then 0
        else
          let x1, y1 = Board.indexOf t goal |> Index.pos
          let x2, y2 = Board.indexOf t board |> Index.pos
          abs (x2 - x1) + abs (y2 - y1))

    let maxColRow (goal : Board) : Heuristic = fun (board : Board) ->
      sumAll (fun t ->
        let x1, y1 = Board.indexOf t goal |> Index.pos
        let x2, y2 = Board.indexOf t board |> Index.pos
        max (abs (x2 - x1)) (abs (y2 - y1)))

    let colRow (goal : Board) : Heuristic = fun (board : Board) ->
      let inline sgn x = if x = 0 then 0 else 1
      sumAll (fun t ->
        let x1, y1 = Board.indexOf t goal |> Index.pos
        let x2, y2 = Board.indexOf t board |> Index.pos
        sgn (y2 - y1) + sgn(x2 - x1))

  let generateSearchGraph (init : Board) : Graph =
    let graph : Graph = Dictionary<Board, ISet<Board>>() :> _
    let fringe = Queue<Board>()
    fringe.Enqueue(init)
    while fringe.Count > 0 do
      let current = fringe.Dequeue()
      if not(graph.ContainsKey(current)) then
        let moves = Board.possibleMoves current
        let nexts = Seq.map (fun m -> Board.makeMove m current) moves
        graph.Add(current, HashSet(nexts) :> ISet<_>)
        for next in nexts do
          if not(graph.ContainsKey(next)) then
            fringe.Enqueue(next)
    graph

  let aStar (graph : Graph) (start : Board) (goal : Board) (complete : bool)
            (makeH : Board -> Heuristic) =
    let h = makeH goal
    let values = Dictionary()
    let mutable pq = Set.empty
    let mutable found = false
    let mutable iterations = 0

    pq <- Set.add (0, start) pq
    for KeyValue(v, _) in graph do
      values.Add(v, { Value.Visited = false; Cost = inf; Value = inf; Prev = None })

    values.[start].Cost <- 0
    values.[start].Value <- h start

    while not(Set.isEmpty pq) && not found do
      iterations <- iterations + 1
      // get a node with min value
      let (topValue, top) as topItem = Set.minElement pq
      pq <- Set.remove topItem pq

      if top = goal && not complete then
        found <- true
      else
        if not values.[top].Visited then
          values.[top].Visited <- true
          // propagate value to adjacents
          for adj in graph.[top] do
            if not values.[adj].Visited then
              let topCost = values.[top].Cost
              let origV = values.[adj].Value
              let calcCost = topCost + 1
              let origCost = values.[adj].Cost

              if calcCost < origCost then
                // found better value
                let calcV = calcCost + h adj // cost of any move is one

                values.[adj].Cost <- calcCost
                values.[adj].Value <- calcV
                values.[adj].Prev <- Some top
                pq <- Set.add (calcV, adj) pq
              else
                pq <- Set.add (origV, adj) pq

    values :> SearchResults, iterations

  let followPath (results : SearchResults) endNode =
    let mutable cur = results.[endNode]
    let mutable path = [ endNode ]
    while cur.Prev.IsSome do
      let node = cur.Prev.Value
      cur <- results.[node]
      path <- node :: path
    path

open Searching
[<EntryPoint>]
let main (argv : string array) =
  if argv.Length < 2 then
    printfn "Usage: EightPuzzle fromBoard toBoard"
    1
  else
    try
      let start = Board.fromString argv.[0]
      let goal = Board.fromString argv.[1]

      // garbage collection is forced before measuring memo
      let m = System.GC.GetTotalMemory(true)
      let sw = System.Diagnostics.Stopwatch.StartNew()
      let graph = generateSearchGraph start
      sw.Stop()
      let m' = System.GC.GetTotalMemory(true)

      printfn "Graph generated. time=%-16A,  vertices=%6d,  edges=%7d,  memory=%.2fMB"
        sw.Elapsed graph.Count (graph |> Seq.sumBy (fun kv -> kv.Value.Count))
        (1e-6 * float(m' - m))
      printfn ""

      let hs = [ "dijkstra", (fun _ _ -> 0)
                 "misplaced tiles", Heuristic.misplacedTiles
                 "misplaced col + row", Heuristic.colRow
                 "max col row", Heuristic.maxColRow
                 "editing distance", Heuristic.editingDistance ]

      let resultSet =
        hs
        |> List.map (fun (name, h) ->
          let sw = System.Diagnostics.Stopwatch.StartNew()
          let results, iterations = aStar graph start goal false h
          sw.Stop()
          let path = followPath results goal
          name, iterations, path, sw.Elapsed)

      for name, iterations, path, time in resultSet do
        printfn "%-20s: vertices visited=%6d,  path len=%3d,  time=%-16A,  hashcode path=%-10A"
          name iterations (path.Length - 1) time (hash path)

      printfn ""

      let (_, _, path, _) = resultSet.Head

      printf "Path:"
      for v in path do
        printf " %s " <| (Board.toString v).Replace("\n", "").Replace(" ", "0")
      printfn ""

      0
    with e ->
      printfn "Error: %s" e.Message
      1

