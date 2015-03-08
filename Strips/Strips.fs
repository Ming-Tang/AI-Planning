namespace StripsPlanner

open System
open System.Collections.Generic

[<AutoOpen>]
module Strips =
  type Symbol = string

  /// A predicate in the add/delete list
  [<StructuredFormatDisplay("{StripsForm}")>]
  type Pred = Pred of polarity : bool * predName : string * values : Symbol list with
    member p.StripsForm =
      match p with
      | Pred(p, name, values) ->
        let spaced values = String.concat " " values
        let s = sprintf "(%s %s)" name (spaced values)
        if p then s
        else sprintf "(not %s)" s

  /// A state is a set of predicates
  type State = (string * Symbol list) Set
  /// An add/delete list of preds
  type AddDelete = State * State

  /// An action of ground atoms only
  type Action =
    { Name : string
      Parameters : Symbol list
      Precondition : AddDelete
      Effect : AddDelete } with
    member a.StripsForm =
      let spaced values = String.concat " " values
      sprintf "(%s %s)" a.Name (spaced a.Parameters)

  /// Ignore negatives in a set of preds
  let ignoreNots preds = Set.filter (fun (Pred(p, _, _)) -> p) preds
  /// Ignore delete lists in preconds and postconds
  let ignoreDeleteList (act : Action) =
    { act with Precondition = (fst act.Precondition, Set.empty)
               Effect = (fst act.Effect, Set.empty) }

  /// Partition a set of preds into positive and negative preds
  let partition preds =
    Set.partition (fun (Pred(p, _, _)) -> p) preds

  /// Create a list of predicates, and merge add and remove list
  let preds (preds : #seq<_>) =
    let add, del = partition (Set.ofSeq preds)
    Set.ofSeq <| Set.map (fun (Pred(_, name, values)) -> name, values) add,
    Set.ofSeq <| Set.map (fun (Pred(_, name, values)) -> name, values) del

  /// Create a state, which is a list of positive predicates
  let state sq = sq |> preds |> fst

  /// Return true if and only if the state satisfied a list of pos/neg predicates
  let satisfies ((add, del) : AddDelete) (state : State) =
    Set.isEmpty <| Set.intersect state del
    && Set.isSubset add state

  /// Return true if and only if the action is applicable to the state
  let isApplicable (state : State) (act : Action) =
    state
    |> satisfies act.Precondition

  let applicableActions acts state =
    acts |> Set.filter (isApplicable state)

  let applyAction state action =
    if not (isApplicable state action) then
      failwith "Action is not applicable: %A" action.StripsForm
    let add, del = action.Precondition
    Set.union add (Set.difference state del)

  let applyActionToAddDelete state action =
    if not (isApplicable state action) then
      failwith "Action is not applicable: %A" action.StripsForm
    let add, del = action.Precondition
    Set.union add (Set.difference state del)

  let isRelevant (goal : AddDelete) (action : Action) =
    let goalAdd, goalDel = goal
    let effAdd, effDel = action.Effect

    let intersectionNonEmpty a b = not (Set.isEmpty (Set.intersect a b))
    let intersectionEmpty a b = Set.isEmpty (Set.intersect a b)

    (intersectionNonEmpty goalAdd effAdd || intersectionNonEmpty goalDel effDel)
    && intersectionEmpty goalAdd effDel && intersectionEmpty goalDel effAdd

  let unapplyAction (goal : AddDelete) (act : Action) : AddDelete =
    let t2 f (a, b) (c, d) = f a c, f b d
    (t2 Set.union (t2 Set.difference goal act.Effect) act.Precondition)

[<AutoOpen>]
module GraphPlan =
  type Layer = Layer of AddDelete * Action Set

  let getInitialLayer allActions (layerState : AddDelete) =
    for a : Action in allActions do
      if not (Set.isEmpty <| snd a.Precondition) then
        failwithf "Action %s contains negative preconditions!" a.StripsForm

    Layer(layerState, applicableActions allActions (fst layerState))

  let getNextLayer allActions (Layer((stateAdd, stateDel), layerActions)) =
    let f2 f (a, b) = f a, f b
    let newAdd, newDel =
      layerActions
      |> Seq.map (fun (a : Action) -> a.Effect)
      |> Array.ofSeq
      |> Array.unzip
      |> f2 Set.unionMany
    let newLayerState = Set.union newAdd stateAdd, Set.union newDel stateDel
    let newLayerActions = applicableActions allActions (fst newLayerState)
    Layer(newLayerState, newLayerActions)

  let satisfiesGoal (Layer((stateAdd, stateDel), _)) ((goalAdd, goalDel) : AddDelete) =
    Set.isSubset goalAdd stateAdd && Set.isSubset goalDel stateDel

[<AutoOpen>]
module FFHeuristic =
  /// A layer is a state and all applicable actions given that state
  type FFLayer = FFLayer of Action Set * (string * Symbol list) Set

  type CompureRPGResult = Success of FFLayer list | Failure of FFLayer list

  let isApplicableRelaxed (state : State) (act : Action) =
    Set.isSubset (fst act.Precondition) state

  let applyActionRelaxed (state : State) (act : Action) =
    Set.union state (fst act.Effect)

  /// Compute relaxed planning graph
  let computeRPG actions (initial : State) (goal : State) =
    let rec computeRPG' layers (state : State) =
      printfn "State: %A" state
      if Set.isSubset goal state then
        Success (List.rev layers)
      else
        let applicables = Set.filter (isApplicableRelaxed state) actions
        printfn "App: %A" applicables
        let newState =
          applicables
          |> Set.map (applyActionRelaxed state)
          |> Set.unionMany
          |> Set.union state
        if newState = state then
          Failure (List.rev layers)
        else
          computeRPG' (FFLayer(applicables, newState) :: layers) newState
    computeRPG' [] initial

  let extractRelaxedPlan (layers : FFLayer list) (goal : State) =
    let layers = Array.ofList layers
    let firstLevel p =
      let rec firstLevel' i =
        if i >= layers.Length then None
        else
          let (FFLayer(ai, si)) = layers.[i]
          if p i ai si then Some i
          else firstLevel' (i + 1)
      firstLevel' 0

    let firstLevelP p = firstLevel (fun i actions state -> Set.contains p state)

    let findFirst p s =
      match s 
            |> Seq.filter p
            |> Seq.truncate 1
            |> List.ofSeq with
      | [] -> None
      | x :: _ -> Some x

    // Level where the all goals can be achieved
    let m = 
      goal
      |> Seq.map (fun gp -> 
        match firstLevelP gp with
        | Some l -> l
        | None -> failwithf "Goal pred cannot be satisfied: %A" gp)
      |> Seq.max

    // Subgoals we need achieve for each layer
    let subgoals = Array.init (m + 1) (fun t ->
      Set.filter (fun gi -> firstLevelP gi = Some t) goal)

    // Actions selected for the relaxed plan
    let selected = Array.init (m + 1) (fun i -> Set.empty)

    // Extract relaxed plan
    for t = m downto 0 do
      // For each pred in this subgoal, select an earliest action that satisfies it
      let (FFLayer(actions, state)) = layers.[t]
      for gp in subgoals.[t] do
        // Find the first level that contains the actions that satisfies gp
        let selectedAction = ref None
        let actionLevel =
          firstLevel (fun i actionsi statei ->
            match findFirst (fun a -> Set.contains gp (fst a.Effect)) actionsi with
            | None -> false
            | Some action ->
              selectedAction := Some action
              true)

        if actionLevel = Some t then
          match !selectedAction with
          | None -> printfn "No selected action @ %d" t
          | Some a ->
            // Select this action
            selected.[t] <- Set.add a selected.[t]
            // For each precond, add subgoal to prev layer
            for p in fst a.Precondition do
              match firstLevelP p with
              | None -> failwithf "Precondition of %s cannot be achieved: %A" a.Name p
              | Some j ->
                subgoals.[j] <- Set.add p subgoals.[j]
        else
          printfn "Action level mismatch: %A <> Some %d" actionLevel t

    List.ofArray selected

  let extractRelaxedPlanSize layers goal =
    extractRelaxedPlan layers goal
    |> List.sumBy Set.count
