namespace StripsPlanner.Domains

open System
open System.Collections.Generic
open StripsPlanner

module DomainFE =
  // (define (domain random-domain-fe)
  //   (:requirements :strips)
  //   (:action op1
  //     :parameters (?x1 ?x2 ?x3)
  //     :precondition (and (R ?x3 ?x1) (S ?x2 ?x2))
  //     :effect (and (R ?x1 ?x3) (S ?x3 ?x1) (not (R ?x3 ?x1)) (not (S ?x2 ?x2))))
  //   (:action op2
  //     :parameters (?x1 ?x2 ?x3)
  //     :precondition (and (R ?x1 ?x2) (S ?x2 ?x1) (R ?x2 ?x3))
  //     :effect (and (R ?x2 ?x2) (S ?x1 ?x2) (not (S ?x2 ?x1)))))
  // 
  // (define (problem rndpb-fe-1)
  //   (:domain random-domain-fe)
  //   (:init
  //      (S B B)
  //      (R C C) (R C B) (R B A) (R B C))
  //   (:goal (and (R B B))))

  let [<Literal>] A = "A"
  let [<Literal>] B = "B"
  let [<Literal>] C = "C"
  let allSymbols = [ A; B; C; ]

  let R x1 x2 = Pred(true, "R", [ x1; x2 ])
  let notR x1 x2 = Pred(false, "R", [ x1; x2 ])

  let S x1 x2 = Pred(true, "S", [ x1; x2 ])
  let notS x1 x2 = Pred(false, "S", [ x1; x2 ])

  let allOps op =
    seq {
      for x1 in allSymbols do
        for x2 in allSymbols do
          for x3 in allSymbols do
            yield op x1 x2 x3
    } |> Set.ofSeq

  let op1 x1 x2 x3 =
    { Action.Name = "op1"
      Parameters = [ x1; x2; x3 ]
      Precondition = preds [ R x3 x1; S x2 x2 ]
      Effect = preds [ R x1 x3; S x3 x1; notR x3 x1; notS x2 x2 ] }

  let op2 x1 x2 x3 =
    { Action.Name = "op2"
      Parameters = [ x1; x2; x3 ]
      Precondition = preds [ R x1 x2; S x2 x1; R x2 x3 ]
      Effect = preds [ R x2 x2; S x1 x2; notS x2 x1 ] }

  let allOp1 = allOps op1
  let allOp2 = allOps op2

  let allActions = Set.union allOp1 allOp2

  let initial = state [ S B B; R C C; R C B; R B A; R B C ]
  let goal = state [ R B B ]

  let main() =
    printfn "Initial:"
    initial |> Seq.iter (printfn "  %A")

    let rpg = computeRPG allActions initial goal
    match rpg with
    | Failure rpg ->
      printfn "%A" rpg
      failwith "Unable to extract RPG."
    | Success rpg ->
      for FFLayer(actions, state) in rpg do
        printfn "------------------------"
        printfn "Actions:"
        actions
        |> Seq.iter (fun action -> printfn "  (%s %s)" action.Name
                                     (String.concat " " action.Parameters))
        printfn "State: "
        state |> Seq.iter (printfn "  %A")

      printfn "------------------------"
      printfn "Goal:"
      goal |> Seq.iter (printfn "  %A")

      printfn "------------------------"
      printfn "Relaxed Plan:"
      extractRelaxedPlan rpg goal
      |> printfn "%A"

module RandomDomain =
  // (define (domain random-domain)
  //   (:requirements :strips)
  //   (:action op1
  //     :parameters (?x1 ?x2 ?x3)
  //     :precondition (and (S ?x1 ?x2) (R ?x3 ?x1))
  //     :effect (and (S ?x2 ?x1) (S ?x1 ?x3) (not (R ?x3 ?x1))))
  //   (:action op2
  //     :parameters (?x1 ?x2 ?x3)
  //     :precondition (and (S ?x3 ?x1) (R ?x2 ?x2))
  //     :effect (and (S ?x1 ?x3) (not (S ?x3 ?x1)))))
  //
  // (define (problem random-pbl1)
  //   (:domain random-domain)
  //   (:init
  //     (S B B) (S C B) (S A C)
  //     (R B B) (R C B))
  //   (:goal (and (S A A))))

  let [<Literal>] A = "A"
  let [<Literal>] B = "B"
  let [<Literal>] C = "C"
  let allSymbols = [ A; B; C; ]

  let R x1 x2 = Pred(true, "R", [ x1; x2 ])
  let notR x1 x2 = Pred(false, "R", [ x1; x2 ])

  let S x1 x2 = Pred(true, "S", [ x1; x2 ])
  let notS x1 x2 = Pred(false, "S", [ x1; x2 ])

  let allActions op =
    seq {
      for x1 in allSymbols do
        for x2 in allSymbols do
          for x3 in allSymbols do
            yield op x1 x2 x3
    } |> Set.ofSeq

  let op1 x1 x2 x3 =
    { Action.Name = "op1"
      Parameters = [ x1; x2; x3 ]
      Precondition = preds [ S x1 x2; R x3 x1 ]
      Effect = preds [ S x2 x1; S x1 x3; notR x3 x1 ] }

  let op2 x1 x2 x3 =
    { Action.Name = "op2"
      Parameters = [ x1; x2; x3 ]
      Precondition = preds [ S x3 x1; R x2 x2 ]
      Effect = preds [ S x1 x3; notS x3 x1 ] }

  let allOp1 = allActions op1
  let allOp2 = allActions op2
  let allOps = Set.union allOp1 allOp2

  let initial = state [ S B B; S C B; S A C; R B B; R C B ]
  let goal = preds [ S A A ]

  let main() =
    applicableActions allOps initial |> Set.iter (printfn "%A")
    printfn "------------"
    allOps
    |> Set.filter (isRelevant goal)
    |> Set.iter (printfn "%A") // (fun a -> printfn "%s %A" a.Name a.Parameters)

