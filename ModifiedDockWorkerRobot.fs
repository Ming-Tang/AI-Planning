namespace StripsPlanner.Domains

open System
open System.Collections.Generic
open StripsPlanner

module ModifiedDockWorkerRobot =
  // ;; Specification in PDDL1 of the DWR domain
  // 
  // (define (domain dock-worker-robot-pos)
  //  (:requirements :strips :typing )
  //  (:types 
  //   location      ; there are several connected locations in the harbor 
  //   pile             ; is attached to a location 
  //                      ; it holds a pallet and a stack of containers 
  //   robot          ; holds at most 1 container, only 1 robot per location
  //   crane          ; belongs to a location to pickup containers
  //   container)
  // 
  //  (:constants
  //   pallet - container)
  // 
  //  (:predicates
  //    (adjacent ?l1  ?l2 - location)       ; location ?l1 is adjacent ot ?l2
  //    (attached ?p - pile ?l - location)  ; pile ?p attached to location ?l
  //    (belong ?k - crane ?l - location)   ; crane ?k belongs to location ?l
  // 
  //    (at ?r - robot ?l - location)       ; robot ?r is at location ?l
  //    (free ?l - location)             ; there is a robot at location ?l
  //    (loaded ?r - robot ?c - container ) ; robot ?r is loaded with container ?c
  //    (unloaded ?r - robot)                 ; robot ?r is empty
  // 
  //    (holding ?k - crane ?c - container) ; crane ?k is holding a container ?c
  //    (empty ?k - crane)                          ; crane ?k is empty
  // 
  //    (in ?c - container ?p - pile)        ; container ?c is within pile ?p
  //    (top ?c - container ?p - pile)      ;  container ?c is on top of pile ?p
  //    (on ?k1 - container ?k2 - container); container ?k1 is on container ?k2
  //    )
  // 
  // ;; there are 5 operators in this domain:
  // 
  // ;; moves a robot between two adjacent locations
  //  (:action move                                
  //      :parameters (?r - robot ?from ?to - location)
  //      :precondition (and (adjacent ?from ?to)
  //                       (at ?r ?from) (free ?to))
  //      :effect (and (at ?r ?to) (free ?from)
  //                     (not (free ?to)) (not (at ?r ?from)) ))
  // 
  // ;; loads an empty robot with a container held by a nearby crane
  //  (:action load                                
  //      :parameters (?k - crane ?l - location ?c - container ?r - robot)
  //      :precondition (and (at ?r ?l) (belong ?k ?l)
  //                       (holding ?k ?c) (unloaded ?r))
  //      :effect (and  (loaded ?r ?c) (not (unloaded ?r))
  //                   (empty ?k) (not (holding ?k ?c))))
  // 
  // 
  // ;; unloads a robot holding a container with a nearby crane
  //  (:action unload                                 
  //      :parameters (?k - crane ?l - location ?c - container ?r - robot)
  //      :precondition (and (belong ?k ?l) (at ?r ?l)
  //                       (loaded ?r ?c) (empty ?k))
  //      :effect (and (unloaded ?r) (holding ?k ?c)
  //                 (not (loaded ?r ?c))(not (empty ?k))))
  // 
  // ;; takes a container from a pile with a crane
  //  (:action take
  //      :parameters (?k - crane ?l - location ?c ?else - container ?p - pile)
  //      :precondition (and (belong ?k ?l)(attached ?p ?l)
  //                        (empty ?k) (in ?c ?p) 
  //                        (top ?c ?p) (on ?c ?else))
  //      :effect (and (holding ?k ?c) (top ?else ?p)
  //                 (not (in ?c ?p)) (not (top ?c ?p))
  //                 (not (on ?c ?else)) (not (empty ?k))))
  // 
  // ;; puts a container held by a crane on a nearby pile
  //  (:action put                                 
  //      :parameters (?k - crane ?l - location ?c ?else - container ?p - pile)
  //      :precondition (and (belong ?k ?l) (attached ?p ?l)
  //                       (holding ?k ?c) (top ?else ?p))
  //      :effect (and (in ?c ?p) (top ?c ?p) (on ?c ?else)
  //                  (not (top ?else ?p)) (not (holding ?k ?c))
  //                  (empty ?k))))
  // 
  // ;; a simple DWR problem with 1 robot and 2 locations
  // (define (problem dwrpb1)
  //   (:domain dock-worker-robot-pos)
  // 
  //   (:objects
  //    r1 - robot
  //    l1 l2 - location
  //    k1 k2 - crane
  //    p1 q1 p2 q2 - pile
  //    ca cb cc cd ce cf - container)
  // 
  //   (:init
  //    (adjacent l1 l2)
  //    (adjacent l2 l1)
  // 
  //    (attached p1 l1)
  //    (attached q1 l1)
  //    (attached p2 l2)
  //    (attached q2 l2)
  // 
  //    (belong k1 l1)
  //    (belong k2 l2)
  // 
  //    (in ca p1)
  //    (in cb p1)
  //    (in cc p1)
  // 
  //    (in cd q1)
  //    (in ce q1)
  //    (in cf q1)
  // 
  //    (on ca pallet)
  //    (on cb ca)
  //    (on cc cb)
  // 
  //    (on cd pallet)
  //    (on ce cd)
  //    (on cf ce)
  // 
  //    (top cc p1)
  //    (top cf q1)
  //    (top pallet p2)
  //    (top pallet q2)
  // 
  //    (at r1 l1)
  //    (unloaded r1)
  //    (free l2)
  // 
  //    (empty k1)
  //    (empty k2))
  // 
  // ;; the task is to move all containers to locations l2
  // ;; ca and cc in pile p2, the rest in q2
  //   (:goal
  //     (and (in ca p2)
  //           (in cb q2)
  //           (in cc p2)
  //           (in cd q2)
  //           (in ce q2)
  //           (in cf q2))))

  type Robot = R1
  type Location = L1 | L2
  type Crane = K1 | K2
  type Pile = P1 | Q1 | P2 | Q2
  type Container = CA | CB | CC | CD | CE | CF | Pallet

  let allRobots = [ R1 ]
  let allLocations = [ L1; L2 ]
  let allCranes = [ K1; K2 ]
  let allPiles = [ P1; Q1; P2; Q2 ]
  let allContainers = [ CA; CB; CC; CD; CE; CF; Pallet ]

  let sym x = (sprintf "%A" x).ToLower()
  let negate (Pred(_, n, a)) = Pred(false, n, a)

  let adjacent (l1 : Location) (l2 : Location) =
    Pred(true, "adjacent", [ sym l1; sym l2 ])
     
  let notAdjacent l1 l2 = adjacent l1 l2 |> negate

  let attached (p : Pile) (l : Location) =
    Pred(true, "attached", [ sym p; sym l ])

  let notAttached p l = attached p l |> negate

  let belong (k : Crane) (l : Location) =
    Pred(true, "belong", [ sym k; sym l ])

  let notBelong k l = belong k l |> negate

  let at (r : Robot) (l : Location) =
    Pred(true, "at", [ sym r; sym l ])

  let notAt r l = at r l |> negate

  let free (l : Location) =
    Pred(true, "free", [ sym l ])

  let notFree l = free l |> negate

  let loaded (r : Robot) (c : Container) =
    Pred(true, "loaded", [ sym r; sym c ])

  let notLoaded r c = loaded r c |> negate

  let unloaded (r : Robot) =
    Pred(true, "unloaded", [ sym r ])

  let notUnloaded r = unloaded r |> negate

  let holding (k : Crane) (c : Container) =
    Pred(true, "holding", [ sym k; sym c ])

  let notHolding k c = holding k c |> negate

  let empty (k : Crane) =
    Pred(true, "empty", [ sym k ])

  let notEmpty k = empty k |> negate

  let in' (c : Container) (p : Pile) =
    Pred(true, "in", [ sym c; sym p ])

  let notIn c p = in' c p |> negate

  let top (c : Container) (p : Pile) =
    Pred(true, "top", [ sym c; sym p ])

  let notTop c p = top c p |> negate 

  let on (k1 : Container) (k2 : Container) =
    Pred(true, "on", [ sym k1; sym k2 ])

  let notOn k1 k2 = on k1 k2 |> negate

  let move (r : Robot) (from : Location) (to' : Location) =
    { Action.Name = "move"
      Parameters = [ sym from; sym to' ]
      Precondition = preds [ adjacent from to'; at r from; free to' ]
      Effect = preds [ at r to'; free from; notFree to'; notAt r from ] }

  let load (k : Crane) (l : Location) (c : Container) (r : Robot) =
    { Action.Name = "load"
      Parameters = [ sym k; sym l; sym c; sym r ]
      Precondition = preds [ at r l; belong k l; holding k c; unloaded r ]
      Effect = preds [ loaded r c; notUnloaded r; empty k; notHolding k c ] }

  let unload (k : Crane) (l : Location) (c : Container) (r : Robot) =
    { Action.Name = "unload"
      Parameters = [ sym k; sym l; sym c; sym r ]
      Precondition = preds [ belong k l; at r l; loaded r c; empty k ]
      Effect = preds [ unloaded r; holding k c; notLoaded r c; notEmpty k ] }

  let take (k : Crane) (l : Location) (c : Container) (else' : Container) (p : Pile) =
    { Action.Name = "take"
      Parameters = [ sym l; sym l; sym c; sym else'; sym p ]
      Precondition = preds [ belong k l; attached p l; empty k; in' c p; top c p; on c else' ]
      Effect = preds [ holding k c; top else' p; notIn c p; notTop c p
                       notOn c else'; notEmpty k ] }

  let put (k : Crane) (l : Location) (c : Container) (else' : Container) (p : Pile) =
    { Action.Name = "put"
      Parameters = [ sym l; sym l; sym c; sym else'; sym p ]
      Precondition = preds [ belong k l; attached p l; holding k c; top else' p ]
      Effect = preds [ in' c p; top c p; on c else'; notTop else' p
                       notHolding k c; empty k ] }

  let gen3 f xs ys zs =
    Set.ofSeq <| seq {
      for x in xs do
        for y in ys do
          for z in zs do
            yield f x y z
    }

  let gen4 f ws xs ys zs =
    Set.ofSeq <| seq {
      for w in ws do
        for x in xs do
          for y in ys do
            for z in zs do
              yield f w x y z
    }

  let gen5 f vs ws xs ys zs =
    Set.ofSeq <| seq {
      for v in vs do
        for w in ws do
          for x in xs do
            for y in ys do
              for z in zs do
                yield f v w x y z
    }

  let allMove = gen3 move allRobots allLocations allLocations
  let allLoad = gen4 load allCranes allLocations allContainers allRobots
  let allUnload = gen4 unload allCranes allLocations allContainers allRobots
  let allTake  = gen5 take allCranes allLocations allContainers allContainers allPiles
  let allPut = gen5 put allCranes allLocations allContainers allContainers allPiles

  let allActions = Set.unionMany [ allMove; allLoad; allUnload; allTake; allPut ]

  let init = 
    state [
      adjacent L1 L2
      adjacent L2 L1

      attached P1 L1
      attached Q1 L1
      attached P2 L2
      attached Q2 L2

      belong K1 L1
      belong K2 L2

      in' CA P1
      in' CB P1
      in' CC P1

      in' CD Q1
      in' CE Q1
      in' CF Q1

      on CA Pallet
      on CB CA
      on CC CB

      on CD Pallet
      on CE CD
      on CF CE

      top CC P1
      top CF Q1
      top Pallet P2
      top Pallet Q2

      at R1 L1
      unloaded R1
      free L2

      empty K1
      empty K2
    ]

  let goal =
    preds [
      in' CA P2
      in' CB Q2
      in' CC P2
      in' CD Q2
      in' CE Q2
      in' CF Q2
    ]

  open GraphPlan

  [<EntryPoint>]
  let main argv =
    let mutable layer = getInitialLayer allActions (init, Set.empty)
    let mutable level = 0

    let printLayer (Layer((add, del), acts)) =
      printfn "%d+ %d- = %d Ps" (Set.count add) (Set.count del) (Set.count add + Set.count del)
      
      add |> Set.iter (fun (n, v) -> printfn "  %s" <| Pred(true, n, v).StripsForm)
      del |> Set.iter (fun (n, v) -> printfn "  %s" <| Pred(false, n, v).StripsForm)

      printfn "%d As" (Set.count acts)
      acts |> Set.iter (fun a -> printfn "  %s" a.StripsForm)

    while not (satisfiesGoal layer goal) do
      printfn "Level %d" level
      printLayer layer
      layer <- getNextLayer allActions layer
      level <- level + 1
    printfn "Level %d" level
    printLayer layer
    0

