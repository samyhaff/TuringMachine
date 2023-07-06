open Machine

let transitionTable =
    Map.empty
    |> Map.add "a" (Map.empty |> Map.add '_' ("b", '0', Right))
    |> Map.add "b" (Map.empty |> Map.add '_' ("a", '1', Right))
let initialState = "a"
let acceptingStates = Set.singleton "b" // never stops
let input = []
let machine = initializeMachine transitionTable initialState acceptingStates input
run machine |> ignore
