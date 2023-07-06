module Machine

// Type definitions
type Symbol = char
type State = string
type Direction = Left | Right
type TransitionTable = Map<State, Map<Symbol, State * Symbol * Direction>>
type Tape = { Left: Symbol list; Current: Symbol; Right: Symbol list }
type Machihne = {
    State: State;
    Accepting: Set<State>;
    Tape: Tape
    Transitions: TransitionTable
}

let blankSymbol = '_'

let initializeTape (input: Symbol list) =
    match input with
    | [] -> { Left = []; Current = blankSymbol; Right = [] }
    | c::cs -> { Left = []; Current = c; Right = cs }

let initializeMachine (transitions: TransitionTable) (state: State) (accepting: Set<State>) (input: Symbol list) =
    { State = state; Accepting = accepting; Tape = initializeTape input; Transitions = transitions }

let moveLeft (tape: Tape) =
    match tape with
    | { Left = []; Current = c; Right = r }
        -> failwith "Cannot move left"
    | { Left = l::ls; Current = c; Right = r }
        -> { Left = ls; Current = l; Right = c::r }

let moveRight (tape: Tape) =
    match tape with
    | { Left = l; Current = c; Right = [] }
        -> { Left = c::l; Current = blankSymbol; Right = [] }
    | { Left = l; Current = c; Right = r::rs }
        -> { Left = c::l; Current = r; Right = rs }

let move (direction: Direction) (tape: Tape) =
    match direction with
    | Left -> moveLeft tape
    | Right -> moveRight tape

let write (symbol: Symbol) (tape: Tape) = { tape with Current = symbol }

let rec run (machine: Machihne) =
    if Set.contains machine.State machine.Accepting then
        machine
    else
        if not (Map.containsKey machine.Tape.Current machine.Transitions.[machine.State]) then
            // no transition for current state and symbol => halt
            machine
        else
            let newState, symbol, direction = machine.Transitions.[machine.State].[machine.Tape.Current]
            printfn "%A" machine
            printfn "New state: %A" newState
            printfn "Symbol: %A" symbol
            printfn "Direction: %A" direction
            let newTape = machine.Tape |> write symbol |> move direction
            let newMachine = { machine with State = newState; Tape = newTape }
            run newMachine
