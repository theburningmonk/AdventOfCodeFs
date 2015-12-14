#load "Day07.fs"

open System.Text.RegularExpressions

open Day07

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Signal = uint16 option

let inline signal n = Some <| uint16 n

type Source = 
    | Value  of Signal
    | Wire   of string
    | Gate   of Gate

and Gate = 
    | Unary  of Source * (Signal -> Signal)
    | Binary of Source * Source * (Signal -> Signal -> Signal)

type Circuit =
    {
        Wires : Map<string, Signal>
        Connections : Map<string, Source>
    }

    static member Default =
        {
            Wires = Map.empty<string, Signal>
            Connections = Map.empty<string, Source>
        }

    member x.Connect (name, gate) =
        { x.AddWireIfNotExist(name)
            with Connections = 
                x.Connections.Add(name, gate) }

    member x.AddWireIfNotExist (name) =
        match x.Wires.TryFind name with
        | Some _ -> 
            x
        | _      -> 
            { x with Wires = 
                x.Wires.Add(name, None) }

    member x.AddWireOrUpdate (name, signal : Signal) =
        match x.Wires.TryFind name with
        | Some oldSignal when signal = oldSignal -> 
            x

        | Some _ ->
            { x with Wires = x.Wires
                                .Remove(name)
                                .Add(name, signal) }
        | _ -> 
            { x with Wires = x.Wires.Add(name, signal) }

[<AutoOpen>]
module Gates =
    let AND wire1 wire2 = 
        Binary (wire1, wire2, (fun x y -> 
            match x, y with
            | Some x, Some y -> Some (x &&& y)
            | _ -> None))

    let OR wire1 wire2 = 
        Binary (wire1, wire2, (fun x y ->
            match x, y with
            | Some x, Some y -> Some (x ||| y)
            | _ -> None))

    let LSHIFT n wire = 
        Unary (wire, (fun x -> 
            x |> Option.bind (fun x' -> 
                x' <<< n |> Some)))

    let RSHIFT n wire = 
        Unary (wire, (fun x ->
            x |> Option.bind (fun x' ->
                x' >>> n |> Some)))

    let NOT wire = 
        Unary (wire, (fun x ->
            x |> Option.bind ((~~~) >> Some)))

let circuit =
    input.Split '\n'
    |> Seq.fold (fun (circuit : Circuit) line ->
        match line with
        | Regex "NOT ([a-z]+) -> ([a-z]+)" 
                [ in'; out ] ->
            circuit.Connect(
                out, 
                Gate <| NOT (Wire in'))

        | Regex "([a-z]+) AND ([a-z]+) -> ([a-z]+)" 
                [ in'; in''; out ] ->
            circuit.Connect(
                out, 
                Gate <| AND (Wire in') (Wire in''))

        | Regex "([0-9]+) AND ([a-z]+) -> ([a-z]+)" 
                [ in'; in''; out ] ->
            circuit.Connect(
                out, 
                Gate <| AND (Value <| signal in') (Wire in''))

        | Regex "([a-z]+) OR ([a-z]+) -> ([a-z]+)"
                [ in'; in''; out ] ->
            circuit.Connect(
                out, 
                Gate <| OR (Wire in') (Wire in''))

        | Regex "([a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)"
                [ in'; n; out ] ->
            circuit.Connect(
                out, 
                Gate <| LSHIFT (int n) (Wire in'))

        | Regex "([a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)"
                [ in'; n; out ] ->
            circuit.Connect(
                out, 
                Gate <| RSHIFT (int n) (Wire in'))

        | Regex "([a-z]+) -> ([a-z]+)"
                [ in'; out ] ->
            circuit.Connect(out, Wire in')

        | Regex "([0-9]+) -> ([a-z]+)"
                [ n; out ] ->
            circuit.Connect(out, Value <| signal n)

        | x -> failwithf "Notsupported : %s\n%A" x circuit
        ) Circuit.Default

let rec evaluate (circuit : Circuit) wire =
    match circuit.Wires.[wire] with
    | Some signal -> 
        circuit
    | _ ->
        let source = circuit.Connections.[wire]
        let (signal : Signal), (circuit : Circuit) = 
            evaluateSource circuit source
        circuit.AddWireOrUpdate(wire, signal)

and evaluateSource (circuit : Circuit) source =
    match source with
    | Value signal ->
        signal, circuit

    | Wire in' ->
        let circuit = evaluate circuit in'
        let signal  = circuit.Wires.[in']
        signal, circuit

    | Gate (Unary (source, f)) ->
        let signal, circuit = evaluateSource circuit source
        f signal, circuit

    | Gate (Binary (source1, source2, f)) ->
        let signal1, circuit = evaluateSource circuit source1
        let signal2, circuit = evaluateSource circuit source2
        f signal1 signal2, circuit
        
module Map =
    let keys (map : Map<'a, 'b>) = 
        map |> Seq.map (fun (KeyValue(k, _)) -> k)

let evalAll (circuit : Circuit) =
    circuit.Wires
    |> Map.keys
    |> Seq.fold evaluate circuit

let aSignal = (evalAll circuit).Wires.["a"]