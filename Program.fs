open System.Numerics
open System.Text.RegularExpressions

// board state
type CellularAutomata = {
    width:BigInteger;
    height:BigInteger;
    livingCells: Set<BigInteger * BigInteger>
}

// get x coordinate of a cell
let xOf cell = fst cell

// get y coordinate of a cell
let yOf cell = snd cell

// get board of specified size with no living cells
let empty width height = { width = width; height = height; livingCells = Set.empty }

let one = bigint(1)
let zero = bigint(0)

// true if the cell is within the board dimensions
let isOnBoard ca (x, y) =
    x >= zero && x < ca.width && y >= zero && y < ca.height

// get all the neighbors of the specified cell (but not the cell itself)
let neighbors (x, y) ca =
    Set
        .empty
        .Add((x - one, y - one))
        .Add((x - one, y))
        .Add((x - one, y + one))
        .Add((x, y - one))
        .Add((x, y + one))
        .Add((x + one, y - one))
        .Add((x + one, y))
        .Add((x + one, y + one)) 
        |> Set.filter (isOnBoard ca)


// get the number of living neighbors of the specified cell
let numberOfLivingNeighbors cell ca =
    neighbors cell ca |> Set.intersect ca.livingCells |> Set.count

// get state of specified cell
let get x y ca =
    ca.livingCells |> Set.contains (x, y)

// set state of specified cell
// this is probably horribly inefficient due to copying the whole set every time. I'm not sure
// how this copying cost is normally minimized in functional pogramming :/
let set x y isAlive ca =
    let isOnBoard = isOnBoard ca (x, y)
    match isOnBoard with
        | true ->
            match isAlive with
                | true -> { ca with livingCells = Set.add (x, y) ca.livingCells }
                | false -> { ca with livingCells = Set.remove (x, y) ca.livingCells }
        | false -> ca              

// Update the specified cell once
let nextCell cell ca =
    let isAlive = get (xOf cell) (yOf cell) ca
    let numberOfLivingNeighbors = numberOfLivingNeighbors cell ca
    let kill = set (xOf cell) (yOf cell) false ca
    let spawn = set (xOf cell) (yOf cell) true ca
    let nop = ca

    match (isAlive, numberOfLivingNeighbors) with
        | (true, 2)
        | (true, 3) -> nop
        | (true, _) -> kill
        | (false, 3) -> spawn
        | (false, _) -> nop

// Gets all cells that can potentially change state in the next update
// which is all living cells and their neighbors
let cellsThatNeedUpdate ca =
    Set.fold (fun cells cell -> neighbors cell ca |> Set.add cell |> Set.union cells) Set.empty ca.livingCells   

// Update the whole board once
let next ca =
    let cellsThatNeedUpdate = cellsThatNeedUpdate ca
    // TODO is the order of update important?
    Set.fold (fun c cell -> nextCell cell c) ca cellsThatNeedUpdate

// Active pattern for matching an integer string
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

// Active pattern for matching a bool string
let (|Boolean|_|) (str: string) =
   let mutable boolvalue =  false
   if System.Boolean.TryParse(str, &boolvalue) then Some(boolvalue)
   else None

// Active pattern for matching a string described by a regex
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

// Active pattern for matching the 'get' CLI command: 'get x y'
let (|GetCmd|_|) str =
    let rgx = "get ([0-9]+) ([0-9]+)"
    match str with
        | ParseRegex rgx [ Integer x; Integer y ] -> Some(bigint(x), bigint(y))
        | _ -> None

// Active pattern for matching the 'set' CLI command: 'set x y v' 
let (|SetCmd|_|) str =
    let rgx = "set ([0-9]+) ([0-9]+) (true|false)"
    match str with
        | ParseRegex rgx [ Integer x; Integer y; Boolean v ] -> Some(bigint(x), bigint(y), v)
        | _ -> None

// Active pattern for matching the 'run' CLI command: 'run n' 
let (|RunCmd|_|) str =
    let rgx = "run ([0-9]+)"
    match str with
        | ParseRegex rgx [ Integer n ] -> Some(n)
        | _ -> None

// Draw a '🐞' in the console at the specified cell position with a given offset
let printCell xOff yOff (x, y) =    
    let xs = bigint.Subtract(x, xOff)
    let xsi : int = int xs 
    let ys = bigint.Subtract(y, yOff)
    let ysi : int = int ys
    System.Console.SetCursorPosition(xsi, ysi)
    System.Console.Write("🐞")

// Draw all the cells to the console with the given offset
let rec printCells xOff yOff cells =
    match cells with
        | [] -> ()
        | cell::rest -> 
            printCell xOff yOff cell
            printCells xOff yOff rest

// Make a graphical representation of the board state in the console
let printBoard ca =
    let cells = cellsThatNeedUpdate ca |> Set.toList
    match cells with
        | [] -> printfn "There are no living cells"
        | _ ->
            System.Console.Clear()

            let minX = cells |> List.minBy xOf |> xOf            
            let minY = cells |> List.minBy yOf |> yOf
            printCells minX minY (ca.livingCells |> Set.toList)

            let maxY = (cells |> List.maxBy yOf |> yOf) - minY
            let maxYi : int = int maxY
            System.Console.SetCursorPosition(0, maxYi)
            System.Console.WriteLine()            

// Update the state n times
let rec run ca n =
    match n with
        | 0 -> ca
        | _ ->
            let nextCa = next ca
            run nextCa (n-1)

// Wait for a command from the user and process it
let rec doCommand ca =
    printf "> "
    let cmd = System.Console.ReadLine()
    match cmd with
    | SetCmd ( x, y, v ) -> 
        let nextCa = set x y v ca
        printBoard nextCa
        doCommand nextCa
    | GetCmd ( x, y ) ->
        let state = get x y ca
        printfn "%b" state
        doCommand ca 
    | "next" -> 
        let nextCa = next ca
        printBoard nextCa
        doCommand nextCa
    | RunCmd n ->
        let nextCa = run ca n
        printBoard nextCa
        doCommand nextCa
    | "print" -> 
        printBoard ca 
        doCommand ca
    | "exit" ->
        ca       
    | _ ->
        printfn "Unknown command: %s" cmd
        printfn ""
        printfn "Use one of the following:"
        printfn ""
        printfn "set x:int y:int v:bool   set the state of a cell e.g.: set 5 9 true"
        printfn "get x:int y:int          get the state of a cell e.g.: get 5 9"
        printfn "next                     advance the state by one step"
        printfn "run n:int                advance the state by n steps e.g.: run 10"
        printfn "print                    display the current state"
        printfn "exit                     exit the application"

        doCommand ca 


[<EntryPoint>]
let main argv =

    let mutable ca = empty (bigint(100)) (bigint(100))

    doCommand ca |> ignore

    printfn "Bye!"
    0
