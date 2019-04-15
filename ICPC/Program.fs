module ICPC
open System

let commaSprinkler input = failwith "Not implemented"

let rivers (input: string) =
    let result = Array.toList (input.Split ' ')
    let arrlen = result.Length
    let tester a = 
        match a = "" with
        |true -> true
        |_ ->false
    match  List.exists tester result    || arrlen <= 2 || input.Contains ',' || input.Contains '!' || input.StartsWith ' ' || input.EndsWith ' '  with // if theres only 1 word or no words or contains punctaution
    |true -> None
    |false ->  Some input

 
            



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
