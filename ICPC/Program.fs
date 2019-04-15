module ICPC
open System
open System.Net


//discriminated union to be used when spliting up  string into tokens
 type SplitUp =
    |Word of string
    |Comma
    |Fullstop
    |Space

let splitLine = (fun (line : string) -> Seq.toList (line.Split ' ')) 
//To convert the list of characters to string 
let chartostring (xy:char list) =
        let sb = System.Text.StringBuilder(xy.Length)
        xy |> List.iter (sb.Append >> ignore)
        sb.ToString()
 
 
 //Adding  comma to succeeding word
let rec addPostComma counter oldlist newlist k =
   match counter < List.length oldlist with
   |true -> match Word k = oldlist.[counter] with
            |false ->let newlist = oldlist.[counter]::newlist
                     addPostComma (counter+1) oldlist newlist k
            |true -> 
                     match not(oldlist.[counter+1]=Comma) && not(oldlist.[counter+1] =Fullstop) with
                     |true -> let newlist = Word k::newlist
                              let newlist = Comma::newlist
                              addPostComma (counter+1) oldlist newlist k
                     |false ->let newlist = Word k::newlist
                              addPostComma (counter+1) oldlist newlist k
    |false -> newlist

 //Adding comma to the preceding word
let rec addPreComma counter oldList newList k = 
   match counter < List.length oldList with
   |true -> 
            let result = oldList.[counter]
            match Word k = oldList.[counter] with
            |false -> let newlist = oldList.[counter]::newList 
                      addPreComma (counter+1) oldList newlist k
            |true -> match not(counter=0) && not(oldList.[counter-1]=Comma) && not(oldList.[counter-1]=Fullstop) with
                     |true -> let newlist = Comma::newList
                              let newlist = Word k::newlist
                              addPreComma(counter+1) oldList newlist k
                     |false -> let newlist = Word k::newList
                               addPreComma (counter+1) oldList newlist k
   |false -> newList

let rec checkIfValid charlist counter =
 match charlist with 
 |[] -> []
 |_ ->
   match counter < List.length charlist with
   |true -> match System.Char.IsLetter(charlist.[counter]) || (charlist.[counter]).Equals(' ') || (charlist.[counter]).Equals(',') || (charlist.[counter]).Equals('.') with
               |true -> match charlist.[counter] with
                         |' ' -> match counter+1 < List.length charlist with
                                |true -> match charlist.[counter+1] with
                                         |' ' -> []
                                         |',' -> []
                                         |'.' -> []
                                         |_ -> checkIfValid charlist (counter+1)
                                |false -> checkIfValid charlist (counter+1)
                        
                         |',' -> match counter+1 < List.length charlist with
                                |true -> match charlist.[counter+1] with 
                                         |' '-> checkIfValid charlist (counter+1)
                                         |',' ->[]
                                         |'.' ->[]
                                         |_ -> []
                                |false -> checkIfValid charlist (counter+1)
                         |'.' -> match counter+1 < List.length charlist with
                                |true-> match charlist.[counter+1] with 
                                        |' ' -> checkIfValid charlist (counter+1)
                                        |',' -> []
                                        |'.' -> []
                                        |_ -> checkIfValid charlist (counter+1)
                                |false -> checkIfValid charlist (counter+1)
                       
                         |_ -> match counter+1 < List.length charlist with
                              |true -> match charlist.[counter+1] with
                                       |_ -> checkIfValid charlist (counter+1)
                              |false -> checkIfValid charlist (counter+1)
               |false -> []
   |false -> charlist
    
let commaSprinkler input =
   let stringlist = splitLine input
   let stringlist = match stringlist.Head with
                    |"," -> []
                    |" " ->[]
                    |"" ->[]
                    |"." ->[]
                    |_ -> stringlist
   let charlist = List.ofSeq input
   let answer = checkIfValid charlist 0 

   let stringlist = 
      match answer with
      |[] -> []
      |_ -> stringlist
   let tList = []

   //A function to make a split up list
   let rec SearchThroughList slist counter tList =
      match counter < List.length slist with
        |true -> let string = slist.[counter]
                 let charlist = List.ofSeq string
                 let charlistr = List.rev charlist
                 let tList = 
                  match charlistr with 
                  |[] -> []
                  |head::tail -> match head with 
                                 |',' -> 
                                        match tail with 
                                        |_ -> let wordstring = chartostring (List.rev tail)
                                              let result = Word wordstring
                                              let tList = result::tList
                                              Comma::tList
                                 |'.' -> 
                                        match tail with 
                                        |_ -> let wordstring = chartostring (List.rev tail)
                                              let result = Word wordstring
                                              let tlist = result::tList
                                              Fullstop::tlist
                                 |_ -> let kstring = chartostring (charlist)
                                       let result = Word kstring
                                       result::tList
                 SearchThroughList slist (counter+1) tList
        |false -> tList
   let tlist = SearchThroughList stringlist 0 tList
   let SpList = List.rev tlist

   //Create newSplitList then find words that have a pre or post coomas and add them to a newSplitList.
   let rec CommaWords tlist count newlist =
        match count < List.length newlist with
        |true ->
            let index1 = count-1
            let index2 = count+1
            let answer = newlist.[count]
            match answer with 
              |Comma -> match newlist.[index1] with
                        |Word a -> let result = a
                                   let newlist = addPostComma 0 newlist [] result
                                   let newlist = List.rev newlist
                                   match newlist.[index2] with
                                   |Word a -> let result = a
                                              let newlist = addPreComma 0 newlist [] result
                                              let newlist = List.rev newlist
                                              CommaWords tlist (count+1) newlist
                        |_ -> match newlist.[index2] with
                              |Word a -> let result = a
                                         let newlist = addPreComma 0 newlist [] result
                                         let newlist = List.rev newlist
                                         CommaWords tlist (count+1) newlist
              |_ -> 
                   CommaWords tlist (count+1) newlist
        |false -> newlist
   let newSplitList = CommaWords SpList 0 SpList
   
 
   //recompose list from being split(tokens) to strings
   let newStrlist =[]
   let LengthCheckList = (List.length newSplitList)-1
   let rec recomposeList oldSplitList counter newStrlist =
     match counter < List.length oldSplitList with
     |true-> match oldSplitList.[counter] with
             |Fullstop -> let newslist =  "."::newStrlist
                          match counter < LengthCheckList with
                          |true -> //let newslist = " "::newslist
                                   recomposeList oldSplitList (counter+1) newslist
                          |false -> recomposeList oldSplitList (counter+1) newslist
             |Comma -> let newslist = ","::newStrlist
                       recomposeList oldSplitList (counter+1) newslist
             |Word z -> match (counter=0) with 
                        |true -> let newslist = z::newStrlist
                                 recomposeList oldSplitList (counter+1) newslist
                        |false -> match not(counter=0) && oldSplitList.[counter-1]=Comma with
                                  |true ->let newslist = " "::newStrlist
                                          let newslist = z::newslist
                                          recomposeList oldSplitList (counter+1) newslist
                                  |false ->let newslist = " "::newStrlist
                                           let newslist = z::newslist
                                           recomposeList oldSplitList (counter+1) newslist
     |false -> newStrlist
   let newslist = recomposeList newSplitList 0 [] 
   let newlist = List.rev newslist

   match newlist with           //Error Cases 
   |[] -> None 
   |_ -> match List.length newlist with
         |1 -> None
         |_ -> match newlist.Head with
               |"," ->None
               |"." ->None
               |_ -> 
                     match List.last newlist with
                     |" " -> None
                     |_ -> let newstring = newlist |> List.fold (+) ""
                        
 
                           match (String.exists (fun c -> System.Char.IsUpper(c)) newstring) with
                                 |true -> None
                                 |false -> Some newstring


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
            




