
{ type token = EOF | Word of string }

rule token = parse
| eof { EOF }
| ['a'-'z' 'A'-'Z']+ as word { Word(word) }
| _ { token lexbuf }

{    
let module StringMap = Map.Make(String) in
 
   let rec list2Map myList myMap =
   match myList with 
   [] -> myMap
   | hd::tl ->  
   if StringMap.mem hd myMap then
   list2Map tl (StringMap.add hd ((StringMap.find hd myMap)+1) myMap)
   else
   list2Map tl (StringMap.add hd 1 myMap) 
   in
   
   let tupleize k v myList = 
   myList@[(v,k)] 
   in
   
   let wordcount wc = 
   List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) wc
   in

   let remapList wordlist =
   let c = (StringMap.fold tupleize (list2Map wordlist StringMap.empty)) []  in
   (wordcount c) 
   in
   
   let strip myTupleList =
   let rec strip_in myTupleList myList = 
   match myTupleList with
   []->List.rev myList
   |hd::tl -> strip_in tl myList@[snd hd] in
   List.rev (strip_in myTupleList [] )
   in
  
   let lexbuf = Lexing.from_channel stdin in
   let wordlist =
   let rec next l =
   match token lexbuf with
   EOF -> l
   | Word(s) -> next (s :: l)
   in next []
   in
   
   
   List.iter print_endline (strip (remapList wordlist))
}
