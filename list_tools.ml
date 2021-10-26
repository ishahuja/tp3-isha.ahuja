#let rec length = function 
        [] -> 0
      | e :: l -> 1+length(l);;

#let rec nth n l =
      if n<0 then invalid_arg "n<0"
      else 
    match (n,l) with 
    |(_,[]) -> failwith "list is too short"
    |(0,h::t) -> h
    |(n,h::t) -> nth (n-1) t ;;

#let rec is_pos l = match l with 
    [] -> true
  |p :: q -> if (p>0) then is_pos q else false ;;

#let rec get_max = function
      |e::[] -> e
      |e::l -> let max = get_max l in 
          if e>max then e else max
      |[] -> failwith "empty" ;;


#let rec init_list a b = 
  match a with n when a<0 -> invalid_arg "no"
             |0 -> []
             |_ -> b :: init_list(a-1) b ;;

#let rec append l2 l3 = match l2 with 
    [] -> l3
  |e::p -> e::append p l3 ;;

#let rec put_list x = function
    e::[] -> x::e::[]
  |e::t -> e::put_list x t
  |_ -> invalid_arg "no" ;;



#let rec init_board (l,c) m = 
  if l<0 && c<0 then invalid_arg "not possible" 
  else 
   match (l,c) with 
    |(0,_)|(_,0) -> [] 
    |(1,c) -> init_list c m ::[]
|(_,_) -> init_list c m ::(l-1,c) m ;;

#let rec length = function 
        [] -> 0
      | e :: l -> 1+length(l);;
#let rec is_board list = match list with 
    [ ] -> true
  |e::[] -> true
  |e::e1::l -> length e = length e1 && is_board (e1::l) ;;
