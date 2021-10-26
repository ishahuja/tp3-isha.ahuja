#let rec length = function 
      [] -> 0
   | e :: l -> 1+length(l);;

#let add_occ i hist =
  if i<0 
  then invalid_arg "add_occ: i should be >=0"
  else 
  if i > (length (hist)-1)
  then invalid_arg "add_occ: hist too short"
  else 
    let rec add (i, hist)= 
      match (i, hist) with 
      |(0, e::p) -> (e+1)::p
      |(_, e::p) -> e::add((i-1), p)
    in add (i, hist);;



#let rec append l2 l3 = match l2 with 
    [] -> l3
  |e::p -> e::append p l3 ;;

#let rec hist_sort = function
  | [] -> []
  | [0] -> [0]
  | e::l ->
      let rec part = function 
        | (l,r,[]) -> append (hist_sort l) (e::hist_sort r)
        | (l,r,h::t) -> if (h <= e)
            then part (h::l,r,t)
            else part (l,h::r,t)
      in 
      part ([],[],l) ;;

