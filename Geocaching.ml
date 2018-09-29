(* Geocaching module body *)

(* 
Aluno 1: 43914 Joao Reis
Aluno 2: 42997 Francisco Cardoso

Comentario:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)


(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789
   80 columns
*)

let altitudeUnknown = -32768 ;;


type cache = {          (* Example: *)
    code: string;       (* "GCK1JY" (url: http://coord.info/GCK1JY) *)
    name: string;       (* "Atlantis [Pico]" *)
    state: string;      (* "ARQUIPELAGO DOS ACORES" *)
    owner: string;      (* "Joao&Olivia" *)
    latitude: float;    (* 38.468917 *)
    longitude: float;   (* -28.3994 *)
    kind: string;       (* "TRADITIONAL"  options: "MULTI", "PUZZLE", etc. *)
    size: string;       (* "REGULAR" options: "MICRO", "SMALL", "LARGE", etc. *)
    difficulty: float;  (* 2.0  options: 1.0, 1.5, 2.0, ..., 4.5, 5.0 *)
    terrain: float;     (* 4.5  options: 1.0, 1.5, 2.0, ..., 4.5, 5.0 *)
    status: string;     (* "AVAILABLE" options: "AVAILABLE", "DISABLED" *)
    hiddenDate: string; (* "2004/07/20" *)
    nFounds: int;       (* 196 *)
    nNotFounds: int;    (* 25 *)
    nFavourites: int;   (* 48 *)
    altitude: int       (* 2286  // -32768 significa altitude desconhecida *)
} ;;

let unknownCache = {
    code = ""; name = ""; state = ""; owner = "";
    latitude = 0.0; longitude = 0.0;
    kind = ""; size = ""; difficulty = 0.0; terrain = 0.0;
    status = ""; hiddenDate = "";
    nFounds = 0; nNotFounds = 0; nFavourites = 0;
    altitude = 0
} ;;

(* https://en.wikipedia.org/wiki/Haversine_formula *)
let haversine (lat1,lon1) (lat2,lon2) =
    let toRad deg = deg *. 3.1415926535898 /. 180.0 in
    let dLat = toRad (lat2 -. lat1) and dLon = toRad (lon2 -. lon1) in
    let sa = sin (dLat /. 2.0) and so = sin (dLon /. 2.0) in
    let a = sa *. sa +. so *. so *. cos(toRad lat1) *. cos(toRad lat2) in
        6372.8 *. 2.0 *. asin (sqrt a)
;;

let cacheDistance c1 c2 =
    haversine (c1.latitude, c1.longitude) (c2.latitude, c2.longitude)
;;

let loadCache ci =
    let code = input_line ci in
    let name = input_line ci in
    let state = input_line ci in
    let owner = input_line ci in
    let latitude = float_of_string (input_line ci) in
    let longitude = float_of_string (input_line ci) in
    let kind = input_line ci in
    let size = input_line ci in
    let difficulty = float_of_string (input_line ci) in
    let terrain = float_of_string (input_line ci) in
    let status = input_line ci in
    let hiddenDate = input_line ci in
    let nFounds = int_of_string (input_line ci) in
    let nNotFounds = int_of_string (input_line ci) in
    let nFavourites = int_of_string (input_line ci) in
    let altitude = int_of_string (input_line ci) in {
        code = code; name = name; state = state; owner = owner;
        latitude = latitude; longitude = longitude;
        kind = kind; size = size; difficulty = difficulty; terrain = terrain;
        status = status; hiddenDate = hiddenDate;
        nFounds = nFounds; nNotFounds = nNotFounds; nFavourites = nFavourites;
        altitude = altitude
    }
;;

let rec loadCaches ci =
	try
		let x = loadCache ci in
			x :: loadCaches ci
	with End_of_file -> []
;;			

let load filename = (* post: result is never [] *)
    let ci = open_in filename in
	let lc = loadCaches ci in
		close_in ci;
    
    if lc = [] 
		then raise (Arg.Bad "load : premature end of file ")
    else 
		lc
;;

let rec count l =
    match l with
        [] -> 0
      	| x::xs -> 1 + count xs
;;

let rec insertOrdDate v l =
    match l with
        [] -> [v]
     	| x::xs ->
     		    if v.hiddenDate >= x.hiddenDate 
			then v::x::xs
          	    else 
			x::insertOrdDate v xs
;;

let rec hiddenDateSort l =
	match l with
		[] -> []
		| x::xs ->
				insertOrdDate x (hiddenDateSort xs)
;;
			
let rec insertOrdAltitude v l =
    match l with
        [] -> [v]
        | x::xs ->
		   if v.altitude >= x.altitude 
			then v::x::xs
          	   else 
			x::insertOrdAltitude v xs
;;

let rec altitudeSort l = 
	match l with
		[] -> []
		| x::xs ->
				insertOrdAltitude x (altitudeSort xs)
;;

let rec insertOrdFounds v l =
	match l with
		[] -> [v]
		| x::xs -> 
			   if v.nFounds >= x.nFounds 
				then v::x::xs
			   else
				x::insertOrdFounds v xs
;;

let rec nFoundsSort l =
 	match l with 
		[] -> []
		| x::xs -> 
				insertOrdFounds x (nFoundsSort xs)
;;

let auxComparator x y = 
	if x.code > y.code
		then x
	else
		y
;;

let rec northmost l =
	match l with
		[] -> unknownCache
		|x::[] -> x
		|x::xs -> let y = northmost xs in
			if x.latitude > y.latitude 
				then x
			else if x.latitude < y.latitude
				then y
			else
				auxComparator x y
			
;;

let rec southmost l = 
	match l with 
		[] -> unknownCache
		|x::[] -> x
		|x::xs -> let y = southmost xs in
			if x.latitude < y.latitude 
				then x
			else if x.latitude > y.latitude
				then y
			else
				auxComparator x y
;;

let rec eastmost l = 
	match l with
		[] -> unknownCache
		|x::[] -> x
		|x::xs -> let y = eastmost  xs in
			if x.longitude > y.longitude 
				then x
			else if x.longitude < y.longitude
				then y
			else 
				auxComparator x y  

;;

let rec westmost l = 
	match l with
  		[]-> unknownCache
		|x::[] -> x
		|x::xs -> let y = westmost xs in
			if x.longitude < y.longitude 
				then x
			else if x.longitude > y.longitude
				then y
			else 
				auxComparator x y
;;


let rec pack l =   
   match l with
       [] -> [] 
	| [x] -> [(x,1)]
	| x::xs -> let(y,n)::ys = pack xs in
			if x = y 
				then (y,n+1)::ys
			else
				(x,1)::(y,n)::ys
;;



let compareCount (x,a) (y,b) = if compare a b = 0
					then compare y x
						
				     	else
						compare b a
			
;;
let getCacheOwner cache = cache.owner
;;
    
let listOfOwners l = List.map getCacheOwner l
;;    

let ownerCount l = List.sort (compareCount) (pack(listOfOwners l))
	
;;



let getCacheKind cache = cache.kind
;;

let listOfKinds l = List.map getCacheKind l
;;


let kindCount l = List.sort (compareCount) (pack(listOfKinds l)) 
;;



let getCacheSize cache = cache.size
;;

let listOfSizes l = List.map getCacheSize l
;;

let sizeCount l = List.sort (compareCount) (pack(listOfSizes l))
;;




let getCacheStates cache = cache.state
;;

let listOfStates l = List.map getCacheStates l
;;

let stateCount l = List.sort (compareCount) (pack(listOfStates l))
;;





let getCacheTerrain cache = cache.terrain
;;

let listOfTerrains l = List.map getCacheTerrain l
;;

let terrainCount l = List.sort (compareCount) (pack(listOfTerrains l))
;;




let getCacheDifficulty cache = cache.difficulty
;;

let listOfDifficulties l = List.map getCacheDifficulty l
;;

let difficultyCount l = List.sort (compareCount) (pack(listOfDifficulties l))
;;


	

let matrix81 l = [[]]
;;

let datesMatrix l = [[]] ;;

let neighborsCount l d = [] ;;

