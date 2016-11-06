(* ***********************)
(* Zad 1, Marcin Wawerka *)
(* Code review by :      *)
(* Miłosz Rzeźnikowski   *)


type wartosc = { valmin : float ; valmax : float ; inf : bool } (* inf = true -> (-inf, valmin] [valmax, inf) else [valmin, valmax] *)

let ( *. ) a b = 
	if (a = 0. && ( b = infinity || b = neg_infinity)) ||
	(b = 0. && ( a = infinity || a = neg_infinity) ) then 0. (* overloading *. *)
	else a *. b
	
let create x y a = {valmin = x; valmax = y; inf = a;}

let wartosc_dokladnosc x p = 
	let procent = (p *. x) /. 100. in
		let end1 =  x -. procent and end2 =  x +. procent in 
			create (min end1 end2) (max end1 end2) false

let wartosc_od_do x y = create x y false

let wartosc_dokladna x = create x x false

let in_wartosc x y = 
	match x.inf with
	|true -> y <= x.valmin || y >= x.valmax
	|false -> y >= x.valmin && y <= x.valmax
	
let min_wartosc x = 
	match x.inf with
	|true -> neg_infinity
	|false -> x.valmin
	
let max_wartosc x = 
	match x.inf with
	|true -> infinity
	|false -> x.valmax
	
let sr_wartosc x = 
	match x.inf with
	|true -> nan
	|false -> (min_wartosc x +. max_wartosc x) /. 2.

(* if two intervals overlap and reach -inf and +inf then connect them and make one *)
let connect x = 
	if x.valmin >= x.valmax && x.inf = true then create 0. 0. true
	else x

let plus x y = 
	match (x.inf, y.inf) with
	|(true, true) -> create 0. 0. true
	|(true, false) -> connect (create (x.valmin +. y.valmax) (x.valmax +. y.valmin) true)
	|(false, true) -> connect (create (y.valmin +. x.valmax) (y.valmax +. x.valmin) true)
	|(false, false) -> connect (create (x.valmin +. y.valmin) (x.valmax +. y.valmax) 
	(x.valmin +. y.valmin = neg_infinity && x.valmax +. y.valmax = infinity) )

let zmien x = 
	create (x.valmax *. (-1.)) (x.valmin *. (-1.)) x.inf
	
let minus x y =
	plus x (zmien y)

(* multiplying [a,b] * [c,d] *)
let minmax x y = 
	let a = x.valmin *. y.valmin and b = x.valmax *. y.valmin 
	and c = x.valmin *. y.valmax and d = x.valmax *. y.valmax in
		connect (create (min (min a b) (min c d)) (max (max a b) (max c d)) 
		( (min (min a b) (min c d)) = neg_infinity && (max (max a b) (max c d)) = infinity ) )

(* dividing (-inf, valmin] [valmax, inf) into two separate intervals or returning two of the same if first interval is compact *)		
let divide x = 
	if x.inf then (create neg_infinity x.valmin false, create x.valmax infinity false)
	else (x, x)

(* union of intervals, elements of list are (-inf, a] or [a, inf) or [0,0] - but only when x is [0,0] too *)	
let rec scal x lst =
	match lst with
	|[] -> x
	|h::t -> 
		if x.inf then 
			if h.valmax = infinity then scal (create x.valmin (min x.valmax h.valmax) true) t
			else if h.valmin = neg_infinity then scal (create (max h.valmax x.valmin) x.valmax true) t
			else x 
		else 
			if (x.valmin = neg_infinity && h.valmax = infinity) then scal (connect (create x.valmax h.valmin true)) t
			else if (h.valmin = neg_infinity && x.valmax = infinity) then scal (connect (create h.valmax x.valmin true)) t
			else scal (connect (create (min x.valmin h.valmin) (max h.valmax x.valmax) false)) t
			
(* multiplication is made interval by interval and then the product intervals are unioned *)		
let razy x y =
	let a = fst (divide x) and b = snd (divide x) and c = fst (divide y) and d = snd (divide y) in
		match (x.inf, y.inf) with
		| (false, false) -> minmax x y
		| (true, false) -> scal (minmax a c) [minmax b c]
		| (false, true) -> scal (minmax a c) [minmax a d]
		| (true, true) -> scal (minmax a c) [minmax b c; minmax b d; minmax a d]

(* create inverse of interval *)
let rec odwroc x = 
	let a = x.valmin and b = x.valmax and c = x.inf in
		match c with
		|true -> scal (odwroc(create neg_infinity a false)) [odwroc(create b infinity false)]
		|false ->
			if (a,b) = (0.,0.) then (create nan nan false)
			else
				if b = 0. then create neg_infinity (1. /. a) false else
				if a = 0. then create (1. /.  b) infinity false else
				if (a > 0. || b < 0.) then create (1. /. b) (1. /. a) false else
				scal (odwroc(create a 0. false)) [odwroc(create 0. b false)]
				
let podzielic x y = 
	razy x (odwroc y)
