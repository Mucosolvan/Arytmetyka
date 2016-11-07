(*********************************************************)
(* Arithmetics of inaccurate values.                     *)
(* In this code [a,b] means closed interval from a to b. *)
(*********************************************************)

(* Type representing inaccurate values. *)
type wartosc

(* wartosc_dokladnosc x p = [x - p/100 * x, x + p/100 * x] 
   initial condition : p > 0  *)
val wartosc_dokladnosc: float -> float -> wartosc    

val stworz: float -> float -> bool -> wartosc
(* wartosc_od_do x y = [x;y]
   initial condition : x <= y  *)
val wartosc_od_do: float -> float -> wartosc                            

(* wartosc_dokladna x = [x;x]  *)
val wartosc_dokladna: float -> wartosc   

(* in_wartosc w x = x in w *)
val in_wartosc: wartosc -> float -> bool 

(* min_wartosc w = minimum value in w          
   or neg_infinity if there is no infimum *)
val min_wartosc: wartosc -> float       

(* max_wartosc w = maximum value in w
   or infinity is there is no supremum *)
val max_wartosc: wartosc -> float       

(* Centre of [min_wartosc, max_wartosc]
   or nan if at least one of min_wartosc, max_wartosc is not defined *)
val sr_wartosc:  wartosc -> float       
    
(* Arithmetic operations on inaccurate values.          *)

(* Plus x y = {a + b: in_wartosc a x && in_wartosc b y} *)
val plus:      wartosc -> wartosc -> wartosc  

(* Minus x y = {a - b: in_wartosc a x && in_wartosc b y} *)
val minus:     wartosc -> wartosc -> wartosc 

(* Razy x y = {a * b: in_wartosc a x && in_wartosc b y} *)
val razy:      wartosc -> wartosc -> wartosc  

(* Podzielic x y = {a / b: in_wartosc a x && in_wartosc b y} *)
val podzielic: wartosc -> wartosc -> wartosc                         
