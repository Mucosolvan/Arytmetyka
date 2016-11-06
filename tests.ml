(*

(* Do uprzyjemnienia toplevela: *)

let print_war fmt w = 
  Format.fprintf fmt "<%g, %g>" (min_wartosc w) (max_wartosc w);;

#install_printer print_war;;

*)



let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

let epsilon = 0.0001

let (=.) x y = (x-.epsilon <= y) && (y <= x+.epsilon) ;;


open Arytmetyka;;



Printf.printf "==== Testy obowiazkowe...\n";;

(* testy konstruktory-selektor *)

let x = wartosc_dokladnosc 7. 20.;;
test 31 (not (in_wartosc x 5.));;
test 32 (in_wartosc x 5.7);;
test 33 (in_wartosc x 6.);;
test 34 (in_wartosc x 7.);;
test 35 (in_wartosc x 8.);;
test 36 (in_wartosc x 8.3);;
test 37 (not (in_wartosc x 9.));;

test 39 (min_wartosc x =. 5.6);;
test 40 (max_wartosc x =. 8.4);;
test 41 (sr_wartosc x =. 7.);;


let x = wartosc_od_do 5.6 8.4;;
test 45 (not (in_wartosc x 5.));;
test 46 (in_wartosc x 5.6);;
test 47 (in_wartosc x 6.);;
test 48 (in_wartosc x 7.);;
test 49 (in_wartosc x 8.);;
test 50 (in_wartosc x 8.4);;
test 51 (not (in_wartosc x 9.));;

test 53 (min_wartosc x =. 5.6);;
test 54 (max_wartosc x =. 8.4);;
test 55 (sr_wartosc x =. 7.);;


let x = wartosc_dokladna 7.;;
test 59 (not (in_wartosc x 5.));;
test 60 (not (in_wartosc x 5.6));;
test 61 (not (in_wartosc x 6.));;
test 62 (in_wartosc x 7.);;
test 63 (not (in_wartosc x 8.));;
test 64 (not (in_wartosc x 8.4));;
test 65 (not (in_wartosc x 9.));;

test 67 (min_wartosc x =. 7.);;
test 68 (max_wartosc x =. 7.);;
test 69 (sr_wartosc x =. 7.);;


(* proste operacje *)

let x = wartosc_od_do 2. 3.;;
let y = wartosc_od_do 4. 5.;;
let z = plus x y;;

test 78 (min_wartosc z =. 6.);;
test 79 (sr_wartosc z =. 7.);;
test 80 (max_wartosc z =. 8.);;


let z = razy x y;;

test 85 (min_wartosc z =. 8.);;
test 86 (sr_wartosc z =. 11.5);;
test 87 (max_wartosc z =. 15.);;


let z = minus y x;;

test 92 (min_wartosc z =. 1.);;
test 93 (sr_wartosc z =. 2.);;
test 94 (max_wartosc z =. 3.);;


let z = podzielic y x;;

test 99 (min_wartosc z =. 4./.3.);;
test 100 (sr_wartosc z =. (4./.3.+.5./.2.)/.2.);;
test 101 (max_wartosc z =. 5./.2.);;

(* mnozenie dodatnie / ujemne *)

(* ++ ++ bylo *)

(* -+ ++ *)
let x = wartosc_od_do (-2.) 3.;;
let y = wartosc_od_do 4. 5.;;
let z = razy x y;;
test 111 (min_wartosc z =. -2. *. 5.);;
test 112 (max_wartosc z =. 3. *. 5.);;

(* -+ -+  ->  21,22 *)
let x = wartosc_od_do (-2.) 3.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 118 (min_wartosc z =. -4. *. 3.);;
test 119 (max_wartosc z =. 3. *. 5.);;

(* -+ -+  ->  12,11 *)
let x = wartosc_od_do (-3.) 2.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 125 (min_wartosc z =. -3. *. 5.);;
test 126 (max_wartosc z =. -3. *. -4.);;

(* -+ -+  ->  21,11 *)
let x = wartosc_od_do (-3.) 2.;;
let y = wartosc_od_do (-4.) 1.;;
let z = razy x y;;
test 132 (min_wartosc z =. -4. *. 2.);;
test 133 (max_wartosc z =. -3. *. -4.);;

(* -+ -+  ->  12,22 *)
let x = wartosc_od_do (-3.) 3.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 139 (min_wartosc z =. -3. *. 5.);;
test 140 (max_wartosc z =. 3. *. 5.);;


(* -- ++ *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do 4. 5.;;
let z = razy x y;;
test 147 (min_wartosc z =. -3. *. 5.);;
test 148 (max_wartosc z =. -2. *. 4.);;


(* -- -+ *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 155 (min_wartosc z =. -3. *. 5.);;
test 156 (max_wartosc z =. -3. *. -4.);;


(* -- -- *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do (-5.) (-4.);;
let z = razy x y;;
test 163 (min_wartosc z =. -2. *. -4.);;
test 164 (max_wartosc z =. -3. *. -5.);;


(* dzielenie dodatnie / ujemne *)

(* ++ ++ było *)

(* ++ -- *)
let x = wartosc_od_do 4. 16.;;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 175 (min_wartosc z =. 16. /. -2.);;
test 176 (max_wartosc z =. 4. /. -4.);;

(* -+ -- *)
let x = wartosc_od_do (-4.) 16.;;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 182 (min_wartosc z =. 16. /. -2.);;
test 183 (max_wartosc z =. -4. /. -2.);;

(* -+ ++ *)
let x = wartosc_od_do (-4.) 16.;;
let y = wartosc_od_do 2. 4.;;
let z = podzielic x y;;
test 189 (min_wartosc z =. -4. /. 2.);;
test 190 (max_wartosc z =. 16. /. 2.);;

(* -- -- *)
let x = wartosc_od_do (-16.) (-4.);;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 196 (min_wartosc z =. -4. /. -4.);;
test 197 (max_wartosc z =. -16. /. -2.);;


Printf.printf "==== Dzielenie przez 0...\n";;

(* dzielenie przez 0 *)


(* ++ ++ *)
let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 209 (min_wartosc z =. 1.);;
test 210 (max_wartosc z =. infinity);;

(* -- ++ *)
let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 216 (min_wartosc z =. neg_infinity);;
test 217 (max_wartosc z =. -1.);;

(* ++ -- *)
let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do (-1.) 0.;;
let z = podzielic x y;;
test 223 (min_wartosc z =. neg_infinity);;
test 224 (max_wartosc z =. -1.);;

(* -- -- *)
let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do (-1.) 0.;;
let z = podzielic x y;;
test 230 (min_wartosc z =. 1.);;
test 231 (max_wartosc z =. infinity);;


Printf.printf "==== Mnozenie przez nieskonczonosc...\n"

(* mnozenie przez plus nieskonczonosc *)

let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 241 (min_wartosc z =. 1.);;
test 242 (max_wartosc z =. infinity);;

let a = razy z y;;
test 245 (min_wartosc a =. 0.);;
test 246 (max_wartosc a =. infinity);;

let b = wartosc_od_do 2. 3.;;
let a = razy z b;;
test 250 (min_wartosc a =. 2.);;
test 251 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-2.) 3.;;
let a = razy z b;;
test 255 (min_wartosc a =. neg_infinity);;
test 256 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-3.) (-2.);;
let a = razy z b;;
test 260 (min_wartosc a =. neg_infinity);;
test 261 (max_wartosc a =. -2.);;


(* mnozenie przez minus nieskonczonosc *)

let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 269 (min_wartosc z =. neg_infinity);;
test 270 (max_wartosc z =. -1.);;

let a = razy z y;;
test 273 (min_wartosc a =. neg_infinity);;
test 274 (max_wartosc a =. 0.);;

let b = wartosc_od_do 2. 3.;;
let a = razy z b;;
test 278 (min_wartosc a =. neg_infinity);;
test 279 (max_wartosc a =. -2.);;

let b = wartosc_od_do (-2.) 3.;;
let a = razy z b;;
test 283 (min_wartosc a =. neg_infinity);;
test 284 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-3.) (-2.);;
let a = razy z b;;
test 288 (min_wartosc a =. 2.);;
test 289 (max_wartosc a =. infinity);;

Printf.printf "==== Przedziały z dziurami...\n";;

(* dzielenie przez zero *)
let j = wartosc_dokladna 1.;;
let x = wartosc_od_do (-1.) 1.;;


let y = podzielic j x;;
(* y = (-inf,-1) \cup (1,+inf) *)

test 301 (min_wartosc y =. neg_infinity);;
test 302 (max_wartosc y =. infinity);;
test 303 (in_wartosc y (-2.));;
test 304 (in_wartosc y (-1.));;
test 305 (not (in_wartosc y 0.));;
test 306 (in_wartosc y 1.);;
test 307 (in_wartosc y 2.);;
test 308 (in_wartosc y (-1.));;


let a = podzielic j y;;
(* a = (-1,1) *)
test 313 (min_wartosc a =. -1.);;
test 314 (max_wartosc a =. 1.);;
test 315 (in_wartosc a 0.);;


let d = wartosc_od_do 0.25 0.5;;
let z = plus y d;;
(* z = (-inf,-0.5) \cup (1.25,+inf) *)
test 321 (min_wartosc z =. neg_infinity);;
test 322 (max_wartosc z =. infinity);;
test 323 (in_wartosc z (-1.));;
test 324 (not (in_wartosc z (-0.25)));;
test 325 (not (in_wartosc z 0.));;
test 326 (not (in_wartosc z 1.));;
test 327 (in_wartosc z 1.5);;

let e = podzielic j z;;
(* e = (-2,0.8) *)
test 331 (min_wartosc e =. -2.);;
test 332 (max_wartosc e =. 0.8);;
test 333 (not (in_wartosc e (-3.)));;
test 334 (in_wartosc e (-1.));;
test 335 (in_wartosc e 0.);;
test 336 (in_wartosc e 0.5);;
test 337 (not (in_wartosc e 1.));;

(* Jeszcze mozna by zrobic dzielenie czegos skonczonego przez
   nieskonczonosc - z roznie dobranymi znakami *)

(* No i dzielenie nieskonczonosci przez nieskonczonosc *)
let j = wartosc_dokladna 1.;;
let x = wartosc_od_do 0. 1.;;
let z = podzielic j x;;
(* z = (1,+inf) *)
test 347 (min_wartosc z =. 1.);;
test 348 (max_wartosc z =. infinity);;

let a = podzielic z z;;
(* a = (0,+inf) *)
test 352 (min_wartosc a =. 0.);;
test 353 (max_wartosc a =. infinity);;



(* .... i pewnie sporo innych przykladow ... *)

Printf.printf "==== Moje przyklady...\n";;

let u = wartosc_od_do (-5.) (-1.);;
let v = wartosc_od_do (-1.) 3.;;

let x = podzielic u v;;
(* x = (-inf,-1/3) \cup (1,+inf) *)

test 400 (min_wartosc x =. neg_infinity);;
test 401 (max_wartosc x =. infinity);;
test 402 (sr_wartosc x == nan);;

let a = wartosc_dokladna (-1.);;
let b = wartosc_od_do 0. 1.;;

let c = podzielic a b;;
(* c = (-inf,-1) *)

let d = wartosc_dokladna 1.;;
let e = podzielic d c;;
(* e = (-1, -0) *)

test 403 (min_wartosc c =. neg_infinity);;
test 404 (max_wartosc c =. -1.);;
test 405 (sr_wartosc c =. neg_infinity);;
test 406 (min_wartosc e =. -1.);;
test 407 (max_wartosc e =. 0.);;
test 408 (sr_wartosc e =. -0.5);;

let f = wartosc_od_do 2. 4.;;
let g = podzielic f e;;
(* g = (-inf, -2) *)
let h = razy f e;;
(* h = (-4, -0) *)

test 406 (min_wartosc g =. neg_infinity);;
test 407 (max_wartosc g =. (-2.));;
test 408 (sr_wartosc g =. neg_infinity);;
test 406 (min_wartosc h =. (-4.));;
test 407 (max_wartosc h =. 0.);;
test 408 (sr_wartosc h =. (-2.));;

let pom = wartosc_od_do 1. 2.;;
let f = wartosc_dokladna 0.;;
let g = podzielic pom f;;
(* g = nan *)

test 450 (min_wartosc g <> min_wartosc g);;
test 451 (max_wartosc g <> min_wartosc g);;
test 452 (sr_wartosc g <> sr_wartosc g);;

let a = wartosc_od_do 4. 8.;;
let b = wartosc_od_do (-6.) 2.;;
let c = podzielic a b;;
(* c = (-inf,-2/3) \cup (2,+inf) *)

let d = wartosc_od_do (-6.) 0.;;
let e = podzielic a d;;
(* e = (-inf,-2/3) *)

test 453 (min_wartosc c = neg_infinity);;
test 454 (max_wartosc c = infinity);;

test 460 (min_wartosc e = neg_infinity);;
test 461 (max_wartosc e =. ((-2.) /. 3.));;
test 462 (sr_wartosc e =. neg_infinity);;

let w = plus x c;;
let q = podzielic d w;;

test 463 (min_wartosc w = neg_infinity);;
test 464 (max_wartosc w = infinity);;
test 473 (min_wartosc q = neg_infinity);;
test 474 (max_wartosc q = infinity);;

let a = wartosc_od_do (-1.) 0.;;
let b = wartosc_dokladna (-1.);;
let c = razy a b;;
let d = podzielic a c;;

test 500 (min_wartosc c = 0.);;
test 501 (max_wartosc c =. 1.);;
test 502 (sr_wartosc c =. 0.5);;
test 503 (min_wartosc d = neg_infinity);;
test 504 (max_wartosc d =. 0.);;
test 505 (sr_wartosc d =. neg_infinity);;


Printf.printf "==== Rozne smieszne...\n";;

let a = razy (wartosc_od_do 2. infinity) (wartosc_od_do neg_infinity 0.);;
let b = razy (wartosc_od_do 0. 10.) (wartosc_od_do neg_infinity (-5.));;
(* a = b = (-inf, -0) *)
let x = podzielic (wartosc_dokladna 10.) (wartosc_od_do (-2.) 5.);;
(* x = (-inf, -5) u (2, inf) *)
let c = razy x (wartosc_od_do 8. infinity);;
(* x = (-inf, -40) u (16, inf) *)

test 510 (min_wartosc a = neg_infinity);;
test 511 (max_wartosc a =. 0.);;
test 512 (not (in_wartosc a 0.5));;
test 513 (in_wartosc a 0.);;

test 514 (min_wartosc b = neg_infinity);;
test 515 (max_wartosc b =. 0.);;
test 516 (in_wartosc b (-0.5));;

test 517 (sr_wartosc c <> sr_wartosc c);;
test 518 (not (in_wartosc c 15.99));;
test 519 (in_wartosc c 16.);;
test 520 (in_wartosc c (-40.));;
test 521 (not (in_wartosc c (-39.)));;

let d = podzielic a (wartosc_od_do 2. 5.);;
test 530 (min_wartosc d = neg_infinity);;
test 531 (max_wartosc d =. 0.);;

let a = razy (wartosc_od_do 0. infinity) (wartosc_od_do (-2.) (-1.));;
(* a = (-inf, -0.) *)
test 532 (min_wartosc a = neg_infinity);;
test 533 (max_wartosc a = 0.);;

let d = podzielic a (wartosc_od_do (-3.) 1.);;
test 542 (min_wartosc d = neg_infinity);;
test 543 (max_wartosc d =. infinity);;

let d = podzielic (wartosc_od_do 1. 2.) a;;
test 544 (min_wartosc d = neg_infinity);;
test 545 (max_wartosc d =. 0.);;

let d = minus (wartosc_od_do 2. 8.) a;;
test 546 (max_wartosc d = infinity);;
test 547 (min_wartosc d =. 2.);;

let d = minus a (wartosc_od_do (-3.) 2.);;
test 548 (min_wartosc d = neg_infinity);;
test 549 (max_wartosc d =. 3.);;

let e = razy (wartosc_dokladna 0.) (wartosc_od_do neg_infinity infinity);;
test 550 (min_wartosc e = 0.);;
test 551 (max_wartosc e =. 0.);;



let _ = 
  if !zle = 0 then 
    Printf.printf "\nTesty OK!\n"
  else  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;
