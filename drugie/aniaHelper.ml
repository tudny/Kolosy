
(* Implementacja stosu *)

type 'a stos = 'a list

exception PustyStos

let pustyStos = []

let czyPustyStos st =
  st = pustyStos

let pushStos x st =
  x :: st

let popStos st =
  match st with
  | [] -> raise PustyStos
  | h :: t -> t

let topStos st =
  match st with
  | [] -> raise PustyStos
  | h :: t -> h

(* Koniec stosu *)


(* Implementacja kolejki *)

type 'a kolejka = ('a stos * 'a stos)

exception PustaKolejka

let pustaKolejka = (pustyStos, pustyStos)

let czyPustaKolejka q =
  let (s1, s2) = q in
  czyPustyStos s1 && czyPustyStos s2

let addKolejka x q =
  let (s1, s2) = q in
  (pushStos x s1, s2)

let odwroc s =
  let rec aux poprz akt =
    if czyPustyStos poprz then akt
    else aux (popStos poprz) (pushStos (topStos poprz) akt)
  in aux s pustyStos

let deleteKolejka q =
  let (s1, s2) = q in
  if czyPustyStos s2 then
    let odwrocone = odwroc s1 in
    match odwrocone with
    | [] -> raise PustaKolejka
    | s  -> (pustyStos, popStos s)
  else (s1, popStos s2)

let frontKolejka q =
  let (s1, s2) = q in
  if czyPustyStos s2 then
    let odwrocone = odwroc s2 in
    match odwrocone with
    | [] -> raise PustaKolejka
    | s  -> topStos s
  else
    topStos s2

(* Koniec psot *)





(* Implementacja kolejki by p. Kubica *)

exception EmptyQueue
(* Kolejka to trójka: przód,tył,rozmiar. *)
(* Jeżeli przód jest pusty,to kolejka jest pusta. *)
type 'a queue = { front: 'a list; back: 'a list; size: int }
(* stworzenie pustej kolejki *)
let empty = { front = []; back = []; size = 0 }
(* rozmiar *)
let size q = q.size
(* czy puste *)
let is_empty q =
  size q = 0
(*  *)
let balance q =
  match q with
  | { front = []; back = [] } -> q
  | { front = []; back = b; size = s } -> { front = List.rev b; back = []; size = s }
  | _ -> q
(* wstawianie -> q, x*)
let insert { front = f; back = b; size = n } x =
  balance { front = f; back = x :: b; size = n + 1 }
(* pierwszy element, q *)
let front q =
  match q with
  | { front = [] } -> raise EmptyQueue
  | { front = x :: _ } -> x
(* usun pierwszy element, q *)
let remove q =
  match q with
  | { front = _ :: f } -> balance { front = f; back = q.back; size = q.size - 1 }
  | _ -> raise EmptyQueue



exception Empty_Queue

type 'a pri_queue =
    | Node of 'a * 'a pri_queue * 'a pri_queue * int
    | Null

let empty_queue = Null

let is_empty q = q = Null

let size q =
    match q with
        | Null -> 0
        | Node (_, _, _, n) -> n
(* pierwszy element *)
let getmax h =
    match h with
        | Null -> raise Empty_Queue
        | Node (r, _, _, _) -> r

(* let set_root h r =
    match h with
        | Null -> Node (r, Null, Null, 1)
        | Node (_,l,p,n) ->Node (r, l, p, n) *)
(* włozenie do kolejki *)
let rec put h x =
    match h with
    | Null -> Node (x, Null, Null, 1)
    | Node (r, l, p, n) ->
        if size l <= size p then
            Node ((max x r), (put l (min x r)), p, (n+1))
        else
            Node ((max x r), l, (put p (min x r)), (n+1))
(* usuniecie z kolejki pierwszego el *)
let rec removemax h =
    match h with
        | Null ->raise Empty_Queue
        | Node (_, Null, Null, _) -> Null
        | Node (_, l, Null, _) -> l
        | Node (_, Null, p, _) -> p
        | Node (_, (Node (rl,_,_,_) as l), (Node (rp,_,_,_) as p), n) ->
            if rl >= rp then
                Node (rl, removemax l, p, n - 1)
            else
              Node (rp, l, removemax p, n - 1)

