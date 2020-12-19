
(* Implementacja stosu za pomocą list funkcyjnych *)

(* BEGIN of stack *)

type 'a stack = 'a list

exception EmptyStack

let emptyStack = []

let isEmptyStack st =
  st = emptyStack

let pushStack x st =
  x :: st

let popStack st =
  match st with
  | [] -> raise EmptyStack
  | h :: t -> t

let topStack st =
match st with
| [] -> raise EmptyStack
| h :: t -> h

(* END of stack *)


(* Implementacja kolejki za pomocą stosu funkcyjnego *)

(* BEGIN of queue *)

type 'a queue = 'a stack * 'a stack

exception EmptyQueue

let emptyQueue =
  (emptyStack, emptyStack)

let isEmptyQueue (s1, s2) =
  isEmptyStack s1 && isEmptyStack s2

let addQueue x (s1, s2) =
  (pushStack x s1, s2)

let rev s =
  let rec loop prev curr =
    if isEmptyStack prev then curr
    else loop (popStack prev) (pushStack (topStack prev) curr)
  in loop s emptyStack

let dequeueQueue (s1, s2) =
  if isEmptyStack s2 then
    try
      (emptyStack, popStack (rev s1))
    with
      EmptyStack -> raise EmptyQueue
  else
    (s1, popStack s2)

let frontQueue (s1, s2) =
  if isEmptyStack s2 then
    try
      topStack (rev s1)
    with
    | EmptyStack -> raise EmptyQueue
  else
    topStack s2

(* END of queue *)

