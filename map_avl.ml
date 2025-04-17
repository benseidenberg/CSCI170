(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

  module type S =
  sig
    type key
    type !+'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    
    (* many more function headers*)
end

module Make(Ord: OrderedType) = struct

  type key = Ord.t

  type 'a t =
      Empty
    | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}

  let height = function
      Empty -> 0
    | Node {h} -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  let singleton x d = Node{l=Empty; v=x; d; r=Empty; h=1}

  let bal l x d r =
    let hl = match l with Empty -> 0 | Node {h} -> h in
    let hr = match r with Empty -> 0 | Node {h} -> h in
    if hl > hr + 1 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node{l=ll; v=lv; d=ld; r=lr} ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> invalid_arg "Map.bal"
            | Node{l=lrl; v=lrv; d=lrd; r=lrr}->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 1 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node{l=rl; v=rv; d=rd; r=rr} ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> invalid_arg "Map.bal"
            | Node{l=rll; v=rlv; d=rld; r=rlr} ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec add x data = function
      Empty ->
        Node{l=Empty; v=x; d=data; r=Empty; h=1}
    | Node {l; v; d; r; h} as m ->
        let c = Ord.compare x v in
        if c = 0 then
          if d == data then m else Node{l; v=x; d=data; r; h}
        else if c < 0 then
          let ll = add x data l in
          if l == ll then m else bal ll v d r
        else
          let rr = add x data r in
          if r == rr then m else bal l v d rr
end