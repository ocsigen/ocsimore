(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**
Cache.

@author Vincent Balat
*)

let (>>=) = Lwt.bind

module Dlist = (struct

  type 'a node =
      { mutable value : 'a;
        mutable succ : 'a node option;
        mutable prev : 'a node option}

  type 'a t = 
      {list : 'a node option (* None = empty *);
       first : 'a node option;
       size : int;
       maxsize : int}

  let create_one a = let rec v = { value = a; succ = None; prev = None} in v ;;

  let create size = {list = None; first = None; size = 0; maxsize = size};;

  let add x r =
    match r.list with
      | None -> 
          let n = create_one x in
          {list = Some n; 
           first = Some n;
           size = 1;
           maxsize = r.maxsize}
      | Some rl ->
          let n = { value = x ; prev = r.list; succ = None } in
          rl.succ <- Some n;
          if r.size >= r.maxsize
          then
            (match r.first with
               | None -> assert false
               | Some a -> 
                   (match a.succ with
                     | None -> assert false
                     | Some b -> b.prev <- None);
                   {list = Some n;
                    first = a.succ;
                    size = r.size;
                    maxsize = r.maxsize})
          else
            {list = Some n;
             first = r.first;
             size = r.size + 1;
             maxsize = r.maxsize}

  let add_ node r =
    match r.list with
      | None -> 
          node.succ <- None;
          node.prev <- None;
          {list = Some node; 
           first = Some node;
           size = 1;
           maxsize = r.maxsize}
      | Some rl ->
          node.succ <- None;
          node.prev <- r.list;
          rl.succ <- Some node;
          if r.size >= r.maxsize
          then
            (match r.first with
               | None -> assert false
               | Some a -> 
                   (match a.succ with
                     | None -> assert false
                     | Some b -> b.prev <- None);
                   {list = Some node;
                    first = a.succ;
                    size = r.size;
                    maxsize = r.maxsize})
          else
            {list = Some node;
             first = r.first;
             size = r.size + 1;
             maxsize = r.maxsize}

  let remove l node =
    let first = 
      match l.first with
        | Some n when node == n -> node.succ
        | _ -> l.first
    in
    let last = 
      match l.list with
        | Some n when node == n -> node.prev
        | _ -> l.list
    in
    (match node.succ with
       | None -> ()
       | Some s -> s.prev <- node.prev);
    (match node.prev with
       | None -> ()
       | Some s -> s.succ <- node.succ);
    {list = last;
     first = first;
     size = l.size - 1;
     maxsize = l.maxsize}

  let last a = a.list

  let first a = a.first

  let maxsize c = c.size

  let length c =
    let rec aux i = function
      | Some {succ=p} -> aux (i + 1) p
      | None -> i
    in aux 0 c.first

  let value n = n.value

  let up l node =
    match l.list with
      | Some n when node == n -> l
      | _ -> 
          let l2 = remove l node in
          add_ node l2
          (* we must not change the physical address => use add_ *)

end : sig
  type 'a t
  type 'a node
  val create : int -> 'a t
  val add : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a node option
  val first : 'a t -> 'a node option
  val remove : 'a t -> 'a node -> 'a t
  val up : 'a t -> 'a node -> 'a t
  val maxsize : 'a t -> int
  val length : 'a t -> int
  val value : 'a node -> 'a
end)


module Make = 
  functor (A: sig
             type key
             type value
           end) ->
struct

  type data = A.key

  module H = Hashtbl.Make(
    struct
      type t = A.key
      let equal a a' = a = a'
      let hash = Hashtbl.hash
    end)

  type t =
      { mutable pointers : A.key Dlist.t;
        mutable table : (A.value * A.key Dlist.node) H.t;
        finder : A.key -> A.value Lwt.t
      }

  let create f size =
    {pointers = Dlist.create size;
     table = H.create size;
     finder = f
    }

  let clear cache =
    let size = Dlist.maxsize cache.pointers in
    cache.pointers <- Dlist.create size;
    cache.table <- H.create size

  let poke r node =
    r.pointers <- Dlist.up r.pointers node

  let find_in_cache cache k =
    let (v, node) = H.find cache.table k in
    poke cache node;
    v

  let remove cache k =
    try
      let (v, node) = H.find cache.table k in
      H.remove cache.table k;
      cache.pointers <- Dlist.remove cache.pointers node
    with Not_found -> ()

  let add cache k v =
    remove cache k;
    let first = Dlist.first cache.pointers in
    let l = Dlist.add k cache.pointers in
    cache.pointers <- l;
    let first' = Dlist.first cache.pointers in
    if first != first'
    then (match first with
            | None -> ()
            | Some v -> H.remove cache.table (Dlist.value v));
    match Dlist.last cache.pointers with
      | None -> assert false
      | Some n -> H.add cache.table k (v, n)

  let size c =
    Dlist.length c.pointers

  let find cache k =
    (try
       Lwt.return (find_in_cache cache k)
     with Not_found ->
       (* DEBUG print_endline "               cache: db access"; *)
       cache.finder k >>= fun r ->
       add cache k r;
       Lwt.return r)

end

