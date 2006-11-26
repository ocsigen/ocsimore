(* A functor for "mutable sets of sets".
 *
 * It relies on Set.Make functor and recursive modules definition. No
 * checks for recursive values, cyclic memberships or other amenities
 * are provided.
 * 
 *)

module type OrdType = sig
  type t 
  val compare : t -> t -> int 
end

module Make = functor (A: OrdType) -> 
  (struct
     type 'a r = {data: A.t; mutable set: 'a}
     module rec Elt : OrdType with type t = SSet.t r = 
     struct
       type t = SSet.t r
       let compare s s' = match A.compare s.data s'.data with
	 | 0 -> SSet.compare s.set s'.set
	 | v -> v
     end and SSet : (Set.S with type elt = Elt.t) = Set.Make(Elt)
   end : sig
     type 'a r = {data: A.t; mutable set: 'a}
     module rec Elt : sig end
     and SSet : Set.S with type elt = SSet.t r
   end)
    
