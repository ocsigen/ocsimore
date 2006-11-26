(** Mutable sets of sets. 

    This module implements the "set of sets" data structure, given a
    total ordering function over the elements. It relies on Set.Make
    functor and recursive modules definition. No checks for recursive
    values, cyclic memberships or other amenities are provided. *)



(** Input signature of the functor {!SetOfSets.Make} *)
module type OrdType = 
sig 
  type t     (** Type of elements *)
  val compare : t -> t -> int  (** Total ordering function over elements. *)
    (** [compare s s'] is zero iif [s] and [s'] are equal; is negative iif
	[s] is less than [s']; is positive
	iif [s] is greater than [s']. *)
end

(** Functor for making sets of sets over an ordered type. *)
module Make :
  functor (A : OrdType) ->
    sig
      type 'a r = { data : A.t; mutable set : 'a; }
      module rec Elt : sig  end
      and SSet :
        sig
          type elt = SSet.t r
          type t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val max_elt : t -> elt
          val choose : t -> elt
          val split : elt -> t -> t * bool * t
        end
    end
