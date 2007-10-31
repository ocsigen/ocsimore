(**
This is the forum component of Ocsimore.

@author Jaap Boender
@author Pieto Furiesi
*)

(* open Eliommod *)
open Eliomparameters
open Eliomsessions
open Eliomservices

(** This type contains information for initalization of the forum. *)
type forum_in = 
{
	identifier: string; (** The short name of the forum *)
	title: string; (** Its title *)
	descr: string; (** A somewhat longer description of the forum *)
	moderated: bool; (** Whether the forum is moderated *)
	readable_by: Users.user; (** The set of users who can read the forum *)
	writable_by: Users.user; (** The set of users who can write to the forum *)
	moderators: Users.user; (** The set of users who can moderate the forum *)
	url: string list; (** The URL of the forum *)
	max_rows: int option; (** The maximum number of messages displayed on one page (or [None] if everything should be displayed) *)
	arborescent: bool; (** Whether the forum is {i arborescent}; i.e. whether its messages are in tree or list format *)
}

class forum: db: Sql.db_t -> foruminfo: forum_in ->
object

	method db: Sql.db_t

	method get_forum_id: int

	(** [can_read] [user] is  [true] if the user can read the current forum. *)
	method can_read: Users.user session_data -> bool

	(** [can_write] [user] is  [true] if the user can write (i.e. modify) the
	current forum. *)
	method can_write: Users.user session_data -> bool

	(** [can_moderate] [user] is  [true] if the user can moderate the current forum. *)
	method can_moderate: Users.user session_data -> bool

end;;
