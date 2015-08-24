(* This file is part of the Batman analyzer, released under GPLv3          *)
(* license. Please read the COPYING file packaged in the distribution.     *)
(*                                                                         *)
(* The Batman analyzer is free software: you can redistribute it and/or    *)
(* modify it under the terms of the GNU General Public License as          *)
(* published by the Free Software Foundation, either version 3 of the      *)
(* License, or (at your option) any later version.                         *)
(*                                                                         *)
(* The Batman analyzer is distributed in the hope that it will be useful,  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *)
(* General Public License for more details.                                *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this Batman analyzer.  If not, see                           *)
(* <http://www.gnu.org/licenses/>                                          *)
(*                                                                         *)
(* Copyright (C) Raphaël Monat 2015.                                       *)

(* This is a global domain signature, only used with apron_domain
   then. It is used in BATMAN to handle numerical domains. Errors are
   not handled. *)
open Apron
open Abs

type error = unit

module type ABSTRACT_DOMAIN = sig
    type t

    val apron_of_var : string -> Apron.Var.t

    (* creates empty domain *)
    val empty: unit -> t
    (* creates a bottom domain, using (the environment of) the domain provided *)
    val bot : t -> t

    (* creates a top domain, using (the environment of) the domain provided *)
    val top : t -> t
		    
    val is_bot : t -> bool

    (* checks if a domain is included in or equal to another *)
    val subseteq : t -> t -> bool

    (* checks if a domain equals to another*)
    val eq : t -> t -> bool

    (* hash of domain *)
    val hash : t -> int
    
    val print : Format.formatter -> t -> unit

    (* returns the dimension (the number of variables) of a domain *)
    val getdim : t -> int

    (* returns the environment of the domain *)
    val getenv : t -> Apron.Environment.t

    (* given a domain and a dimension i, this function returns the
       variable corresponding to the ith dimension *)
    val var_of_dim : t -> int -> Apron.Var.t

    val add_vars : ?init:bool -> t -> Apron.Var.t array -> t
    val del_vars : t -> Apron.Var.t array -> t
    val ren_vars : t -> Apron.Var.t array -> Apron.Var.t array -> t
    val add_var : t -> var -> t
    val del_var : t -> var -> t

    val copy : t -> t
    val join : t -> t -> t
    val join_with : t -> t -> unit
    val widen : t -> t -> t
    val widen_threshold : t -> t -> Apron.Lincons1.earray -> t

    val meet : t -> t -> t
    val meet_with : t -> t -> unit

    val assign : t -> var -> aexpr -> t * error
    val assign_with : t -> var -> aexpr -> unit
      
    (* Given an environment and a boolean expression, this function
       returns a set of constraints equivalent to the boolean expression,
       and its negation*)
    val tcons_of_bexpr : Apron.Environment.t -> bexpr -> Apron.Tcons1.t * Apron.Tcons1.t
					    
    val filter : t -> bexpr -> t * t * error

    (* Transforms a domain into an array of linear constraints *)
    val to_lincons_array : t -> Apron.Lincons1.earray
			       
end


let no_err = ()
let join_err e1 e2 = ()
