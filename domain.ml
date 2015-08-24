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
(* Copyright (C) Raphaël Monat 2015.                                       *)(* open Apron *)
(* open Abs *)

(* type error = unit *)

(* module type ABSTRACT_DOMAIN = sig *)
(*     type t *)
(*     type var = string *)

(*     val apron_of_var : string -> Apron.Var.t *)

(*     val empty: unit -> t *)
(*     val bot : t -> t *)
(*     val top : t -> t *)
		     
(*     val is_bot : t -> bool *)
(*     val subseteq : t -> t -> bool *)
(*     val eq : t -> t -> bool *)
(*     val hash : t -> int *)
    
(*     val print : Format.formatter -> t -> unit *)
(*     val getdim : t -> int *)
(*     val getenv : t -> Apron.Environment.t *)
(*     val var_of_dim : t -> int -> Apron.Var.t *)

(*     val add_vars : ?init:bool -> t -> Apron.Var.t array -> t *)
(*     val del_vars : t -> Apron.Var.t array -> t *)
(*     val ren_vars : t -> Apron.Var.t array -> Apron.Var.t array -> t *)
(*     val add_var : t -> var -> t *)
(*     val del_var : t -> var -> t *)

(*     val copy : t -> t *)
(*     val join : t -> t -> t *)
(*     val join_with : t -> t -> unit *)
(*     val widen : t -> t -> t *)
(*     val widen_threshold : t -> t -> Apron.Lincons1.earray -> t *)

(*     val meet : t -> t -> t *)
(*     val meet_with : t -> t -> unit *)

(*     val assign : t -> var -> aexpr -> t * error *)
(*     val assign_with : t -> var -> aexpr -> unit *)
(*     val tcons_of_bexpr : Apron.Environment.t -> bexpr -> Apron.Tcons1.t * Apron.Tcons1.t *)
					    
(*     val filter : t -> bexpr -> t * t * error *)

(*     val to_lincons_array : t -> Apron.Lincons1.earray *)
			       
(* end *)


(* let no_err = () *)
(* let join_err e1 e2 = () *)
