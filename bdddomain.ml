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
open Apron
open Bddapron

type error = unit

module type BDD_ABSTRACT_DOMAIN = sig
  type domain (* = ('a, Param.lib) Bddapron.Mtbdddomain1.t *)
  type env (* = 'a Bddapron.Env.t (ou string ?)*)
  type apron_expr (* string Bddapron.Expr1.Apron.t*)
  type bool_expr (* string bddapon.Expr1.Bool.t*)
  type bint_expr
  type expr
  type label (* Bddapron.Expr1.Bint.t || int ?*)

(*  val apron : Param.lib Apron.Manager.t*)
(*  val man : ('a, Param.lib) Bddapron.Mtbdddomain0.man*)
(*  val init : unit -> Cudd.Man.v Cudd.Man.t*)
  (* val cudd : Cudd.Man.v Cudd.Man.t *)
  (* val cond : string Bddapron.Cond.t *)
  val init : unit -> unit 
  val mkenv : unit -> env 
  val empty : unit -> domain
  val top : env -> domain

  val bint_cst : env -> int -> int -> bint_expr
  val bint_expr : bint_expr -> expr

  val apron_cst : env -> Apron.Coeff.t -> apron_expr
  val apron_int : env -> int -> int -> apron_expr
  val apron_add : apron_expr -> apron_expr -> apron_expr
  val apron_sub : apron_expr -> apron_expr -> apron_expr
  val apron_mul : apron_expr -> apron_expr -> apron_expr
  val apron_div : apron_expr -> apron_expr -> apron_expr
  val apron_gmod : apron_expr -> apron_expr -> apron_expr
  val apron_neg : apron_expr -> apron_expr
  val apron_var : env -> string -> apron_expr
  val apron_eq : apron_expr -> bool_expr
  val apron_supeq : apron_expr -> bool_expr
  val apron_sup : apron_expr -> bool_expr
  val apron_print : Format.formatter -> apron_expr -> unit
  val apron_expr : apron_expr -> expr

  val bool_var : env -> string -> bool_expr
  val bool_and : bool_expr -> bool_expr -> bool_expr
  val bool_or : bool_expr -> bool_expr -> bool_expr
  val bool_not : bool_expr -> bool_expr
  val bool_eq : bool_expr -> bool_expr -> bool_expr
  val bool_true : env -> bool_expr
  val bool_false : env -> bool_expr
  val bool_print : Format.formatter -> bool_expr -> unit 
  val bool_expr : bool_expr -> expr

  val expr_print : Format.formatter -> expr -> unit
  val expr_var : env -> string -> expr
  val env_print : Format.formatter -> env -> unit

  val bot : env -> domain
  val eq : domain -> domain -> bool
  val subseteq : domain -> domain -> bool
  val print : Format.formatter -> domain -> unit
  val getenv : domain -> env 
  val getcudd : unit -> Cudd.Man.v Cudd.Man.t
  val join : domain -> domain -> domain
  val meet : domain -> domain -> domain
  val widen : domain -> domain -> domain
  val widen_threshold : domain -> domain -> Apron.Lincons1.earray -> domain
  val assign : domain -> string -> expr -> domain * error
  val to_lincons_array : domain -> Apron.Lincons1.earray
  val get_apron_env : domain -> Apron.Environment.t
  val filter : domain -> bool_expr -> domain * domain * error

  val add_vars : ?init:bool -> domain -> (string * string Bddapron.Env.typ) list -> domain
  val del_vars : domain -> (string * 'a) list -> domain
  val ren_vars : domain -> (string * 'b) list -> (string * 'c) list -> domain
end


let no_err = ()
let join_err e1 e2 = ()
