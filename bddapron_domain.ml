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
open Bdddomain
open Bddapron
open Apron

module type BDDAPRON_PARAM = sig
  type lib
  val manager: lib Manager.t
end

module BDDAPRON_DOMAIN(Param: BDDAPRON_PARAM) =
  (struct

    let apron = Param.manager
    let man = Mtbdddomain1.make_man apron

    type domain = (string, Param.lib) Bddapron.Mtbdddomain1.t
    type env = string Bddapron.Env.t
    type apron_expr = string Bddapron.Expr1.Apron.t
    type bool_expr = string Bddapron.Expr1.Bool.t
    type bint_expr = string Bddapron.Expr1.Bint.t
    type expr = string Bddapron.Expr1.t
    type label = int

    let init () = 
      Cudd.Man.print_limit := 200; 
      Cudd.Man.set_gc 10000
        (begin fun () -> Format.printf "@.CUDD GC@." end)
        (begin fun () -> Format.printf "@.CUDD REORDER@." end);;

    let cudd = Cudd.Man.make_v ()
    let cond = Cond.make ~symbol:Env.string_symbol cudd
    (*let env = Env.make ~symbol:Env.string_symbol cudd*)


    let mkenv () =  Env.make ~symbol:Env.string_symbol ~relational:true cudd
    let empty () = Mtbdddomain1.top man (Env.make ~symbol:Env.string_symbol cudd)

    let top env = Mtbdddomain1.top man env
    let bot env = Mtbdddomain1.bottom man env

    let bint_cst env vmax v = Expr1.Bint.of_int env cond (`Bint (false, vmax)) v
    let bint_expr e = Expr1.Bint.to_expr e

    let apron_cst env v = Expr1.Apron.cst env cond v
    let apron_int env a b = failwith "intervals not implemented in bddapron..."
    let apron_add x y = Expr1.Apron.add cond x y
    let apron_sub x y = Expr1.Apron.sub cond x y
    let apron_mul x y = Expr1.Apron.mul cond x y
    let apron_div x y = Expr1.Apron.div cond x y
    let apron_gmod x y = Expr1.Apron.gmod cond x y
    let apron_neg x = Expr1.Apron.negate cond x
    let apron_var env x = Expr1.Apron.var env cond x
    let apron_eq x = Expr1.Apron.eq cond x
    let apron_supeq x = Expr1.Apron.supeq cond x
    let apron_sup x = Expr1.Apron.sup cond x
    let apron_print fmt x = Expr1.Apron.print cond fmt x
    let apron_expr e = Expr1.Apron.to_expr e

    let bool_var env x = Expr1.Bool.var env cond x
    let bool_and x y = Expr1.Bool.dand cond x y
    let bool_or x y = Expr1.Bool.dor cond x y
    let bool_not x  = Expr1.Bool.dnot cond x
    let bool_eq x y = Expr1.Bool.eq cond x y
    let bool_true env = Expr1.Bool.dtrue env cond
    let bool_false env = Expr1.Bool.dfalse env cond
    let bool_print fmt e = Expr1.Bool.print cond fmt e
    let bool_expr e = Expr1.Bool.to_expr e

    let expr_print fmt e = Expr1.print cond fmt e
    let expr_var env v = Expr1.var env cond v
    let env_print fmt e = Env.print fmt e


    let eq a b = Mtbdddomain1.is_eq man a b

    let subseteq a b = Mtbdddomain1.is_leq man a b

    let print fmt a = Mtbdddomain1.print fmt a

    let getenv dom = Mtbdddomain1.get_env dom

    let getcudd () = cudd


    let join a b = Mtbdddomain1.join man a b

    let meet a b = Mtbdddomain1.meet man a b
        
    let widen a b = Mtbdddomain1.widening man a b

    let widen_threshold a b threshold = Mtbdddomain1.widening_threshold man a b threshold

    let assign dom var a = Mtbdddomain1.assign_lexpr ~relational:true man cond dom [var] [a] None, ()

    let to_lincons_array dom = 
      let domains = List.map (fun (x, y) -> y) (Mtbdddomain1.to_bddapron man dom) in
      let d = List.fold_left (fun x y -> Apron.Abstract1.join apron x y) (Apron.Abstract1.bottom apron (Env.apron(Mtbdddomain1.get_env dom))) domains in
      Apron.Abstract1.to_lincons_array apron d

    let get_apron_env dom = 
      let env = getenv dom in
      Env.apron env
    
    let filter dom b = 
      Mtbdddomain1.meet_condition man cond dom b, Mtbdddomain1.meet_condition man cond dom (Expr1.Bool.dnot cond b), ()
    

    let add_vars ?(init=true) (*?(init=true) on suppose que y'a jamais besoin d'initialiser*) dom vars = 
      let env = Mtbdddomain1.get_env dom in
      let env = Env.add_vars env vars in
      let dom = Mtbdddomain1.change_environment man dom env in
      if (init) then
        (
          let rec do_init dom remaining_vars = match remaining_vars with
            | [] -> dom
            | t::q -> (match (snd t) with
                |`Bool -> do_init (fst (assign dom (fst t) (Expr1.Bool.to_expr (bool_false env)))) q
                |`Int -> do_init (fst (assign dom (fst t) (Expr1.Apron.to_expr (apron_cst env (Apron.Coeff.s_of_int 0))))) q
                | _ -> do_init dom q
              )
          in do_init dom vars
        )
      else
        dom
          

    let del_vars (dom:domain) vars =
      let env = Mtbdddomain1.get_env dom in
      (*Bddapron.Env.print Format.std_formatter env;*)
      let env = Env.remove_vars env (List.map (fun (x, y) -> x) vars) in
      Mtbdddomain1.change_environment man dom env

    let ren_vars (dom:domain) begv endv = 
(*      let env = Mtbdddomain1.get_env domain in
      let env = Env.rename_vars env (List.map2 (fun x y -> (x, y)) begv endv) in
      Mbtdddomain1.change_environment man domain env*)
      Mtbdddomain1.rename man dom (List.map2 (fun x y -> (fst x, fst y)) begv endv)

  end:BDD_ABSTRACT_DOMAIN)


module PolkaDomain = 
  BDDAPRON_DOMAIN
    (struct
      type lib = Polka.loose Polka.t
      let manager = Polka.manager_alloc_loose ()
    end)


module PplDomain = 
  BDDAPRON_DOMAIN
    (struct
      type lib = Ppl.loose Ppl.t
      let manager = Ppl.manager_alloc_loose ()
    end)

module IntervalDomain = 
  BDDAPRON_DOMAIN
    (struct
      type lib = Box.t
      let manager = Box.manager_alloc ()
    end)

module OctagonDomain = 
  BDDAPRON_DOMAIN
    (struct
      type lib = Oct.t
      let manager = Oct.manager_alloc ()
    end)
