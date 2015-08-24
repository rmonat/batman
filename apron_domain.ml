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

(* Translation into apron of the domain.ml functions*)
open Apron
open Abs
open Domain

module type APRON_PARAM = sig
    type lib
    val manager: lib Manager.t
  end

module ApronDomain(Param: APRON_PARAM) = 
  (struct

    let man = Param.manager
    type t = Param.lib Abstract1.t

    let apron_of_var v = 
      Var.of_string v

    let empty () =
      Abstract1.top man (Environment.make [||] [||])
	
    let hash a = 
      Abstract1.hash man a
	    
    let bot a =
      Abstract1.bottom man (Abstract1.env a)
		       
    let top a =
      Abstract1.top man (Abstract1.env a)
	    
    let is_bot a =
      Abstract1.is_bottom man a
			  
    let subseteq a b =
      Abstract1.is_leq man a b

    let eq a b =
      Abstract1.is_eq man a b

    let hash a =
      Abstract1.hash man a
      
    let print fmt a =
      Abstract1.print fmt a

    let getdim domain =
      let env = (Environment.dimension (Abstract1.env domain)) in
      env.intd

    let getenv domain = Abstract1.env domain

    let var_of_dim domain dim =
      Environment.var_of_dim (Abstract1.env domain) dim

    let add_vars ?(init=true) domain vars =
      let env = Environment.add (Abstract1.env domain) vars [||] in
      Abstract1.change_environment man domain env init

    let del_vars domain vars = 
      let env = Environment.remove (Abstract1.env domain) vars in
      Abstract1.change_environment man domain env true

    let ren_vars domain begv endv =
(*      let env = Environment.rename (Abstract1.env domain) begv endv in
      Abstract1.change_environment man domain env true*)
      Abstract1.rename_array man domain begv endv

    let add_var domain var = 
      (* on ne travaille qu'avec des entiers*)
      let env = Environment.add (Abstract1.env domain) [|apron_of_var var|] [||] in
      Abstract1.change_environment man domain env true
      (* il faut bien mettre true dans change_environment ?*)
      
    let del_var domain var =
      let env = Environment.remove (Abstract1.env domain) [|apron_of_var var|] in
      Abstract1.change_environment man domain env false
      (* et là par contre il faut bien mettre un false ?*)

    let join a b = Abstract1.join man a b
        
    let join_with a b = Abstract1.join_with man a b

    let copy a = Abstract1.copy man a

    let meet a b = Abstract1.meet man a b

    let meet_with a b = Abstract1.meet_with man a b
				  
    let widen a b = Abstract1.widening man a b

    let widen_threshold a b threshold = Abstract1.widening_threshold man a b threshold

    let texpr_of_aexpr env arexp = 
      let rec firstconv = function
        | ARand (x, y) -> Texpr1.Cst (Coeff.i_of_int x y)
        | AVar v -> Texpr1.Var (apron_of_var v)
        | AInt i -> Texpr1.Cst (Coeff.s_of_int i)
        | APlus (x, y) -> Texpr1.Binop (Texpr1.Add, firstconv x, firstconv y, Texpr1.Int, Texpr1.Near)
        | AMinus (x, y) -> Texpr1.Binop (Texpr1.Sub, firstconv x, firstconv y, Texpr1.Int, Texpr1.Near)
        | ATimes (x, y) -> Texpr1.Binop (Texpr1.Mul, firstconv x, firstconv y, Texpr1.Int, Texpr1.Near)
        | ADivided (x, y) -> Texpr1.Binop (Texpr1.Div, firstconv x, firstconv y, Texpr1.Int, Texpr1.Near)
        | APercent (x, y) -> Texpr1.Binop (Texpr1.Mod, firstconv x, firstconv y, Texpr1.Int, Texpr1.Near)
        | ANeg x -> Texpr1.Unop (Texpr1.Neg, firstconv x, Texpr1.Int, Texpr1.Near)
      in Texpr1.of_expr env (firstconv arexp)

    let assign domain var arexpr = (Abstract1.assign_texpr man domain (apron_of_var var) (texpr_of_aexpr (Abstract1.env domain) arexpr) None), ()

    let assign_with domain var arexpr = (Abstract1.assign_texpr_with man domain (apron_of_var var) (texpr_of_aexpr (Abstract1.env domain) arexpr) None)
    

    let to_lincons_array domain = Abstract1.to_lincons_array man domain

    (* TODO *)
    (** 
       @return la contrainte exprimée par boexpr suivie de sa négation, sous forme de Tcons1
    *)
    let rec tcons_of_bexpr env boexpr = 
      match boexpr with
      | Bequal (x, y) -> Tcons1.make (texpr_of_aexpr env (AMinus (x, y))) EQ, Tcons1.make (texpr_of_aexpr env (AMinus (x, y))) DISEQ
      | Blesse (x, y) -> Tcons1.make (texpr_of_aexpr env (AMinus (y, x))) SUPEQ (* x <= y <==> y SUPEQ x*), Tcons1.make (texpr_of_aexpr env (AMinus (x, APlus(y, AInt 1)))) SUPEQ (* x >=  y+1 *)
      | Bless (x, y) ->  Tcons1.make (texpr_of_aexpr env (AMinus (y, (APlus (x, AInt 1))))) SUPEQ (* x < y <==> y SUP x*), Tcons1.make (texpr_of_aexpr env (AMinus (x, y))) SUPEQ
      | Bgreater (x, y) -> let (t1, t2) = tcons_of_bexpr env (Blesse (x, y)) in (t2, t1) (*x > y <=> not (x <= y)*)
      | Bgreatere (x, y) -> let (t1, t2) = tcons_of_bexpr env (Bless (x, y)) in (t2, t1)
      | _ -> failwith "not implemented"

      
    let rec filter domain boexpr = 
      let env = Abstract1.env domain in
      match boexpr with
      | BTrue -> domain, bot domain, ()
      | BFalse -> bot domain, domain, ()
      | Band (e1, e2) -> 
        let a1, a2, _ = filter domain e1 in
        let a11, a12, _ = filter a1 e2 in
        a11, join a2 a12, ()
      | Bor (e1, e2) ->
        let a1, a2, _ = filter domain e1 in
        let a21, a22, _ = filter a2 e2 in
        join a1 a21, a22, ()
      | BNot e -> let (t, f, _) = filter domain e in f, t, ()
      | _ ->
        let cons1, cons2 = tcons_of_bexpr env boexpr in
        let a1, a2 = Tcons1.array_make env 1, Tcons1.array_make env 1 in
        Tcons1.array_set a1 0 cons1;
        Tcons1.array_set a2 0 cons2;
        Abstract1.meet_tcons_array man domain a1, Abstract1.meet_tcons_array man domain a2, ()
        (* on veut pouvoir renvoyer deux environnements issus des paramètres donnés, mais qui suivent ou non la condition booléenne donnée aussi en argument. Pour cela, on cherche à transformer la condition booléenne en une contrainte de type Tcons1 ou Lincons1*)
      

  end: ABSTRACT_DOMAIN)

module PolkaDomain =
  ApronDomain
    (struct
      type lib = Polka.loose Polka.t
      let manager = Polka.manager_alloc_loose ()
    end)

module PplDomain =
  ApronDomain
    (struct
      type lib = Ppl.loose Ppl.t
      let manager = Ppl.manager_alloc_loose ()
    end)


module IntervalDomain =
  ApronDomain
    (struct
      type lib = Box.t
      let manager = Box.manager_alloc ()
    end)


module OctagonDomain =
  ApronDomain
    (struct
      type lib = Oct.t
      let manager = Oct.manager_alloc ()
    end)
