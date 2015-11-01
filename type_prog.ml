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

open Abs
open Abs_init
open Sys
open Bddapron
open Bdddomain

module TypeProg(D:BDD_ABSTRACT_DOMAIN) =
  struct
    module A = Abs(D)

    let check_vardecl vdecl =
      let rec process_list intl booll l = match l with
        |[] -> (intl, booll)
        |t::q -> (match t with 
            |IVarDeclI v -> process_list (v::intl) (booll) q
            |IVarDeclB v -> process_list (intl) (v::booll) q)
      in 
      let i, b = process_list [] [] vdecl in
      (* true : pas de conflit, false : conflit*)
      let rec check_conflict i b = match i with
        |[] -> true
        |t::q -> 
          if (List.mem t b) then
            (
              Format.printf "Error %s is declared as a boolean and integer !" t;
              exit 1
            )
          else
            check_conflict q b
      in 
      if (check_conflict i b) then i, b
      else [], []


    let rec extract_aexpr env i b a = match a with
      | IAVar v -> 
        if (List.mem v i) then D.apron_var env v 
        else failwith ("in extract_aexpr env bad type of " ^ v)
      | IAInt i -> D.apron_cst env (Apron.Coeff.s_of_int i)
      | IARand (x, y) -> D.apron_int env x y
      | IAPlus (x, y) -> D.apron_add (extract_aexpr env i b x) (extract_aexpr env i b y)
      | IAMinus (x, y) -> D.apron_sub (extract_aexpr env i b x) (extract_aexpr env i b y)
      | IATimes (x, y) -> D.apron_mul (extract_aexpr env i b x) (extract_aexpr env i b y)
      | IADivided (x, y) -> D.apron_div (extract_aexpr env i b x) (extract_aexpr env i b y)
      | IAPercent (x, y) -> D.apron_gmod (extract_aexpr env i b x) (extract_aexpr env i b y)
      | IANeg x -> D.apron_neg (extract_aexpr env i b  x)
      | _ -> failwith "not purely arithmetical expression [extract_aexpr]"

    let rec extract_bexpr env i b bex = match bex with
      | IAVar v -> 
        if (List.mem v b) then D.bool_var env v
        else failwith ("in extract_bexpr bad type of " ^ v)
      | IBand (x, y) -> D.bool_and (extract_bexpr env i b x) (extract_bexpr env i b y)
      | IBor (x, y) -> D.bool_or (extract_bexpr env i b x) (extract_bexpr env i b y)
      | IBNot x -> D.bool_not (extract_bexpr env i b x)
      | IBequal (x, y) -> 
        (try D.apron_eq (extract_aexpr env i b (IAMinus (x, y))) 
        with _ -> D.bool_eq (extract_bexpr env i b x) (extract_bexpr env i b y))

      | IBlesse (x, y) -> D.apron_supeq (extract_aexpr env i b (IAMinus(y, x))) (*x <= y <==> y-x >= 0*)
      | IBless (x, y) -> D.apron_sup (extract_aexpr env i b (IAMinus(y, x)))
      | IBgreater (x, y) -> D.apron_sup  (extract_aexpr env i b (IAMinus(y, x)))
      | IBgreatere (x, y) -> D.apron_supeq  (extract_aexpr env i b (IAMinus(y, x)))
      | IBTrue -> D.bool_true env
      | IBFalse -> D.bool_false env
      | _ -> failwith "not purely boolean expression [extract_bexpr env]"

    let label_count = ref 0;;

    let rec extract_cmd env i b c = match c with
      | ICSkip -> A.CSkip

      | ICAssign (bl, v, e) -> 
        let nextlabel = if bl then (incr label_count; (!label_count)) else !label_count in
        if (List.mem v i) then
          A.CAssign  (nextlabel, nextlabel, v, (D.apron_expr (extract_aexpr env i b e)))
        else
          A.CAssign (nextlabel, nextlabel, v, (D.bool_expr (extract_bexpr env i b e)))

      | ICAssume (bl, iexp) -> 
        let nextlabel = if bl then (incr label_count; (!label_count)) else !label_count in
        A.CAssume (nextlabel, nextlabel, (extract_bexpr env i b iexp))

      | ICSeq (c1, c2) -> 
        let r1 = (extract_cmd env i b c1) in A.CSeq (r1, (extract_cmd env i b c2))

      | ICIf (bl, iexp, t, f) -> 
        let lab = if bl then (incr label_count; (!label_count)) else !label_count in
        let rt = (extract_cmd env i b t) in
        let nextlabel = lab in
        label_count := lab;
        let rf = (extract_cmd env i b f) in
        let nextlabel = max nextlabel (!label_count) in
        A.CIf (lab, nextlabel, (extract_bexpr env i b iexp), rt, rf)

      | ICWhile (bl, iexp, c) ->
        let lab = if bl then (incr label_count; (!label_count)) else !label_count in
        let r = extract_cmd env i b c in
        A.CWhile (lab, !label_count, (extract_bexpr env i b iexp), r)

    let transform_varlist i b = 
      (List.map (fun x -> (x, `Int)) i)@(List.map (fun x -> (x, `Bool)) b)

    let lmax x = if(x <= 1) then 1 else (int_of_float) (ceil ((log (float_of_int (x+1))) /. log 2.));;


    let extract_prog l = 
      let (ithreads, ivars, iinit) = l in
      let i, b = check_vardecl ivars in
      let glob_vars = transform_varlist i b in

      let env = D.mkenv () in
      D.init ();
      let top = D.top env in 
      let top = D.add_vars top glob_vars in
      let env = D.getenv top in


      let init = extract_bexpr env i b iinit in
      let rec extract_thread = function
        |[] -> []
        |t::q -> 
          let IThread (s, v, c) = t in
          let il, bl = check_vardecl v in
          label_count := 0;
          let c = extract_cmd env i b c in
          let lab = lmax (!label_count) in
          ((A.Thread (s, (transform_varlist il bl), lab, c))::(extract_thread q))
      in
      let threads = (extract_thread ithreads) in
      let label_vars = List.map (fun x -> let A.Thread(i,_,l,_) = x in ("_aux_"^i, `Bint(false, l))) threads in
      let top = D.add_vars top label_vars in
      let env = D.getenv top in
      (threads, (glob_vars@label_vars), init), env
  end
