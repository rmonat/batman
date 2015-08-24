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
(* Copyright (C) Raphaël Monat 2015.                                       *)open Abs

let rec sprint_aexpr = function
  | AVar v -> v
  | ARand (x, y) -> "[" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ "]"
  | AInt i -> (string_of_int i)
  | APlus (x, y) -> "(" ^ (sprint_aexpr x) ^ " + " ^ (sprint_aexpr y) ^ ")"
  | AMinus (x, y) -> "(" ^ (sprint_aexpr x) ^ " - " ^ (sprint_aexpr y) ^ ")"
  | ATimes (x, y) -> "(" ^ (sprint_aexpr x) ^ " * " ^ (sprint_aexpr y) ^ ")"
  | ADivided (x, y) -> "(" ^ (sprint_aexpr x) ^ " / " ^ (sprint_aexpr y) ^ ")"
  | APercent (x, y) -> "(" ^ (sprint_aexpr x) ^ " % " ^ (sprint_aexpr y) ^ ")"
  | ANeg x -> "-" ^ (sprint_aexpr x)
and sprint_bexpr = function
  | BTrue -> "true"
  | BFalse -> "false"
  | Band (x, y) -> "(" ^ (sprint_bexpr x) ^ " and " ^ (sprint_bexpr y) ^ ")"
  | Bor (x, y) -> "(" ^ (sprint_bexpr x) ^ " or " ^ (sprint_bexpr y) ^ ")"
  | BNot x -> "not" ^ (sprint_bexpr x)
  | Bequal (x, y) -> (sprint_aexpr x) ^ " == " ^ (sprint_aexpr y)
  | Bless (x, y) -> (sprint_aexpr x) ^ " < " ^ (sprint_aexpr y)
  | Blesse (x, y) -> (sprint_aexpr x) ^ " <= " ^ (sprint_aexpr y)
  | Bgreater (x, y) -> (sprint_aexpr x) ^ " > " ^ (sprint_aexpr y)
  | Bgreatere (x, y) -> (sprint_aexpr x) ^ " >= " ^ (sprint_aexpr y)
and sprint_exp = function
  | CstA a -> sprint_aexpr a
  | CstB b -> sprint_bexpr b
(*  | CstRand -> "rand"*)
