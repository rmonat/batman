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
open Lexing

type var = string

type aexpr = 
  | AVar of var
  | AInt of int
  | ARand of int * int
  | APlus of aexpr * aexpr
  | AMinus of aexpr * aexpr
  | ATimes of aexpr * aexpr
  | ADivided of aexpr * aexpr
  | APercent of aexpr * aexpr
  | ANeg of aexpr

type bexpr =
(*  | BVar of var*)
  | BTrue | BFalse
  | Band of bexpr * bexpr
  | Bor of bexpr * bexpr
  | BNot of bexpr
  | Bequal of aexpr * aexpr
  | Blesse of aexpr * aexpr
  | Bless  of aexpr * aexpr
  | Bgreater  of aexpr * aexpr
  | Bgreatere  of aexpr * aexpr

type exp =
  | CstA of aexpr
  | CstB of bexpr
(*  | CstRand*)

type cmd = 
  | CSkip
  | CAssign of var * exp
  | CAssume of bexpr
  | CSeq of cmd * cmd
  | CIf of bexpr * cmd * cmd
  | CWhile of bexpr * cmd

type vardecl = 
  | VarDeclI of var
  | VarDeclB of var

type thread = 
  | Thread of string * vardecl list * cmd
