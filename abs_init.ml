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

type ilabel = bool

type iexpr = 
  | IAVar of var
  | IAInt of int
  | IARand of int * int
  | IAPlus of iexpr * iexpr
  | IAMinus of iexpr * iexpr
  | IATimes of iexpr * iexpr
  | IADivided of iexpr * iexpr
  | IAPercent of iexpr * iexpr
  | IANeg of iexpr
  | IBTrue | IBFalse
  | IBand of iexpr * iexpr
  | IBor of iexpr * iexpr
  | IBNot of iexpr
  | IBequal of iexpr * iexpr
  | IBlesse of iexpr * iexpr
  | IBless  of iexpr * iexpr
  | IBgreater  of iexpr * iexpr
  | IBgreatere  of iexpr * iexpr

type icmd = 
  | ICSkip
  | ICAssign of ilabel * var * iexpr
  | ICAssume of ilabel * iexpr
  | ICSeq of icmd * icmd
  | ICIf of ilabel * iexpr * icmd * icmd
  | ICWhile of ilabel * iexpr * icmd

type ivardecl = 
  | IVarDeclI of var
  | IVarDeclB of var

type ithread = 
  | IThread of string * ivardecl list * icmd
