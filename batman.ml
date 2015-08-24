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
open Analyzer
open Lexing
open Type_prog
open Bddapron_domain

module PolyAnalyzer = Analyzer.Iterator(Bddapron_domain.PolkaDomain)
module OctAnalyzer = Analyzer.Iterator(Bddapron_domain.OctagonDomain)
module PplAnalyzer = Analyzer.Iterator(Bddapron_domain.PplDomain)

module Typrog = TypeProg(Bddapron_domain.PolkaDomain)

let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_args () =
  let logi = ref false in
  let logfp = ref false in
  let logdomains = ref false in
  let logglobal = ref false in
  let widening_incr_step = ref 2 in
  let widening_decr_step = ref 2 in
  let widening_interf_step = ref 2 in
  let widening_interf_aff_step = ref 1 in
  let filename = ref "" in
  let speclist = [("--logi", Arg.Set logi, "Log des interferences");
                  ("--logfp", Arg.Set logfp, "Log des calculs des points fixes");
                  ("--logdomains", Arg.Set logdomains, "Affiche régulièrement les domaines");
                  ("--logglobal", Arg.Set logglobal, "la progression");
                  ("--wi", Arg.Int (fun x -> widening_incr_step := x), "Nombre d'increasing operations avant de faire un widening");
                  ("--wd", Arg.Int (fun x -> widening_decr_step := x), "Nombre de decreasing operations après un widening");
                  ("--winterf", Arg.Int (fun x -> widening_interf_step := x), "Nombre d'iterations avant de faire du widening sur les interférences, après chaque analyse des threads");
                  ("--waffinterf", Arg.Int (fun x -> widening_interf_aff_step := x), "Nombre d'iterations avant de faire du widening pendant l'application d'interférences dans une affectation");]
  in 
  let usage_msg = "This is BATMAN, a BAsic Thread-Modular ANalyzer. Avalaible options:" in 
  Arg.parse speclist (fun x -> filename := x) usage_msg;
  (!logi, !logfp, !logdomains, !logglobal, !widening_incr_step, !widening_decr_step, !widening_interf_step, !widening_interf_aff_step, !filename)



let parse_file filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- 
      { lex.Lexing.lex_curr_p with Lexing.pos_fname = filename; };
    let r = Parser.prog Lexer.lexer lex in
    close_in f;
    r
  with
  | Parser.Error ->
     Printf.eprintf "Parse error (invalid syntax) near %s\n" 
        (string_of_position lex.Lexing.lex_start_p);
      failwith "Parse error"
  | Failure "lexing: empty token" ->
     Printf.eprintf "Parse error (invalid token) near %s\n" 
        (string_of_position lex.Lexing.lex_start_p);
      failwith "Parse error"

let calc () =
  let logi, logfp, logd, logg, wis, wds, winterfstep, waff, filename = parse_args () in
  let iprog = parse_file filename in 
  let prog, env = Typrog.extract_prog iprog in
  PolyAnalyzer.global_analysis prog env (logi, logfp, logd, logg, wis, wds, winterfstep, waff)

let _ = calc ()
