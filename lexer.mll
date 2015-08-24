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
(* Copyright (C) Raphaël Monat 2015.                                       *){
open Lexing
open Parser

}

let newline = '\r' | '\n' | "\r\n"

rule lexer = parse
| [' ' '\t']
 {lexer lexbuf}
| newline
{ new_line lexbuf; lexer lexbuf }
| "/*"
    { comments 0 lexbuf }
| "initial" { T_INIT }
| "thread" { T_THREAD }
| "var" { T_VAR }
| "skip" { T_SKIP }
| ":" { T_COLON }
| "," { T_COMMA }
| "true" { T_BOOL true }
| "false" { T_BOOL false }
| "(" { T_LPAR }
| ")" { T_RPAR }
| "||" { T_BOR }
| "or" { T_BOR }
| "&&" { T_BAND }
| "and" { T_BAND }
| "not" { T_NOT }
| "==" { T_BEQUAL }
| "<" { T_BLESS }
| "<=" { T_BLESSE }
| ">" { T_GREATER }
| ">=" { T_GREATERE }
| "+" { T_PLUS }
| "-" { T_MINUS }
| "*" {T_TIMES}
| "%" { T_PERCENT }
| "/" { T_DIVIDED }
| "random" { T_RAND }
            | "[" { T_LBRACKET }
            | "]" { T_RBRACKET }
| "assume" { T_ASSUME }
| "while" {T_WHILE}
| "do" { T_DO }
| "done" { T_DONE }
| "if" { T_IF }
| "then" { T_THEN }
| "else" { T_ELSE }
| "endif" { T_ENDIF }
| ";" { T_SEMICOLON}
| "=" { T_EQUAL }
| "begin" { T_BEGIN }
| "end" { T_END}
| ['_''a'-'z' 'A'-'Z'] ['_''a'-'z' 'A'-'Z' '0'-'9']*
  { T_id (Lexing.lexeme lexbuf) }
| '-'?['0'-'9']+
  { T_int_litt (int_of_string (Lexing.lexeme lexbuf)) }
| eof			
  { T_EOF }
and comments level = parse
		   | "*/" { if level = 0 then lexer lexbuf else comments (level-1) lexbuf}
		   | "/*" { comments (level+1) lexbuf }
		   | eof { T_EOF}
		   | _ {comments level lexbuf}
