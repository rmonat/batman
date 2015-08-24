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

%{
  open Abs
%}

%token T_INIT
%token T_THREAD
%token T_VAR
%token T_COLON
%token T_COMMA
%token T_LBRACKET
%token T_RBRACKET

%token <bool> T_BOOL
%token T_LPAR
%token T_RPAR
%token T_BOR
%token T_BAND
%token T_NOT
%token T_BEQUAL
%token T_BLESS
%token T_BLESSE
%token T_GREATER
%token T_GREATERE

%token T_PLUS
%token T_MINUS
%token T_TIMES
%token T_PERCENT
%token T_DIVIDED

%token T_RAND
%token T_ASSUME

%token T_WHILE
%token T_DO
%token T_DONE
%token T_IF
%token T_THEN
%token T_ELSE
%token T_ENDIF
%token T_SEMICOLON
%token T_EQUAL
%token T_BEGIN
%token T_END
%token T_SKIP

%token <string> T_id
%token <int> T_int_litt

%token T_EOF

%nonassoc T_WHILE 
%nonassoc T_DO
%nonassoc T_DONE
%nonassoc T_IF T_THEN 
%nonassoc T_ELSE
%nonassoc T_LPAR T_RPAR T_BEGIN T_END
%left T_BOR
%left T_BAND
%nonassoc T_NOT
%left T_BEQUAL
%left T_BLESS T_BLESSE T_GREATER T_GREATERE T_EQUAL
%left T_PLUS T_MINUS
%left T_TIMES T_DIVIDED T_PERCENT



%start<Abs.thread list * Abs.vardecl list * Abs.bexpr> prog
%type<Abs.thread list> prog2
%type <Abs.cmd> cmd
%type <Abs.cmd> cmd2
/*%start<Abs.cmd> cmd*/
%type <Abs.aexpr> aexp
%type <Abs.bexpr> bexp
%type <Abs.exp> exp
%type <Abs.vardecl> vardecl
%type <Abs.vardecl list> vardecll
%type <Abs.thread> thread_decl

%%

aexp :
| x = T_int_litt { AInt x}
| v = T_id { AVar v}
(*| T_RAND { ARand } divergence with concurinterproc. (but only way to manage rands in bddapron ?...*)
| T_LBRACKET x = T_int_litt T_COMMA y = T_int_litt T_RBRACKET { ARand (x, y) }
| x = aexp T_PLUS y = aexp {APlus (x, y)}
| x = aexp T_MINUS y = aexp {AMinus (x, y)}
| x = aexp T_TIMES y = aexp {ATimes (x, y)}
| x = aexp T_DIVIDED y = aexp {ADivided (x, y)}
| x = aexp T_PERCENT y = aexp {APercent (x, y)}
| T_MINUS x = aexp {ANeg x}
| T_LPAR x = aexp T_RPAR {x}

bexp :
| x = T_BOOL {if(x = true) then BTrue else BFalse}
/*| v = var {BVar v}*/
| x = bexp T_BAND y = bexp {Band (x, y)}
| x = bexp T_BOR y = bexp {Bor (x, y)}
| x = aexp T_BEQUAL y = aexp {Bequal (x, y)}
| x = aexp T_BLESS y = aexp {Bless (x, y)}
| x = aexp T_BLESSE y = aexp {Blesse (x, y)}
| x = aexp T_GREATER y = aexp {Bgreater (x, y)}
| x = aexp T_GREATERE y = aexp {Bgreatere (x, y)}
| T_NOT x = bexp {BNot x}
| T_LPAR x = bexp T_RPAR {x}

exp :
| a = aexp { CstA a}
| b = bexp { CstB b}
/*| T_RAND {CstRand}*/
/*| T_LPAR x = exp T_RPAR {x}*/

cmd :
| cl = cmd c = commande {CSeq(cl, c)}
| c = commande {c}

commande :
| c = cmd2 T_SEMICOLON {c}

cmd2 :
| T_ASSUME b = bexp { CAssume b}
| T_SKIP { CSkip }
| v = T_id T_EQUAL e = exp { CAssign (v, e); }
| T_IF b = bexp T_THEN c1 = cmd T_ENDIF {CIf(b, c1, CSkip)}
| T_IF b = bexp T_THEN T_ELSE c1 = cmd T_ENDIF {CIf(b, CSkip, c1)}
| T_IF b = bexp T_THEN c1 = cmd T_ELSE c2 = cmd T_ENDIF {CIf(b, c1, c2)}
| T_WHILE b = bexp T_DO T_DONE {CWhile(b, CSkip)}
| T_WHILE b = bexp T_DO c = cmd T_DONE {CWhile(b, c)}


prog :
| T_VAR v = vardecll T_INIT b = bexp T_SEMICOLON p = prog2 { (p, v, b) }
| T_VAR v = vardecll p = prog2 { (p, v, BTrue) }


prog2 :
| th = thread_decl T_EOF { [th] }
| th = thread_decl p = prog2 { th::p }

vardecl :
| v = T_id T_COLON t = T_id {if(t = "int") then VarDeclI v else VarDeclB v}

vardecll :
| v = vardecl T_SEMICOLON { [v] }
| v = vardecl T_COMMA l = vardecll { (v::l)}

thread_decl : 
| T_THREAD t_id = T_id T_COLON
			     T_BEGIN c = cmd T_END { Thread(t_id, [], c)}
| T_THREAD t_id = T_id T_COLON
			     T_VAR v = vardecll
			     T_BEGIN c = cmd T_END { Thread(t_id, v, c)}
			     
