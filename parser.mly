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
(* Copyright (C) Raphaël Monat 2015.                                       *)%{
  open Abs_init
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



%start<Abs_init.ithread list * Abs_init.ivardecl list * Abs_init.iexpr> prog
%type<Abs_init.ithread list> prog2
%type <Abs_init.icmd> cmd
%type <Abs_init.icmd> cmd2
/*%start<Abs_init.cmd> cmd*/
%type <Abs_init.iexpr> exp
%type <Abs_init.ivardecl> vardecl
%type <Abs_init.ivardecl list> vardecll
%type <Abs_init.ithread> thread_decl

%%


exp :
| x = T_int_litt { IAInt x}
| v = T_id { IAVar v}
| T_LBRACKET x = T_int_litt T_COMMA y = T_int_litt T_RBRACKET { IARand (x, y) }
| x = exp T_PLUS y = exp {IAPlus (x, y)}
| x = exp T_MINUS y = exp {IAMinus (x, y)}
| x = exp T_TIMES y = exp {IATimes (x, y)}
| x = exp T_DIVIDED y = exp {IADivided (x, y)}
| x = exp T_PERCENT y = exp {IAPercent (x, y)}
| T_MINUS x = exp {IANeg x}
| x = T_BOOL {if(x = true) then IBTrue else IBFalse}
| x = exp T_BAND y = exp {IBand (x, y)}
| x = exp T_BOR y = exp {IBor (x, y)}
| x = exp T_BEQUAL y = exp {IBequal (x, y)}
| x = exp T_BLESS y = exp {IBless (x, y)}
| x = exp T_BLESSE y = exp {IBlesse (x, y)}
| x = exp T_GREATER y = exp {IBgreater (x, y)}
| x = exp T_GREATERE y = exp {IBgreatere (x, y)}
| T_NOT x = exp {IBNot x}
| T_LPAR x = exp T_RPAR {x}

cmd :
| cl = cmd c = commande {ICSeq(cl, c)}
| c = commande {c}

commande :
| c = cmd2 T_SEMICOLON {c}

cmd2 :
| T_ASSUME b = exp { ICAssume b}
| T_SKIP { ICSkip }
| v = T_id T_EQUAL e = exp { ICAssign (v, e); }
| T_IF b = exp T_THEN c1 = cmd T_ENDIF {ICIf(b, c1, ICSkip)}
| T_IF b = exp T_THEN c1 = cmd T_ELSE c2 = cmd T_ENDIF {ICIf(b, c1, c2)}
| T_WHILE b = exp T_DO c = cmd T_DONE {ICWhile(b, c)}

prog :
| T_VAR v = vardecll T_INIT b = exp T_SEMICOLON p = prog2 { (p, v, b) }
| T_VAR v = vardecll p = prog2 { (p, v, IBTrue) }


prog2 :
| th = thread_decl T_EOF { [th] }
| th = thread_decl p = prog2 { th::p }

vardecl :
| v = T_id T_COLON t = T_id {if(t = "int") then IVarDeclI v else IVarDeclB v}

vardecll :
| v = vardecl T_SEMICOLON { [v] }
| v = vardecl T_COMMA l = vardecll { (v::l)}

thread_decl : 
| T_THREAD t_id = T_id T_COLON
			     T_BEGIN c = cmd T_END { IThread(t_id, [], c)}
| T_THREAD t_id = T_id T_COLON
			     T_VAR v = vardecll
			     T_BEGIN c = cmd T_END { IThread(t_id, v, c)}
			     
