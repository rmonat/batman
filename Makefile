# (* This file is part of the Batman analyzer, released under GPLv3          *)
# (* license. Please read the COPYING file packaged in the distribution.     *)
# (*                                                                         *)
# (* The Batman analyzer is free software: you can redistribute it and/or    *)
# (* modify it under the terms of the GNU General Public License as          *)
# (* published by the Free Software Foundation, either version 3 of the      *)
# (* License, or (at your option) any later version.                         *)
# (*                                                                         *)
# (* The Batman analyzer is distributed in the hope that it will be useful,  *)
# (* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
# (* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *)
# (* General Public License for more details.                                *)
# (*                                                                         *)
# (* You should have received a copy of the GNU General Public License       *)
# (* along with this Batman analyzer.  If not, see                           *)
# (* <http://www.gnu.org/licenses/>                                          *)
# (*                                                                         *)
# (* Copyright (C) Raphaël Monat 2015.                                       *)

all: analyzer

analyzer : 
	ocamlbuild -tag custom -use-menhir batman.byte -use-ocamlfind -package apron,apron.apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll,apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,gmp,bddapron,bddapron.bdd,bddapron.bddapron

debug:
	ocamlbuild -tag debug -use-menhir batman.d.byte -use-ocamlfind -package apron,apron.apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll,apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,gmp,bddapron,bddapron.bdd,bddapron.bddapron



bin:
	ocamlbuild -tag custom -use-menhir batman.native -use-ocamlfind -package apron,apron.apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll,apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,gmp,bddapron,bddapron.bdd,bddapron.bddapron

