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
# OCAMLC = ocamlc.opt
# OCAMLOPT = ocamlopt.opt
# OCAMLDEP = ocamldep
# OCAMLDOC = ocamldoc
# OCAMLLEX = ocamllex
# MENHIR = menhir


# ZARITHDIR = ~/.opam/4.02.1/lib/zarith
# APRONDIR = ~/.opam/4.02.1/lib/apron/ 
# GMPDIR = ~/.opam/4.02.1/lib/gmp/

# OCAMLINC = -I $(ZARITHDIR) -I $(APRONDIR) -I $(GMPDIR)
# OCAMLLIBS =  gmp.cma apron.cma polkaMPQ.cma octD.cma zarith.cma str.cma unix.cma -cclib "-L$(ZARITHDIR) -L$(APRONDIR) -L$(GMPDIR)"
# OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa polkaMPQ.cmxa octD.cmxa zarith.cmxa str.cmxa unix.cmxa -cclib "-L$(ZARITHDIR) -L$(APRONDIR) -L$(GMPDIR)"
# MENHIRFLAGS = --explain

all: analyzer

analyzer : 
	ocamlbuild -lflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/ unix.cma' -lflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/threads/ threads.cma' -cflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/' -cflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/threads/' -use-ocamlfind -tag thread -package apron,apron.apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll,apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,gmp -use-menhir batman.byte

bin:
	ocamlbuild -cflags '-g' -lflags '-g' -lflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/' -lflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/threads/' -cflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/' -cflags '-I /home/raphael/.opam/4.02.1/lib/ocaml/threads/' -use-ocamlfind -tag thread -package apron,apron.apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll,apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,gmp -use-menhir batman.native
