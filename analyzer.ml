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

(* This files defines an iterator over an abstract domain. It uses the
   abstract domain D to analyze a given program *)

(*TODO
- ne pas utiliser diseq mais deux sup à la place ???
- assignation ou assignment ?
- Itération décroissante pour les interférences
r- Au début, tout passer dans apron et pas faire les conversions à chaque fois... (fait dans bddapron)
- Les filtrages sur les interférences servent à rien car on fait un meet ensuite. plus on ne sait pas ce qu'il se passe ?
- Faire un analyze_block plutot que hacker les t_id
- Nettoyage à faire
- Vérifier qu'on fait bien le calc_interf.
- On pourrait factoriser un peu le calcul des interf et des assign
- Log
- Gérer les erreurs*)

open Abs
open Domain
open Apron
open Apron_domain
open Printer

let log_i = ref false;;
let log_fp = ref false;;
let log_domains = ref true;;
let log_global = ref true;;
let widening_interf_aff_step = ref 1;;
let widening_interf_step = ref 2;;
let widening_incr_step = ref 2;;
let widening_decr_step = ref 2;;
let glob_indent = ref 0;;
let indent_step = "    ";;

module Iterator(D:ABSTRACT_DOMAIN) =
  struct
 
    (* returns the string indent_step ^ glob_indent *) 
    let tabs () = 
      let s = ref "" in
      for i = 1 to !glob_indent do
        s := !s ^ indent_step
      done;
      !s;;

    let open_indent () =
      incr glob_indent;;
    
    let close_indent () =
      decr glob_indent;;

    (* print textb, then a domain, then texta*)
    let print_domain domain textb texta =
      Format.fprintf Format.std_formatter "%s" textb;
      D.print Format.std_formatter domain;
      Format.fprintf Format.std_formatter "%s@." texta;;

    (* returns a chain containing the domain description *)
    let sprint_domain_only domain =
      D.print Format.str_formatter domain;
      Format.sprintf "\027[38;5;1m%s\027[0m" (Format.flush_str_formatter ());;

    (* prints all the domains and interferences *)
    let print_result nbprogs domains interf = 
        Format.fprintf Format.std_formatter "@.\t====== Domains ======\n";
        for x = 0 to nbprogs-1 do
          print_domain domains.(x) "" "";
        done;
        Format.fprintf Format.std_formatter "\t====== Interferences ======\n";
        for x = 0 to nbprogs-1 do
          print_domain interf.(x) "" ""
        done;;

      
    (* Computes, given a certain domain, the domain stable by the
       application of all interferences present in interf_tab (except at
       the position n_curr_th) *)
    let apply_all domain n_curr_th interf_tab = 
      if(!log_i) then Format.printf "\t\tApply_all interferences on thread%d\n" n_curr_th;
      (* Applies interference interf_tab[ith] to domaini *)
      let apply_interf_one domaini ith interf_tab =
        if(!log_i) then Format.printf "\t\tInterf_one with thread %d\n" ith;
        (* To compute interferences, we first have to double the dimension of the domain *)
        let dim = D.getdim domaini in
        let newvars = Array.make dim (Var.of_string "") in 
        let toremove = Array.make dim (Var.of_string "") in
        for i = 0 to dim-1 do
          newvars.(i) <- Var.of_string  ((Var.to_string (D.var_of_dim domaini i)) ^ "'"); 
          (* adding a quote to a variable prevents any collision with another variable, because of their definition in the lexer *)
          toremove.(i) <- D.var_of_dim domaini i
        done;
        let domain2 = D.add_vars ~init:false domaini newvars in
        if (!log_i) then 
          (
            print_domain domain2 "\tDomain2 : " "";
            print_domain interf_tab.(ith) "\n\tinterfs : " "";
          );
        D.meet_with domain2 interf_tab.(ith);
        if (!log_i) then print_domain domain2 "\n\tDomain2 after meet :" "";
        let domain3 = D.del_vars domain2 toremove in
        let domain4 = (D.ren_vars domain3 newvars toremove) in
        if (!log_i) then print_domain domain4 "\n\tDomain4" "\n";
        domain4;
      in

      (* Applies interferences in interf_tab[i] for i != n_curr_th to domain *)
      let apply_all_interf_one domain n_curr_th interf_tab =
        let res = ref domain in
        for i = 0 to (Array.length interf_tab)-1 do
          if(i <> n_curr_th) then 
            ( 
              res := D.join (!res) (apply_interf_one !res i interf_tab);
              if (!log_i) then print_domain !res ("\tRes in for with i = " ^ (string_of_int i)) "";
            )
        done;
        if (!log_i) then print_domain (!res) "\tRes calc_all_interf_one :" "\n";
        !res
      in

      let last = ref (D.bot domain) in
      let curr = ref domain in
      let count = ref 0  in
      while (not (D.subseteq !curr !last)) do
        last := !curr;
        let applied_interfs = (apply_all_interf_one (!curr) n_curr_th interf_tab) in
        if(!count <= !widening_interf_aff_step) then
          (
            curr := D.join (!curr) applied_interfs;
            incr count
          )
        else
          curr := D.widen (!curr) applied_interfs;
      done;
      D.join domain !curr

    (* Computes the domain after the assignation v <- e and interferences created by the preemption of other threads (and stored in interf_tab) had been taken into account *)
    let calc_assign v e nth domain interf_tab = 
      if(!log_i) then Format.printf "\t\tCalc_assign (thread %d, var %s)\n" nth v;
      let d2, d2err = D.assign domain v e in
      apply_all d2 nth interf_tab


    (* Computes the interferences created by an assignation v <- e, when te domain before the assignation is pre_assign *)
    let calc_interf pre_assign v e interf_t = 
      if (!log_i) then 
        (
          Format.printf "\t\tCalc_interf of var %s@." v;
          print_domain interf_t "\t\tCurrent interf_t : " "";
          print_domain pre_assign "\t\tPre_assign : " "";
        );
      let dim = D.getdim pre_assign in
      let newvars = Array.make dim (Var.of_string "") in 
      for i = 0 to dim-1 do
        newvars.(i) <- Var.of_string ((Var.to_string (D.var_of_dim pre_assign i)) ^ "'");
      done;
      let res = D.add_vars ~init:false pre_assign newvars in
      let dom2, dom2err = D.assign res (v ^ "'") e in
      for i = 0 to dim-1 do
        if(D.var_of_dim pre_assign i <> Var.of_string v) then
          D.assign_with dom2 (Var.to_string newvars.(i)) (AVar (Var.to_string (D.var_of_dim pre_assign i)));
      done;
      let dom3 = D.join interf_t dom2 in
      if (!log_i) then print_domain dom3 "\t\tdom3 (calc_interf) = " "";
      dom3

      (** Analysis of a thread
          @param th : the thread we want to analyze
          @param t_id : the thread number
          @param interferences : the interferences array * interferences created by this thread
          @param thread_domain : the threads domain at the endle domaine du thread en question
          @return the interferences created by this thread * the new domain at the end of the thread
      *)
      let rec analyze th t_id interferences thread_domain ann_prog analysis lincons = 
        let (union, interf_t) = interferences in
        match th with
        | CSkip -> 
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%sskip;\n%s%s\n" (tabs ()) (tabs ()) (sprint_domain_only thread_domain)); 
          interf_t, thread_domain
        | CAssign (v, e) -> 
          let (i, d) = 
            (match e with
             | CstA a -> (calc_interf thread_domain v a interf_t), (calc_assign v a t_id thread_domain union) 
             | _ -> failwith "CAssign ? not implemented")
          in
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%s%s = %s;\n%s%s\n" (tabs ()) v (sprint_exp e) (tabs ()) (sprint_domain_only d));
          i, d
        | CAssume e -> 
          let r1, _, _ = D.filter thread_domain e in 
          let r = apply_all r1 t_id union in
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%sassume %s\n%s%s\n" (tabs ()) (sprint_bexpr e) (tabs ()) (sprint_domain_only r)); 
          interf_t, r
        | CSeq (c1, c2) -> let i, d = analyze c1 t_id interferences thread_domain ann_prog analysis lincons in
          if (!log_domains) then Format.printf "NEW SEQ@.@.";
          analyze c2 t_id (union, i) d ann_prog analysis lincons
        | CIf (b, t, f) -> 
          let thread_domain_i = apply_all thread_domain t_id union in
          if (!log_domains) then Format.printf "CIf@.";
          if(!log_domains) then print_domain thread_domain_i "Before :" "";
          let t_domain, f_domain, err = D.filter thread_domain_i b in
          if (ann_prog) then
            (
              analysis := !analysis ^ (Format.sprintf "%sif %s then\n"  (tabs ()) (sprint_bexpr b));
              open_indent ()
            );
          let int_true, dom_true = analyze t t_id (union, interf_t) t_domain ann_prog analysis lincons in
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%selse\n" (tabs ()));
              open_indent ();
            );
          let int_false, dom_false = analyze f t_id (union, interf_t) f_domain ann_prog analysis lincons in (* danger sur les interferences ? clairement. Comment gérer ça ?*)
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%sendif;\n" (tabs ()));
            );
          if(!log_domains) then
            (
              print_domain dom_true "After true" "";
              print_domain dom_false "After false" "";
              print_domain (D.join dom_true dom_false) "Join" "";
            );
          (D.join interf_t (D.join int_true int_false)), (D.join dom_true dom_false)
        | CWhile (b, c) -> if (!log_domains) then Format.printf "CWhile\n"; 
          let in_domain_, out_domain, _ = D.filter thread_domain b in
          let in_domain = apply_all in_domain_ t_id union in
          if (ann_prog) then
            (
              analysis := !analysis ^ (Format.sprintf "%swhile %s do\n" (tabs ()) (sprint_bexpr b));
              open_indent ();
            );
          let s = ref "" in
          let i, d = fp t_id b c in_domain interf_t union ann_prog s 
              (fun int dom annp analys -> 
                 let domain_in_, _, _ = D.filter dom b in 
                 let domain_in = apply_all domain_in_ t_id union in
                 let (i, d) = (analyze c t_id (union, int) domain_in annp analys lincons) in
                 i, d, domain_in) lincons in
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%s%sdone;\t%s\n" !s (tabs ()) (sprint_domain_only d));
            );
          let (_, r2, _) =  (D.filter d b)
          in (D.join i interf_t, D.join r2 out_domain)
      (* computes the fixpoint in case of a while statement *)
      and fp t_id boexpr c beg_domain beg_interf_t glob_interf ann_prog analysis calc_onestep lincons =  
        (* TODO : arguments -- donner t_id, interferences Retour : plut^ot domaines ET interférences non ? *)
        let interfs = ref beg_interf_t in
        let rec increase nb_inc domain = 
          if (!log_fp) then print_domain domain "\tDomain = " "";
          if (nb_inc > 0) then 
            (
              if (!log_fp) then 
                (
                  Format.fprintf Format.std_formatter "\tincreaseif\n";
                  print_domain !interfs "\t!interfs" "";
                );
              let i, d, domain_in = calc_onestep !interfs domain false (ref "") in
              (* TODO : et ici ? il faudrait pas refiltrer ? Ou ça serait fait avant ?*)
              increase (nb_inc-1) (D.join d domain_in)
            )
          else
            (
              if (!log_fp) then Format.fprintf Format.std_formatter "\n\tincreaseelse\n";
              let i, d, domain_in = calc_onestep !interfs domain false (ref "") in
              let domain' = D.widen_threshold domain_in (D.join d domain_in) lincons in
              if (!log_fp) then 
                (
                  print_domain domain' "\tDomain' = " "";
                  print_domain !interfs "\t!interfs = " "\n";
                );
              if (D.subseteq domain' domain) then domain'
              else
                increase 0 domain'
            )
        in
        let rec decrease nb_decr domain = 
          if (nb_decr > 0) then 
            (
              if (!log_fp) then Format.fprintf Format.std_formatter "\tdecreaseif@.";
              if (!log_fp) then print_domain domain "\tdecreaseif, old domain" "";
              let i, d, domain_in = 
                if (nb_decr == 1 && ann_prog) then
                  calc_onestep !interfs domain true analysis
                else
                  calc_onestep !interfs domain false (ref "")
              in
              if (!log_fp) then 
                (
                  print_domain d "\tdecreaseif, new domain" "";
                  print_domain !interfs "\t!interfs" "";
                );
              if (nb_decr == 1) then interfs := D.join (!interfs) i;
              decrease (nb_decr-1) (D.join d beg_domain)
            )
          else
            (
              if (!log_fp) then 
                (
                  print_domain domain "\tdecreaseelse" "";
                  print_domain !interfs "\t!interfs" "";
                );
              !interfs, domain
            )
        in 
        decrease !widening_decr_step (increase !widening_incr_step beg_domain)

      (* search for interesting linear constraints in if statements,
         to be able to use them in widening with threshold *)
      let search_lincons init_domain prog = 
        let rec search p acc = 
          match p with
          | CIf (b, ct, cf) -> 
            let _, d, _ = D.filter init_domain b in
            search ct (search cf (d::acc))
          | CSeq (c1, c2) -> search c2 (search c1 acc) 
          | CWhile (b, c) -> search c acc
          (* TODO : add search here too ?*)
          | _ -> acc
        in
        let dom_array = Array.of_list (search prog []) in
        if (Array.length dom_array) == 0 then
          Lincons1.array_make (D.getenv init_domain) 0
        else
          (
            let d = D.bot init_domain in
            for x = 0 to (Array.length dom_array)-1 do
              D.join_with d dom_array.(x);
            done;
            let to_neg = D.to_lincons_array d in
            for x = 0 to (Lincons1.array_length to_neg)-1 do
              let a = Lincons1.array_get to_neg x in
              let tmp = ref [] in
              Lincons1.iter (fun x y -> tmp := (Coeff.neg x,y)::(!tmp)) a;
              Lincons1.set_list a !tmp (Some (Coeff.neg (Lincons1.get_cst a)));
            done;
            to_neg
          )

      (** Global analysis of the full multithreaded program 
          @param some threads, the list of global variables, a boolean
          expression giving the initial conditions on the variables *
          some parameters 
          @return nothing ?  *)
    let global_analysis (threads, glob_vars, bexpr_init) (logi, logfp, logd, logg, wis, wds, winterfstep, waff) =
      log_i := logi;
      log_fp := logfp;
      log_domains := logd;
      log_global := logg;
      widening_incr_step := wis;
      widening_decr_step := wds;
      widening_interf_step := winterfstep;
      widening_interf_aff_step := waff;
      let realthreads = Array.of_list threads in
      let realprog = Array.map (fun x -> let Thread (_,_,c) = x in c) realthreads in
      let nbprogs = Array.length realthreads in
      (* array of interferences *)
      let interf = Array.make nbprogs (D.empty ()) in
      (* array of domains, to store the result at the end of each analysis *)
      let domains = Array.make nbprogs (D.empty ()) in
      (* array of domains, where variables are intiated according to bexpr_init *)
      let idomains = Array.make nbprogs (D.empty ()) in
      (*  same as idomains, but I need a copy of idomains *)
      let init_domains = Array.make nbprogs (D.empty ()) in
      let lincons = Array.make nbprogs (Lincons1.array_make (D.getenv (D.empty ())) 0) in
      (* we add variables to each domain *)
      for x = 0 to nbprogs-1 do
        let vars = (Array.of_list (let rec parsevars z =
                                                 match z with
                                                 | [] -> []
                                                 | x::q -> (match x with
                                                     | VarDeclI v ->  (D.apron_of_var v)::parsevars q
                                                     | _ -> parsevars q)
                                               in parsevars glob_vars)) in
        domains.(x) <- (D.add_vars domains.(x) vars);
        idomains.(x) <- (D.add_vars ~init:false idomains.(x) vars );
        lincons.(x) <- search_lincons idomains.(x) realprog.(x);
        if (!log_domains) then
          (
            Format.printf "Detected tresholds for threads %i :" x;
            Lincons1.array_print Format.std_formatter lincons.(x);
            Format.printf "@.";
          );
        let initialized, _, _ = D.filter idomains.(x) bexpr_init in
        idomains.(x) <- initialized;
        init_domains.(x) <- initialized;
        interf.(x) <- (D.add_vars (* ~init:false *) interf.(x) vars);
        interf.(x) <- (D.add_vars (* ~init:false *) interf.(x) (Array.map (fun x -> Var.of_string ((Var.to_string x) ^ "'")) vars));
        interf.(x) <- D.bot interf.(x);
      done;

      (* this function computes one step of the thread-modular
         analysis, ie: for each threads, it computes the new domain at
         the end, and the new interferences created by each thread *)
      let analyze_onestep stepn = 
        let is_stable = ref true in
        if(!log_global) then Format.printf "@.Analyze_onestep, stepn = %d\n" stepn;
        let newidomains = Array.make nbprogs (D.bot interf.(0)) in
        for x = 0 to nbprogs-1 do
          let Thread (id, _, _) = realthreads.(x) in 
          idomains.(x) <- apply_all (init_domains.(x)) x interf;
          
          if(!log_global || !log_i) then Format.fprintf Format.std_formatter "@.\tAnalysing thread %d (ie %s)@." x id;
          if(!log_domains) then
            (
              print_domain domains.(x) "\tOldDomain : " "";
              print_domain interf.(x) "\tOldInterf_t :" "\n";
            );
          let s = ref "" in
          let (i, d) = analyze realprog.(x) x (interf, interf.(x)) idomains.(x) false s lincons.(x) in
          if (stepn < !widening_interf_step) then
            newidomains.(x) <- i
          else
            newidomains.(x) <- D.widen interf.(x) i;
          domains.(x) <- d;
          is_stable := !is_stable && (D.eq interf.(x) newidomains.(x)) && (D.eq domains.(x) d);
          if(!log_domains) then
            (
              print_domain domains.(x) "\tNewDomain : " "";
              print_domain newidomains.(x) "\n\tNewInterf_t : " "";
            );
        done;
        for x = 0 to nbprogs-1 do
          interf.(x) <- newidomains.(x)
        done;
        !is_stable
      in
      let n = ref 1 in
      while not (analyze_onestep !n) do
        incr n;
        if(!log_domains || !log_global) then print_result nbprogs domains interf
      done;

      Format.printf "@.Analysis completed in %d steps\n" !n;
      let s = ref "" in
      for x = 0 to nbprogs-1 do
        let Thread (id, _, _) = realthreads.(x) in 
        s := !s ^ Format.sprintf "thread %s:\nbegin\n" id;
        open_indent ();
        idomains.(x) <- apply_all (init_domains.(x)) x interf;
        if(!log_global || !log_i) then Format.fprintf Format.std_formatter "@.\tAnalysing thread %d (ie %s)@." x id;
        s := !s ^ Format.sprintf "%s%s\n" (tabs ()) (sprint_domain_only idomains.(x));
        let _ = analyze realprog.(x) x (interf, interf.(x)) (idomains.(x)) true s lincons.(x) in
        s := !s ^ Format.sprintf "end\n\n";
        close_indent ();
      done;
      Format.printf "%s" !s;

      print_result nbprogs domains interf;
      let res_final = ref domains.(0) in
      for x = 1 to nbprogs-1 do
        res_final := D.meet !res_final domains.(x)
      done;
      print_domain !res_final "Meet :" "";
  end
