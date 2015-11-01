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

(* Faire un module d'affichage que l'on passe à analyze ? *)
(*TODO
- changer les perint. Verifier interférences avec booléens. Changer la partie sur les dimensions : à la place, garder les variables et les variables' qui de toute façon ne changent en fait pas au fil du temps

- Itération décroissante pour les interférences
- Au début, tout passer dans apron et pas faire les conversions à chaque fois...
- Les filtrages sur les interférences servent à rien car on fait un meet ensuite. plus on ne sait pas ce qu'il se passe ?
- Faire un analyze_block plutot que hacker les t_id
- Nettoyage à faire
- Vérifier qu'on fait bien le calc_interf.
- On pourrait factoriser un peu le calcul des interf et des assign
- Log
- Gérer les erreurs*)
open Abs
open Bdddomain
open Bddapron_domain
open Bddapron

let log_i = ref false;;
let log_fp = ref false;;
let log_domains = ref true;;
let log_global = ref true;;
let widening_interf_aff_step = ref 1;;
let widening_interf_step (*basic_interf_step*) = ref 2;;
let widening_incr_step = ref 2;;
let widening_decr_step = ref 2;;
let glob_indent = ref 0;;
let indent_step = "    ";;

module Iterator(D:BDD_ABSTRACT_DOMAIN) =
  struct

    module A = Abs(D)

    let newvars = ref []
    let toremove = ref []
    let threadname = ref [||]

    let sprint_aexpr a = D.apron_print Format.str_formatter a; Format.flush_str_formatter ();;
    let sprint_bexpr b = D.bool_print Format.str_formatter b; Format.flush_str_formatter ();;

    let sprint_exp e = D.expr_print Format.str_formatter e; Format.flush_str_formatter ();;
 
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

    let print_domain domain textb texta =
      Format.fprintf Format.std_formatter "%s" textb;
      D.print Format.std_formatter domain;
      Format.fprintf Format.std_formatter "%s@." texta;;
    
    let sprint_domain_only domain =
      D.print Format.str_formatter domain;
      Format.sprintf "\027[38;5;1m%s\027[0m" (Format.flush_str_formatter ());;

    let print_result nbprogs domains interf = 
        Format.fprintf Format.std_formatter "@.\t====== Domains ======\n";
        for x = 0 to nbprogs-1 do
          print_domain domains.(x) "" "";
        done;
        Format.fprintf Format.std_formatter "\t====== Interferences ======\n";
        for x = 0 to nbprogs-1 do
          print_domain interf.(x) "" ""
        done;;

    let apply_all domain n_curr_th interf_tab = 
      if(!log_i) then Format.printf "\t\tApply_all interferences on thread%d\n" n_curr_th;
      (* TODO : c'est correct de faire ça ? 
         Refactoriser certaines parties *)
      (* let apply_decrease domaini n_curr_th interf_tab = *)
      (*   let res = ref (D.bot (D.getenv domaini)) in *)
      (*   for x = 0 to (Array.length interf_tab)-1 do *)
      (*     if (x <> n_curr_th) then *)
      (*       ( *)
      (*         let tmp = D.del_vars interf_tab.(x) !toremove in *)
      (*         let tmp2 = D.ren_vars tmp !newvars !toremove in *)
      (*         res := D.join !res (D.meet tmp2 domaini); *)
      (*       ) *)
      (*   done; *)
      (*   if(!log_i) then  *)
      (*     ( *)
      (*       print_domain !res ("\t\tApply_decrease interferences on thread " ^ (string_of_int n_curr_th)) ""; *)
      (*     ); *)
      (*   !res *)
      (* in *)
      let apply_interf_one domaini ith interf_tab =
        if(!log_i) then Format.printf "\t\tInterf_one with thread %d\n" ith;
        (* On veut commencer par doubler la dimension de domain *)
        let domain2 = D.add_vars ~init:false domaini !newvars in
        if (!log_i) then 
          (
            print_domain domain2 "\tDomain2 : " "";
            print_domain interf_tab.(ith) "\n\tinterfs : " "";
          );
        let domain2 = D.meet domain2 interf_tab.(ith) in (** on veut faire domain2 = domain2 inter interf_tab[ith]. Bon ordre des domaines ?*)
        if (!log_i) then print_domain domain2 "\n\tDomain2 after meet :" "";
        let domain3 = D.del_vars domain2 !toremove in
        if (!log_i) then print_domain domain3 "\n\tDomain3" "";
        let domain4 = (D.ren_vars domain3 !newvars !toremove) in
        if (!log_i) then print_domain domain4 "\n\tDomain4" "\n";
        domain4;
      in
      let apply_all_interf_one domain n_curr_th interf_tab =
        let res = ref domain in
        for i = 0 to (Array.length interf_tab)-1 do
          if(i <> n_curr_th) then 
            ( 
              res := D.join (!res) (apply_interf_one !res i interf_tab); (*TODO : il manque probablement un join dans le coin *)
              if (!log_i) then print_domain !res ("\tRes in for with i = " ^ (string_of_int i)) "";
            )
        done;
        if (!log_i) then print_domain (!res) "\tRes calc_all_interf_one :" "\n";
        !res
      in
      let last = ref (D.bot (D.getenv domain)) in
      let curr = ref domain in
      let count = ref 0  in 
      while (not (D.subseteq !curr !last)) do
        last := !curr;
        let applied_interfs = (apply_all_interf_one (!curr) n_curr_th interf_tab) in
        (* print_domain !curr "Curr = " ""; *)
        (* print_domain applied_interfs "Applied interfs = " ""; *)
        if(!count <= !widening_interf_aff_step) then
          (
            (* print_domain !curr "Curr : " ""; *)
            (* print_domain applied_interfs "Applied_interfs" ""; *)
            curr := D.join (!curr) (*D.join domain*) applied_interfs;
            incr count
          )
          (* TODO : trouver un compromis entre le widen et le join*)
          (* Faire une itération décroissante ? Je vois pas comment... *)
        else
          curr := D.widen (!curr) (*D.join domain*) applied_interfs;
      done;
      D.join domain (*(apply_decrease*) !curr (*n_curr_th interf_tab*)
    (*!curr*)

    (** Calcule le nouveau domaine que peut prendre domain après qu'on ait fait v <- e dans le thread nth sachant que les autres threads peuvent interférer au niveau de interf_tab *)
    (** lui passer man et env ? ou env se trouve dans domaine -> Abstract1.env ??*)
    let calc_assign v e nth domain interf_tab id labi labf labmax = 
      if(!log_i) then Format.printf "\t\tCalc_assign (thread %d, var %s)\n" nth v;
      let d2, d2err = D.assign domain v e in
      apply_all d2 nth interf_tab


    let calc_interf pre_assign v e interf_t id labi labf labmax = 
      if (!log_i) then 
        (
          Format.printf "\t\tCalc_interf of var %s@." v;
          print_domain interf_t "\t\tCurrent interf_t : " "";
          print_domain pre_assign "\t\tPre_assign : " "";
        );
      let res = D.add_vars ~init:false pre_assign !newvars in
      let dom2, dom2err = ref (fst (D.assign res (v ^ "'") e)), () in
      (* for i = 0 to dim-1 do *)
      (*   if(D.var_of_dim pre_assign i <> Var.of_string v) then *)
      (*     D.assign_with dom2 (Var.to_string newvars.(i)) (AVar (Var.to_string (D.var_of_dim pre_assign i))); *)
      (* done; *)
      List.iter (fun x -> if ((fst x) <> v) then dom2 := fst (D.assign !dom2 ((fst x)^"'") (D.expr_var (D.getenv !dom2) (fst x)))) !toremove;
      let auxname = "_aux_"^(!threadname.(id)) in
      dom2 := fst (D.assign !dom2 (auxname^"'") (D.bint_expr (D.bint_cst (D.getenv !dom2) labmax labf)));
      dom2 := fst (D.assign !dom2 (auxname) (D.bint_expr (D.bint_cst (D.getenv !dom2) labmax labi)));
      let dom3 = D.join interf_t !dom2 in
      (*let dom5, _, _ = D.filter dom3 (BNot (Bequal ((AVar v), (AVar (v ^ "'"))))) in (* et là on veut l'expression y = y' car on prend la négation sur le filter car après pbs d'implémentations*) *)
      if (!log_i) then print_domain dom3 "\t\tdom3 (calc_interf) = " "";
      dom3
        (* faire un filter ?*)

    (* force that only var=label in dom *)
    let enforce_label dom var label labmax = 
      fst (D.assign dom (var) (D.bint_expr (D.bint_cst (D.getenv dom) labmax label)))

                                      (* init : enforce init? _aux_bla 0*)
                                      (* et après le faire avec les l' à chaque fois. Pb avec les while ou truc du genre ? Je sais pas...*)
      
      (** Analyse d'un thread1
          @param th : le thread en question
          @param t_id : le numéro du thread en question dans le tableau des interférences
          @param interferences : le tableau des interférences * les interférences locales
          @param thread_domain : le domaine du thread en question
          @param ann_prog : un booléen qui si vrai annote le programme par son analyse
          @param analysis : la chaine de caractère qui garde l'annotation du programme si ann_prog
          @param lincons : les contraintes détectées automatiquement pour le widening avec threshold
          @param nb_lab : nombre de labels pour t_id OU on fait un tableau
          @return les nouvelles interférences * le domaine du thread ?
      *)

    (* A MODIFIER : 
       Ajouter les variables _aux_... Ok normalement ?
       Eventuellement faire attention au début du domaine ? (ou pour le thread 1 pas ajouter les _aux_autres)
       Quand on crée une interférence, bien mettre _aux_(t_id) (ou le nom du thread ?) _aux_(t_id)' 
       Pour calc interf : rajouter le changement l->l' pour _aux_(threadname.(t_id))
       Pour calc assign : appliquer le changement [v -> e] pour _aux_(...) = l'

       Quand on applique les interférences ?       
    *)
      let rec analyze th t_id interferences thread_domain ann_prog analysis lincons nb_lab = 
        (* prendre le thread (-,-,-) ou juste la cmd comme on fait pour le moment ? *)
        let (union, interf_t) = interferences in
        match th with
        | A.CSkip -> 
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%sskip;\n%s%s\n" (tabs ()) (tabs ()) (sprint_domain_only thread_domain)); 
          interf_t, thread_domain
        | A.CAssign (l, l', v, e) -> (* là c'est la partie pénible, il faut faire attention aux interférences etc etc *)
          (*          Printf.printf "CAssign %s\n" v;*)
          let th_d = (enforce_label thread_domain ("_aux_"^(!threadname.(t_id))) l' nb_lab) in
          let (i, d) = (calc_interf th_d v e interf_t t_id l l' nb_lab), (calc_assign v e t_id th_d union t_id l l' nb_lab) 
          in
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%s[%d/%d]%s = %s;[%d/%d]\n%s%s\n" (tabs ()) l nb_lab v (sprint_exp e) l' nb_lab (tabs ()) (sprint_domain_only d));
          i, d
        | A.CAssume (l, l', e) -> 
          let th_d = (enforce_label thread_domain ("_aux_"^(!threadname.(t_id))) l' nb_lab) in
          let r1, _, _ = D.filter th_d e in 
          let r = apply_all r1 t_id union in
          if (ann_prog) then analysis := !analysis ^ (Format.sprintf "%s[%d]assume %s[%d]\n%s%s\n" (tabs ()) l (sprint_bexpr e) l' (tabs ()) (sprint_domain_only r)); 
          interf_t, r
        | A.CSeq (c1, c2) -> let i, d = analyze c1 t_id interferences thread_domain ann_prog analysis lincons nb_lab in
          if (!log_domains) then Format.printf "NEW SEQ@.@.";
          let i, d = analyze c2 t_id (union, i) d ann_prog analysis lincons nb_lab in
          i, d
        | A.CIf (l, l', b, t, f) -> 
          (* if (Texpr1.is_interval_linear (Tcons1.get_texpr1 (fst (D.tcons_of_bexpr (D.getenv thread_domain) b)))) then  *)
          (*   Format.printf "La condition booléenne %s est linéaire@." (sprint_bexpr b) *)
          (* else Format.printf "La condition booléenne %s n'est PAS linéaire@." (sprint_bexpr b); *)
(*          let th_d = (enforce_label thread_domain ("_aux_"^(!threadname.(t_id))) l' nb_lab) in*)
          let th_d = thread_domain in
          let thread_domain_i = apply_all th_d t_id union in
          if (!log_domains) then Format.printf "CIf@.";
          if(!log_domains) then print_domain thread_domain_i "Before :" "";
          let t_domain, f_domain, err = D.filter thread_domain_i b in
(*          let interf_true, interf_false, err = D.filter interf_t b in*)
          if (ann_prog) then
            (
              analysis := !analysis ^ (Format.sprintf "%s[%d]if %s then\n"  (tabs ()) l (sprint_bexpr b));
              open_indent ()
            );
          let int_true, dom_true = analyze t t_id (union, interf_t) t_domain ann_prog analysis lincons nb_lab in
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%selse\n" (tabs ()));
              open_indent ();
            );
          let int_false, dom_false = analyze f t_id (union, interf_t) f_domain ann_prog analysis lincons nb_lab in (* danger sur les interferences ? clairement. Comment gérer ça ?*)
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%sendif;[%d]\n" (tabs ()) l');
            );
          if(!log_domains) then
            (
              print_domain dom_true "After true" "";
              print_domain dom_false "After false" "";
              print_domain (D.join dom_true dom_false) "Join" "";
            );
          let i, d = (D.join interf_t (D.join int_true int_false)), (D.join dom_true dom_false) in
          i, d
        | A.CWhile (l, l', b, c) -> if (!log_domains) then Format.printf "CWhile\n"; 
          let th_d = (enforce_label thread_domain ("_aux_"^(!threadname.(t_id))) l' nb_lab) in
          let in_domain_, out_domain, _ = D.filter th_d b in
          let in_domain = apply_all in_domain_ t_id union in
          (* print_domain in_domain_ "in_domain_ " ""; *)
          (* print_domain in_domain "in_domain " ""; *)
          (*let in_intf, out_intf = interf_t, interf_t in (* TODO : filtrer les interférences ? ça n'a aucun sens ?!*)*)
          if (ann_prog) then
            (
              analysis := !analysis ^ (Format.sprintf "%s[%d]while %s do\n" (tabs ()) l (sprint_bexpr b));
              open_indent ();
            );
          let s = ref "" in
          let i, d = fp t_id b c in_domain interf_t union ann_prog s 
              (fun int dom annp analys -> 
                 let domain_in_, _, _ = D.filter dom b in 
                 let domain_in = apply_all domain_in_ t_id union in
                 let (i, d) = (analyze c t_id (union, int) domain_in annp analys lincons nb_lab) in
                 (* print_domain dom "(anonymousfun)dom : " ""; *)
                 (* print_domain domain_in "(anonymousfun)domain_in : " ""; *)
                 (* print_domain d "(anonymousfun)d" ""; *)
                 i, d, domain_in) lincons in
          if (ann_prog) then
            (
              close_indent ();
              analysis := !analysis ^ (Format.sprintf "%s%sdone;[%d]\t%s\n" !s (tabs ()) l' (sprint_domain_only d));
            );
          let (_, r2, _) =  (D.filter d b)
          in 
          let i, d = (D.join i interf_t, D.join r2 out_domain) in
          i, d
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
(*              let domain_in, _,_ = D.filter domain boexpr in
                let i, d = analyze c t_id (glob_interf, !interfs) domain_in false (ref "") in*)
              let i, d, domain_in = calc_onestep !interfs domain false (ref "") in
(*              interfs := D.join (!interfs) i;*)
              (* TODO : et ici ? il faudrait pas refiltrer ? Ou ça serait fait avant ?*)
              increase (nb_inc-1) (D.join d domain_in)
            )
          else
            (
              if (!log_fp) then Format.fprintf Format.std_formatter "\n\tincreaseelse\n";
(*              let domain_in, _,_ = D.filter domain boexpr in
              let i, d = analyze c t_id (glob_interf, !interfs) domain_in false (ref "") in*)
              let i, d, domain_in = calc_onestep !interfs domain false (ref "") in
              (* let llincons = Lincons1.array_make (D.getenv domain_in) 1 in *)
              (* let linexpr = Linexpr1.make (D.getenv domain_in) in *)
              (* Linexpr1.set_list linexpr [(Coeff.s_of_int (-1), D.apron_of_var "y")] (Some (Coeff.s_of_int (10))); *)
              (* Lincons1.array_set llincons 0 (Lincons1.make linexpr SUPEQ); *)
              (* Lincons1.print Format.std_formatter (Lincons1.array_get llincons 0); *)
              (* print_domain d "\t d = " ""; *)
              (* print_domain domain "\t domain_in = " ""; *)
(*               let domain' = D.widen_threshold domain_in (D.join d domain_in) llincons in (* vérifier si le join sert*) *)
(*              let domain' = D.widen domain_in (D.join d domain_in) in (* vérifier si le join sert*)*)
              let domain' = D.widen_threshold domain_in (D.join d domain_in) lincons in
              (* print_domain domain' "\t res = " ""; *)
              if (!log_fp) then 
                (
                  print_domain domain' "\tDomain' = " "";
                  print_domain !interfs "\t!interfs = " "\n";
                );
(*              interfs := D.join (!interfs) i;*)
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
(*              let domain_in, _,_ = D.filter domain boexpr in
              let i, d = 
                if (nb_decr == 1 && ann_prog) then
                  analyze c t_id (glob_interf, !interfs) domain_in true analysis
                else
                  analyze c t_id (glob_interf, !interfs) domain_in false (ref "")
              in*)
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

      let search_lincons init_domain prog = 
        let rec search p acc = 
          match p with
          | A.CIf (l, l', b, ct, cf) -> 
            let _, d, _ = D.filter init_domain b in
            search ct (search cf (d::acc))
          | A.CSeq (c1, c2) -> search c2 (search c1 acc) 
          | A.CWhile (l, l', b, c) -> search c acc
          | _ -> acc
        in
        let dom_array = Array.of_list (search prog []) in
        if (Array.length dom_array) == 0 then
          Apron.Lincons1.array_make (D.get_apron_env init_domain) 0
        else
          (
            let d = ref (D.bot (D.getenv init_domain)) in
            for x = 0 to (Array.length dom_array)-1 do
              d := D.join !d dom_array.(x);
            done;
            let to_neg = D.to_lincons_array !d in
            for x = 0 to (Apron.Lincons1.array_length to_neg)-1 do
              let a = Apron.Lincons1.array_get to_neg x in
              let tmp = ref [] in
              Apron.Lincons1.iter (fun x y -> tmp := (Apron.Coeff.neg x,y)::(!tmp)) a;
              Apron.Lincons1.set_list a !tmp (Some (Apron.Coeff.neg (Apron.Lincons1.get_cst a)));
            done;
            to_neg
          )
    (** Analyse globale d'un prog multithreadé
        @param prog, vars est une thread list * des variables globales
        @return Rien ? Les erreurs plus tard ?
    *)
    let global_analysis (threads, glob_vars, bexpr_init) env (logi, logfp, logd, logg, wis, wds, winterfstep, waff) =
      log_i := logi;
      log_fp := logfp;
      log_domains := logd;
      log_global := logg;
      widening_incr_step := wis;
      widening_decr_step := wds;
      widening_interf_step := winterfstep;
      widening_interf_aff_step := waff;
      (* il va falloir créer le tableau des interférences, le tableau des domaines et voilà *)
(*      label_vars := List.map (fun x -> let A.Thread(i,_,l,_) = x in ("_aux_"^i, `Bint(false, l))) threads; *)
      newvars := List.map (fun (x,y) -> (x^"'", y)) (glob_vars);(*@(!label_vars));*)
      List.iter (fun (x, y) -> Format.fprintf Format.std_formatter "%s " x) (!newvars);
      (* ajouter les _aux_??? de type `Bint (false, log2(vmax))*)
      toremove := glob_vars;(*@(!label_vars);*)
      List.iter (fun (x, y) -> Format.fprintf Format.std_formatter "%s " x) (!toremove);
      let realthreads = Array.of_list threads in
      let realprog = Array.map (fun x -> let A.Thread (_,_,_,c) = x in c) realthreads in
      let maxlab = Array.map (fun x -> let A.Thread (_, _, l, _) = x in l) realthreads in
      threadname := Array.map (fun x -> let A.Thread(n, _, _, _) = x in n) realthreads;
      let nbprogs = Array.length realthreads in
      let interf = Array.make nbprogs (D.top env) in
      let domains = Array.make nbprogs (D.top env) in
      let idomains = Array.make nbprogs (D.top env) in
      let init_domains = Array.make nbprogs (D.top env) in
      let lincons = Array.make nbprogs (Apron.Lincons1.array_make (D.get_apron_env (D.top env)) 0) in
      (* Initialisation : on ajoute les variables à chaque domaine ? *)
      for x = 0 to nbprogs-1 do
        (* let Thread (_, vlist, _) = realthreads.(x) in *)
        let vars = glob_vars in

        (* List.iter (fun (x, y) -> match y with *)
        (*     |`Bool -> Format.printf "Bool : %s@." x *)
        (*     |`Int -> Format.printf "Int : %s@." x *)
        (*     | _ -> Format.printf "? : %s@." x) vars; *)

        (* domains.(x) <- (D.add_vars domains.(x) vars); *)
        (* idomains.(x) <- (D.add_vars ~init:false idomains.(x) vars); (\* domains after init *\) *)
        lincons.(x) <- search_lincons idomains.(x) realprog.(x);
        if (!log_domains) then
          (
            Format.printf "Detected tresholds for thread %i :" x;
            Apron.Lincons1.array_print Format.std_formatter lincons.(x);
            Format.printf "@.";
          );

        (* D.env_print Format.std_formatter (D.getenv idomains.(x)); *)
        (* Format.printf "@."; *)

        let initialized, _, _ = D.filter idomains.(x) bexpr_init in
        let initd = ref initialized in
        for i = 0 to nbprogs-1 do
          initd := (enforce_label (!initd) ("_aux_"^(!threadname.(i))) 0 maxlab.(i));
(*          Format.printf "initd, enforcing _aux_%s: %s\n" (!threadname.(i)) (sprint_domain_only (!initd));*)
        done;
        idomains.(x) <- !initd;
        init_domains.(x) <- !initd;
        (* interf.(x) <- (D.add_vars (\* ~init:false *\) interf.(x) vars); *)
        interf.(x) <- (D.add_vars (* ~init:false *) interf.(x) !newvars);
        interf.(x) <- D.bot (D.getenv interf.(x));
        (** Continuation de l'initialisation : on va chercher toutes les contraintes linéaires créées par des ifs qui pourraient servir lors de widen_threshold*)
      done;

      (** on fait une étape d'analyse, ie on analyse TOUS les threads une fois *)
      let analyze_onestep stepn = 
        let is_stable = ref true in
        if(!log_global) then Format.printf "@.Analyze_onestep, stepn = %d\n" stepn;
        let newidomains = Array.make nbprogs (D.bot (D.getenv interf.(0))) in
        for x = 0 to nbprogs-1 do
          let A.Thread (id, _, _, _) = realthreads.(x) in 
          idomains.(x) <- apply_all (init_domains.(x)) x interf;
          if(!log_global || !log_i) then Format.fprintf Format.std_formatter "@.\tAnalysing thread %d (ie %s)@." x id;
          if(!log_domains) then
            (
              print_domain domains.(x) "\tOldDomain : " "";
              print_domain interf.(x) "\tOldInterf_t :" "\n";
            );
          let s = ref "" in
          let (i, d) = analyze realprog.(x) x (interf, interf.(x)) idomains.(x) false s lincons.(x) maxlab.(x) in
          (*Format.printf "%s\n\n" !s;*)
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
        if(!log_domains) then print_result nbprogs domains interf
      done;

      Format.printf "@.Analysis completed in %d steps\n" !n;
      let s = ref "" in
      for x = 0 to nbprogs-1 do
        let A.Thread (id, _, _, _) = realthreads.(x) in 
        s := !s ^ Format.sprintf "thread %s:\nbegin\n" id;
        open_indent ();
        idomains.(x) <- apply_all (init_domains.(x)) x interf;
        if(!log_global || !log_i) then Format.fprintf Format.std_formatter "@.\tAnalysing thread %d (ie %s)@." x id;
        s := !s ^ Format.sprintf "%s%s\n" (tabs ()) (sprint_domain_only idomains.(x));
        let useless = analyze realprog.(x) x (interf, interf.(x)) (idomains.(x)) true s lincons.(x) maxlab.(x) in
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
