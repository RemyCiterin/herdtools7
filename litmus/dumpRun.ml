(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(*********************************)
(* Dump or run a series of tests *)
(*********************************)

open Answer
open Printf

module type Config = sig
  val carch : Archs.System.t
  val platform : string
  val makevar : string list
  val gcc : string
  val stdio : bool
  val index : string option
  val crossrun : Crossrun.t
  val adbdir : string
  val sleep : int
  val tarname : string
  val driver : Driver.t
  val cross : bool
  val hexa : bool
  val threadstyle : ThreadStyle.t
  val asmcommentaslabel : bool
  include RunUtils.CommonConfig
  val mkopt : Option.opt -> Option.opt
  val variant : Variant_litmus.t -> bool
  val nocatch : bool
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
end

module type OneTest = sig
  val from_file :
      StringSet.t -> hash_env -> string -> out_channel -> answer
end


module Make (Cfg:Config) (Tar:Tar.S) (CT:OneTest) : sig
  val from_files : string list -> unit
end = struct

  module Param = SkelUtil.Param(Cfg)

  let is_kvm =
    match Cfg.mode with
    | Mode.Kvm -> true
    | Mode.PreSi|Mode.Std -> false

  open MySys

  module GD = GenerateCrossDoc.Make(Cfg)

  let report_failure name msg chan =
    if not Cfg.is_out then begin
      let title = sprintf "!!! Test %s failed !!!" name in
      let warn = String.make (String.length title) '!' in
      output_line chan  warn ;
      output_line chan  title ;
      output_line chan  warn ;
      fprintf chan "Reported: %s\n" msg ;
      output_line chan ""
    end

  module type ArchConf = sig
    val word : Word.t
    val delay : int
    val gccopts :  string
  end

  let get_arch arch =
    let opt = Option.get_default arch in
    let opt = Cfg.mkopt opt in
    let module M = struct
      let word = Option.get_word opt
      let gccopts = Option.get_gccopts opt
      let delay = Option.get_delay opt
    end in
    (module M : ArchConf)

(* Makefile utilities *)
  let do_self = Cfg.variant Variant_litmus.Self

  let makefile_vars chan infile arch pac sources =
    let module O = struct
      include Cfg
      include (val (get_arch arch) : ArchConf)
    end in
    let module RU = RunUtils.Make(O) in
    List.iter
      (fun line -> fprintf chan "%s\n" line)
      Cfg.makevar ;
    let gcc_opts =
      if do_self then LexO.tr RU.get_gcc_opts
      else RU.get_gcc_opts in
    begin
      match Cfg.mode with
      | Mode.Std|Mode.PreSi ->
          fprintf chan "GCC=%s\n" Cfg.gcc ;
      | Mode.Kvm -> ()
    end ;
    fprintf chan "GCCOPTS=%s\n" gcc_opts ;
    let link_opts = RU.get_link_opts in
    fprintf chan "LINKOPTS=%s\n" link_opts ;
    if infile then
      fprintf chan "SRC := $(shell cat src)\n"
    else begin
      fprintf chan "SRC=\\\n" ;
      List.iter
        (fun src -> fprintf chan " %s\\\n" src)
        (List.rev sources) ;
      fprintf chan "\n"
    end ;
    begin
      match Cfg.mode with
      | Mode.Std|Mode.PreSi -> ()
      | Mode.Kvm ->
         let utils =
           ["litmus_rand.o"; "utils.o"; "kvm_timeofday.o";] in
         let utils =
           if Cfg.stdio then utils
           else
             "platform_io.o" :: "litmus_io.o" :: utils in
         let utils =
           if pac then
             "auth.o"::utils
           else utils in
         fprintf chan "UTILS=%s\n"
           (String.concat " " utils)
    end ;
    ()

  let makefile_clean chan extra =
    fprintf chan "clean:\n" ;
    fprintf chan "\t/bin/rm -f *.o *.s *.t *%s *~%s\n"
      (Mode.exe Cfg.mode) extra ;
    fprintf chan "\n" ;
    fprintf chan "cleansource:\n" ;
    fprintf chan "\t/bin/rm -f *.o *.c *.h *.s *~\n" ;
    fprintf chan "\n" ;
    ()

  let makefile_utils chan utils =
    let utils =
      List.fold_right
        (fun s k ->
          if Filename.check_suffix s ".c" then
            let b = Filename.chop_suffix (Filename.basename s) ".c" in
            b :: k
          else k) utils [] in
    List.iter
      (fun u ->
        let src = u ^ ".c" and obj = u ^ ".o" in
        fprintf chan "%s: %s\n" obj src ;
        fprintf chan "\t$(GCC) $(GCCOPTS) %s-O2 -c %s\n"
          (if
            TargetOS.is_freebsd Cfg.targetos &&
            u = "affinity"
          then "-D _FREEBSD_AFFINITY "
          else "")
          src ;
        fprintf chan "\n")
      utils ;
(* UTIL objs *)
    let utils_objs =
      String.concat " " (List.map (fun s -> s ^ ".o") utils) in
    fprintf chan "UTILS=%s\n\n" utils_objs ;
    ()
;;

(* Compile (and run in command line mode) tests *)
let run_tests names out_chan =
  let exp = match Cfg.index with
  | None -> None
  | Some exp -> Some (open_out exp) in
  let  arch,docs,sources,_,_,nts,some_pac =
    Misc.fold_argv_or_stdin
      (fun name (a0,docs,srcs,cycles,hash_env,nts,some_pac) ->
        let ans =
          try CT.from_file cycles hash_env name out_chan
          with
          | e ->
              if Cfg.nocatch then raise e ;
              Interrupted (a0,e) in
        match ans with
        | Completed (a,doc,src,cycles,hash_env,nt,pac) ->
            begin match exp with
            | None -> ()
            | Some exp -> fprintf exp "%s\n" name
            end ;
            a,(doc::docs),(src::srcs),cycles,hash_env,
            IntSet.add nt nts,(pac || some_pac)
        | Absent a -> a,docs,srcs,cycles,hash_env,nts,some_pac
        | Interrupted (a,e) ->
            let msg =  match e with
            | Misc.Exit -> "None"
            | Misc.Fatal msg
            | Misc.UserError msg ->
                eprintf "%a %s\n%!" Pos.pp_pos0 name msg ;
                msg
            | e ->
                if Cfg.nocatch then raise e ;
                let msg = sprintf "exception %s"  (Printexc.to_string e) in
                eprintf "%a %s\n%!" Pos.pp_pos0 name msg ;
                msg in
            report_failure name msg out_chan ;
            a,docs,srcs,cycles,hash_env,nts,some_pac)
      names (`X86,[],[],StringSet.empty,StringMap.empty,IntSet.empty,false) in
  begin match exp with
  | None -> ()
  | Some exp -> close_out exp
  end ;
  let utils =
    let module O = struct
      include Cfg
      let cached =
        match Cfg.threadstyle with ThreadStyle.Cached -> true | _ -> false
      let arch = arch
      let sysarch  = Archs.get_sysarch arch Cfg.carch
    end in
    let module Obj = ObjUtil.Make(O)(Tar) in
    Obj.dump some_pac in
  arch,docs,sources,utils,nts,some_pac

(* Run tests (command line mode) *)
let dump_command names =
  let out_chan = stdout in
  let arch,_,_,utils,_,_ = run_tests names out_chan in
  let module O = struct
      include Cfg
    include (val (get_arch arch) : ArchConf)
  end in
  let module RU = RunUtils.Make(O) in
  RU.report_machine out_chan ;
  List.iter MySys.remove utils ;
  flush out_chan ;
  ()

let dump_shell_prefix out_chan =
  output_line out_chan "#!/bin/sh\n" ;
  output_line out_chan "date" ;
  output_line out_chan "LITMUSOPTS=\"${@:-$LITMUSOPTS}\"" ;
  ()

let dump_shell_postfix out_chan =
  output_line out_chan "echo \"LITMUSOPTS=$LITMUSOPTS\"" ;
  output_line out_chan "date" ;
  ()

let dump_shell_kvm_dorun out_chan e =
  fprintf out_chan "TDIR=$(dirname $0)\n" ;
  fprintf out_chan "KVM_RUN=\"${KVM_RUN:-%s}\"\n" e ;
  fprintf out_chan "dorun () {\n" ;
  fprintf out_chan "  EXE=$1\n" ;
  fprintf out_chan "  shift\n" ;
  fprintf out_chan "  OPTS=\"$@\"\n" ;
  fprintf out_chan "  ${KVM_RUN} ${TDIR}/${EXE} -smp %i -append \"${OPTS}\"\n"
    (match Cfg.avail with
     | Some e -> e
     | None ->
        Warn.user_error "Available threads must be set in kvm mode") ;
  fprintf out_chan "}\n"

let dump_c_shell_kvm e =
  Misc.output_protect
    (fun out_chan ->
      dump_shell_prefix out_chan ;
      dump_shell_kvm_dorun out_chan e ;
      fprintf out_chan "dorun ./run.flat -q ${LITMUSOPTS}\n" ;
      dump_shell_postfix out_chan)
    (Tar.outname (MyName.outname "run" ".sh"))

let dump_shell names =
  Misc.output_protect
    (fun out_chan ->
      dump_shell_prefix out_chan ;
      begin match Cfg.crossrun with
      | Crossrun.No -> ()
      | Crossrun.Qemu e ->
          fprintf out_chan "QEMU=\"${QEMU:-%s}\"\n" e
      | Crossrun.Kvm e ->
         dump_shell_kvm_dorun out_chan e
      | Crossrun.Adb  ->
          fprintf out_chan "RDIR=%s\n" Cfg.adbdir ;
          fprintf out_chan "adb shell mkdir $RDIR >/dev/null 2>&1\n" ;
          fprintf out_chan "dorun () {\n" ;
          fprintf out_chan "  EXE=$1\n" ;
          fprintf out_chan "  shift\n" ;
          fprintf out_chan "  OPTS=\"$@\"\n" ;
          fprintf out_chan "  BASE=$(basename $EXE)\n" ;
          fprintf out_chan "  adb push $EXE $RDIR >/dev/null 2>&1\n" ;
          fprintf out_chan "  adb shell $RDIR/$BASE $OPTS | tr -d '\\r' \n" ;
          fprintf out_chan "  adb shell rm $RDIR/$BASE >/dev/null 2>&1\n" ;
          fprintf out_chan "}\n" ;
      | Crossrun.Host h ->
          fprintf out_chan "RHOST=%s\n" h.Crossrun.host ;
          fprintf out_chan "RPORT=%i\n"
            (match h.Crossrun.port with| None -> 22| Some p -> p) ;
          fprintf out_chan "doscp () {\n" ;
          fprintf out_chan "  EXE=$1\n" ;
          fprintf out_chan "  scp -q -P $RPORT $EXE $RHOST: 2> /dev/null\n" ;
          fprintf out_chan "  while [ $? -ne 0 ]\n" ;
          fprintf out_chan "  do\n" ;
          fprintf out_chan "    sleep 30\n" ;
          fprintf out_chan "    scp -q -P $RPORT $EXE $RHOST: 2> /dev/null\n" ;
          fprintf out_chan "  done\n" ;
          fprintf out_chan "  true\n" ;
          fprintf out_chan "}\n" ;
          ()
      end ;
      let sleep = Cfg.sleep in
      if sleep >= 0 then fprintf out_chan "SLEEP=%i\n" sleep ;
      let arch,_,sources,utils,_,pac = run_tests names out_chan in

      let module O = struct
        include Cfg
        include (val (get_arch arch) : ArchConf)
      end in
      let module RU = RunUtils.Make(O) in
      RU.report_machine out_chan ;
      if not is_kvm then begin
        output_line out_chan "sed '2q;d' comp.sh"
       end ;
      dump_shell_postfix out_chan ;
      arch,sources,utils,pac)
    (Tar.outname (MyName.outname "run" ".sh"))

let dump_shell_cont arch pac sources utils =
  let sources = List.map Filename.basename  sources in
(* Shell script for sequential compilation *)
  let module O = struct
    include Cfg
    include (val (get_arch arch) : ArchConf)
  end in
  begin match Cfg.mode with
  | Mode.Std|Mode.PreSi ->
      let module RU = RunUtils.Make(O) in
      Misc.output_protect
        (fun chan ->
          fprintf chan "#!/bin/sh\n" ;
          let gcc_opts = RU.get_gcc_opts in
          fprintf chan "GCC=%s\n" Cfg.gcc ;
          fprintf chan "GCCOPTS=\"%s\"\n" gcc_opts ;
          let link_opts = RU.get_link_opts  in
          fprintf chan "LINKOPTS=\"%s\"\n" link_opts ;
          fprintf chan "/bin/rm -f *%s *.s\n" (Mode.exe Cfg.mode) ;
          List.iter
            (fun s ->
              if Filename.check_suffix s ".c" then
                fprintf chan "$GCC $GCCOPTS -O2 -c %s\n"
                  (Filename.basename s))
            utils ;
          let utils_objs =
            List.fold_right
              (fun s k ->
                if Filename.check_suffix s ".c" then
                  let b = Filename.chop_suffix (Filename.basename s) ".c" in
                  (b ^ ".o") :: k
                else k) utils [] in
          let utils_objs = String.concat " " utils_objs in
          List.iter
            (fun src ->
              let exe = Filename.chop_extension src ^ (Mode.exe Cfg.mode) in
(* No more moderate parallelism [blocks on abducens, sh bug ?] *)
              fprintf chan
                "$GCC $GCCOPTS $LINKOPTS -o %s %s %s\n" exe utils_objs src ;
              let srcS = Filename.chop_extension src ^ ".s" in
              let srcT = Filename.chop_extension src ^ ".t" in
              fprintf chan "$GCC $GCCOPTS -S %s && awk -f show.awk %s > %s && /bin/rm %s\n"
                src srcS srcT srcS ;
              ())
            (List.rev sources))
        (Tar.outname (MyName.outname "comp" ".sh")) ;
  | Mode.Kvm -> ()
  end ;
(* Add a small README file *)
  Misc.output_protect
    (fun chan ->
      GD.gen_readme_src chan arch sources)
    (Tar.outname (MyName.outname "README" ".txt")) ;
(* Makefile for parallel compilation *)
  Misc.output_protect
    (fun chan ->
(* Variables *)
      makefile_vars chan false arch pac sources ;
      fprintf chan "EXE=$(SRC:.c=%s)\n"
        (match Cfg.mode with
        | Mode.Std|Mode.PreSi -> ".exe"
        | Mode.Kvm -> ".flat") ;
      fprintf chan "T=$(SRC:.c=.t)\n" ;
      fprintf chan "\n" ;
(* Entry points *)
      fprintf chan "all: $(EXE) $(T)\n" ;
      fprintf chan "\n" ;
      begin match Cfg.mode with
      | Mode.Std|Mode.PreSi ->
          makefile_clean chan "";
          makefile_utils chan utils ;
          ()
      | Mode.Kvm -> ()
      end ;
      begin match  Cfg.mode with
      | Mode.Std|Mode.PreSi ->
(* Cannot use %.s here for Mac that preprocesses assembly files,
   and because #comments makes the preprocessor crash ! *)
          let src_ext = match Cfg.targetos with
          | TargetOS.Mac -> 'c'
          | TargetOS.Linux|TargetOS.AIX
          | TargetOS.FreeBsd|TargetOS.Android8
            -> 's' in
          fprintf chan "%%.exe:%%.%c $(UTILS)\n" src_ext ;
          fprintf chan
            "\t$(GCC) $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) $<\n" ;
          fprintf chan "\n" ;
(* .s pattern rule *)
          fprintf chan "%%.s:%%.c\n" ;
          fprintf chan "\t$(GCC) $(GCCOPTS) -S $<\n" ;
          fprintf chan "\n" ;
(* .t pattern rule *)
          fprintf chan "%%.t:%%.s\n" ;
          fprintf chan "\tawk -f show.awk $< > $@\n" ;
          fprintf chan "\n"
      | Mode.Kvm ->
          let module A = struct
            let sysarch = Archs.get_sysarch arch Cfg.carch
          end in
          let module Insert =  ObjUtil.Insert(A) in
          Insert.insert (fprintf chan "%s\n") "kvm.rules"
      end)
    (Tar.outname (MyName.outname "Makefile" "")) ;
  Tar.tar  () ;
  ()

let dump_c xcode names =
  Misc.output_protect
    (fun out_chan ->
      let module O = Indent.Make(struct let hexa = Cfg.hexa let out = out_chan end) in
      O.o "#include <stdlib.h>" ;
      begin
        match Cfg.mode with
        | Mode.Std|Mode.PreSi ->
           if Cfg.stdio then
             O.o "#include <stdio.h>"
           else
             O.o "#include \"litmus_io.h\"" ;
           if Cfg.sleep > 0 then  O.o "#include <unistd.h>"
        | Mode.Kvm ->
           O.o "#include \"kvm-headers.h\"" ;
           O.o "#include \"utils.h\"" ;
           if Cfg.sleep > 0 then O.o "#include <asm/delay.h>" ;
           if not Cfg.stdio then O.o "#include \"litmus_io.h\"" ;
      end ;
      begin match Cfg.threadstyle with
      | ThreadStyle.Cached -> O.o "extern void set_pool(void);"
      | _ -> ()
      end ;
      O.o "" ;
      O.o "/* Declarations of tests entry points */" ;
      let arch,docs,srcs,utils,nts,pac =
        run_tests names out_chan in
      let module C = struct
        include Cfg
        include (val (get_arch arch) : ArchConf)
      end in
      let module RU = RunUtils.Make(C) in
      O.o "" ;
      if not is_kvm then begin
        O.o "/* Date function */" ;
        O.o "#include <time.h>" ;
        O.o "static void my_date(FILE *out) {" ;
        O.oi "time_t t = time(NULL);" ;
        if Cfg.stdio then
          O.oi "fprintf(out,\"%s\",ctime(&t));"
        else
          O.oi "emit_string(out,ctime(&t));" ;
        O.o "}" ;
        ()
      end ;
      O.o "" ;
      O.o "/* Postlude */" ;
      O.o "static void end_report(int argc,char **argv,FILE *out) {" ;
      let dstring =
        if Cfg.stdio then
          fun s -> O.fi "fprintf(out,\"%%s\\n\",\"%s\");" (String.escaped s)
        else
          fun s -> O.fi "emit_string(out,\"%s\\n\");" (String.escaped s) in
      RU.report_parameters dstring ;
      O.o "/* Command line options */" ;
      let dstring =
        if Cfg.stdio then
          fun s -> O.fi "fprintf(out,\"%%s\",\"%s\");" (String.escaped s)
        else
          fun s -> O.fi "emit_string(out,\"%s\");" (String.escaped s) in
      dstring "Command:" ;
      O.oi "for ( ; *argv ; argv++) {" ;
      if Cfg.stdio then begin
        O.oii "fprintf(out,\" %s\",*argv);"
      end else begin
        O.oii "emit_string(out,\" \");" ;
        O.oii "emit_string(out,*argv);"
      end ;
      O.oi "}" ;
      dstring "\n" ;
      O.o "}" ;
      O.o"" ;
      O.o"/* Run all tests */" ;
      let runbody () =
        if not is_kvm then O.oi "my_date(out);" ;
        begin match Cfg.threadstyle with
        | ThreadStyle.Cached -> O.oi "set_pool();"
        | _ -> ()
        end ;
        List.iteri
          (fun k doc ->
            if k > 0 && Cfg.sleep > 0 then begin
              match Cfg.mode with
              | Mode.Std|Mode.PreSi ->
                 O.fi "sleep(%i);" Cfg.sleep
              | Mode.Kvm ->
                 O.fi "mdelay(%i*1000);" Cfg.sleep
              end ;
            O.fi "%s(argc,argv,out);" (MyName.as_symbol doc) ;
            if xcode then O.oi "[tick tick];")
          (List.rev docs) ;
        O.oi "end_report(argc,argv,out);" ;
        if not is_kvm then O.oi "my_date(out);" in
      if xcode then begin
        O.o "#import \"run.h\"";
        O.o "" ;
        O.o "@implementation Run" ;
        O.o "+ (void) runWithArgc:(int) argc argv: (char **) argv out: (FILE *) out tick: (id <Ticker>)tick {" ;
        runbody() ;
        O.o "}"
      end else begin
        O.o "static void run(int argc,char **argv,FILE *out) {" ;
        runbody();
        O.o "}"
      end ;
      O.o"" ;
      if xcode then begin
        O.o "+ (int) getNTests {" ;
        O.fi "return %i;" (List.length docs) ;
        O.o "}" ;
        O.o "" ;
        O.o "@end"
      end else  begin
        O.o"int main(int argc,char **argv) {" ;
        if is_kvm then O.oi "litmus_init();" ;
        O.oi "run(argc,argv,stdout);" ;
        O.oi "return 0;" ;
        O.o"}"
      end ;
      arch,srcs,utils,nts,pac)
    (Tar.outname (MyName.outname "run" (if xcode then ".m" else ".c")))


let dump_c_cont xcode arch pac sources utils nts =
  let nts = IntSet.filter Param.mk_dsa nts in
  let shared_topology =  not (xcode || IntSet.is_empty nts) in
  let sources = List.map Filename.basename  sources in
  let utils =
    if shared_topology then utils@[Tar.outname "topology.c"]
    else utils in
(* Makefile *)
  let infile = not xcode in
  Misc.output_protect
    (fun chan ->
      makefile_vars chan infile arch pac sources ;
(* Various intermediate targets *)
      fprintf chan "T=$(SRC:.c=.t)\n" ;
      fprintf chan "H=$(SRC:.c=.h)\n" ;
      if not xcode then begin
        fprintf chan "OBJ=$(SRC:.c=.o)%s\n"
          (if shared_topology then " topology.o" else "") ;
        fprintf chan "EXE=run.%s\n" (if is_kvm then "flat" else "exe") ;
        fprintf chan "\n" ;
      end ;
(* Entry point *)
      if xcode then begin
        fprintf chan "all: $(H)\n" ;
      end else begin
        fprintf chan "all: $(EXE)\n" ;
      end ;
      fprintf chan "\n" ;
      if is_kvm then begin
          let module A = struct
            let sysarch = Archs.get_sysarch arch Cfg.carch
          end in
          let module Insert =  ObjUtil.Insert(A) in
          Insert.insert (fprintf chan "%s\n") "kvm.c.rules"
      end else begin
        makefile_clean chan ((if infile then " obj " else " ")^"$(H)");
        if not xcode then makefile_utils chan utils ;
(* Rules *)
        if not xcode then begin
          if infile then begin
            fprintf chan "obj: $(OBJ) src\n" ;
            fprintf chan "\tsed -e 's|.c$$|.o|g' < src > obj\n\n"
          end ;
          let o1 =
            if infile then "$(UTILS) obj run.o"
            else "$(UTILS) $(OBJ) run.o" in
          let o2 =
            if infile then "$(UTILS) @obj run.o"
            else o1 in
          fprintf chan "$(EXE): %s\n" o1 ;
          fprintf chan "\t$(GCC)  $(GCCOPTS) $(LINKOPTS) -o $@ %s\n" o2 ;
          fprintf chan "\n" ;
          (* .o pattern rule *)
          fprintf chan "%%.o:%%.c\n" ;
          fprintf chan
            "\t$(GCC) $(GCCOPTS) $(LINKOPTS) -c -o $@ $<\n" ;
          fprintf chan "\n"
        end ;
        (* .s pattern rule *)
        fprintf chan "%%.s:%%.c\n" ;
        fprintf chan "\t$(GCC) -DASS $(GCCOPTS) -S $<\n" ;
        fprintf chan "\n" ;
        (* .t pattern rule *)
        fprintf chan "%%.t:%%.s\n" ;
        fprintf chan "\tawk -f show.awk $< > $@\n" ;
        fprintf chan "\n" ;
        (* .h pattern rule *)
        fprintf chan "%%.h:%%.t\n" ;
        fprintf chan "\tsh toh.sh $< > $@\n" ;
        fprintf chan "\n"
        end ;
      (* Dependencies *)
        if not xcode then begin
          fprintf chan "\n" ;
          List.iter
            (fun src ->
              let base = Filename.chop_extension src in
              fprintf chan "%s.o: %s.h %s.c\n" base base base)
            sources
        end)
    (Tar.outname (MyName.outname "Makefile" "")) ;
(* Source list in file  *)
  if infile then begin
    Misc.output_protect
      (fun chan -> List.iter (fprintf chan "%s\n") sources)
      (Tar.outname (MyName.outname "src" ""))
  end ;
(* XCode interface file *)
  if xcode then begin
    Misc.output_protect
      (fun chan ->
        let module O = Indent.Make(struct let hexa = Cfg.hexa let out = chan end) in
        O.o "#import <Foundation/Foundation.h>" ;
        O.o "#import \"ticker.h\"" ;
        O.o "" ;
        O.o "@interface Run : NSObject" ;
        O.o
          "+ (void) runWithArgc:(int) argc argv: (char **) argv out: (FILE *) out tick: (id <Ticker>)tick ;" ;
        O.o "+ (int) getNTests;" ;
        O.o "@end")
       (Tar.outname (MyName.outname "run" ".h")) ;
  end ;
  (* Topology arrays, when shared *)
  if shared_topology then begin
    let m =
      Misc.output_protect
        (fun chan ->
          let module O =
            Indent.Make(struct let hexa = Cfg.hexa let out = chan end) in
          O.f "/* Topologies for threads in {%s} */"
            (IntSet.pp_str "," (sprintf "%i") nts) ;
          IntSet.fold
            (fun k m ->
              let module K = struct
                let verbose = Cfg.verbose
                let file_name = "topology.c"
                let nthreads = k
                let avail = match Cfg.avail with None -> 0 | Some a -> a
                let do_affinity = Cfg.affinity <> Affinity.No
                let smt = Cfg.smt
                let nsockets = Cfg.nsockets
                let smtmode = Cfg.smtmode
                let mode = Cfg.mode
                let is_active = false
                let inlined = false
              end in
              let module Topo = Topology.Make(K)(O) in
              O.o "" ;
              O.o "/***************/" ;
              O.f "/* nthreads=%d */" k ;
              O.o "/***************/" ;
              O.o "" ;
              let sz = Topo.dump_alloc [] in
              IntMap.add k sz m)
            nts IntMap.empty)
        (Tar.outname (MyName.outname "topology" ".c")) in
      Misc.output_protect
        (fun chan ->
          let module O =
            Indent.Make(struct let hexa = Cfg.hexa let out = chan end) in
          O.f "/* Topologies for threads in {%s} */"
            (IntSet.pp_str "," (sprintf "%i") nts) ;
          IntMap.iter
            (fun k sz ->
              let open Topology in
              O.o "" ;
              let open Mode in
              begin match Cfg.mode with
              | PreSi | Kvm ->
                 O.f "extern const int *inst_%d;" k ;
                 O.f "extern const int *role_%d;" k
              | Std ->
                 O.f "extern const int *cpu_scan_%d;" k
              end ;
              O.f "extern const char **group_%d;" k ;
              O.f "#define scansz_%d %d" k sz.scansz ;
              O.f "#define scanline_%d %d" k sz.scanline)
            m)
        (Tar.outname (MyName.outname "topology" ".h")) ;
      ()
  end  ;
  Tar.tar  () ;
  ()

let dump_cross _arch =
  (* Now build new directory structure *)
  let top = Filename.temp_file "dir" ".tmp" in
  MySys.mkdir top ;
  let dname = "litmus_tests" in
  let dir = Filename.concat top dname in
  MySys.mkdir dir ;
  (* Put this in dir *)
  Misc.output_protect
    (fun chan -> output_line chan (GD.gen_makefile ()))
    (Filename.concat dir "Makefile") ;
  Misc.output_protect
    (fun chan -> GD.gen_readme chan)
    (Filename.concat dir "README.txt") ;
(* Untar sources in src sub-directory *)
  if Tar.is_archive then begin
    let src = Filename.concat dir "src" in
    MySys.mkdir src ;
    let cmd =
      let tf = Filename.basename Cfg.tarname in
      sprintf "mv %s %s && cd %s && tar x%sf %s && /bin/rm -f %s"
        Cfg.tarname src src (Tar.tarz ()) tf tf in
    MySys.exec_stdout cmd ;
    Tar.tar_dir dir
  end else begin
    let tgt = Cfg.tarname in
    let tmp = sprintf "%s.tmp" tgt in
    let src = Filename.concat tgt "src" in
    let com =
      sprintf
        "mv %s %s && mv %s %s && mv %s %s && /bin/rm -rf %s"
        tgt tmp dir tgt tmp src top in
    MySys.exec_stdout com
  end

let from_files =
  if not Cfg.is_out then begin
    dump_command
  end else
    fun names ->
      let arch =
        match Cfg.driver with
        | Driver.Shell ->
            let arch,sources,utils,pac = dump_shell names in
            dump_shell_cont arch pac sources utils ;
            arch
        | Driver.C|Driver.XCode as d ->
            let xcode = match d with
            | Driver.XCode -> true
            | _ -> false in
            let arch,sources,utils,nts,pac = dump_c xcode names in
            begin match Cfg.crossrun with
            | Crossrun.Kvm e ->
               dump_c_shell_kvm e
            | _ ->  ()
            end ;
            dump_c_cont xcode arch pac sources utils nts ;
            arch in
      if Cfg.cross then dump_cross arch
end
