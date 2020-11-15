(* This module is a copy/pasted subset of upstream opamAction.ml,
to avoid a full dependency on opam-client *)

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log ?level fmt = OpamConsole.log ?level "ACTION" fmt
let slog = OpamConsole.slog

open OpamTypes
open OpamFilename.Op
open OpamProcess.Job.Op

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build env opam nv dir =
  let patches = OpamFile.OPAM.patches opam in

  let rec iter_patches f = function
    | [] -> Done []
    | (patchname,filter)::rest ->
      if OpamFilter.opt_eval_to_bool env filter
      then
        OpamFilename.patch (dir // OpamFilename.Base.to_string patchname) dir
        @@+ function
        | None -> iter_patches f rest
        | Some err ->
          iter_patches f rest @@| fun e -> (patchname, err) :: e
      else iter_patches f rest
  in
  let print_apply basename =
    log "%s: applying %s.\n" (OpamPackage.name_to_string nv)
      (OpamFilename.Base.to_string basename);
    if OpamConsole.verbose () then
      OpamConsole.msg "[%s: patch] applying %s\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv))
        (OpamFilename.Base.to_string basename)
  in

  if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.fake) then
    iter_patches print_apply patches @@| fun _ -> None
  else

  let subst_patches, subst_others =
    List.partition (fun f -> List.mem_assoc f patches)
      (OpamFile.OPAM.substs opam)
  in
  let subst_errs =
    OpamFilename.in_dir dir  @@ fun () ->
    List.fold_left (fun errs f ->
        try
          OpamFilter.expand_interpolations_in_file env f;
          errs
        with e -> (f, e)::errs)
      [] subst_patches
  in

  (* Apply the patches *)
  let text =
    OpamProcess.make_command_text (OpamPackage.Name.to_string nv.name) "patch"
  in

  OpamProcess.Job.with_text text @@
  iter_patches (fun base ->
      let patch = dir // OpamFilename.Base.to_string base in
      print_apply base;
      OpamFilename.patch patch dir)
    patches
  @@+ fun patching_errors ->

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  let subst_errs =
    OpamFilename.in_dir dir @@ fun () ->
    List.fold_left (fun errs f ->
        try
          OpamFilter.expand_interpolations_in_file env f;
          errs
        with e -> (f, e)::errs)
      subst_errs subst_others
  in
  if patching_errors <> [] || subst_errs <> [] then
    let msg =
      (if patching_errors <> [] then
         Printf.sprintf "These patches didn't apply at %s:\n%s"
           (OpamFilename.Dir.to_string dir)
           (OpamStd.Format.itemize
              (fun (f,err) ->
                 Printf.sprintf "%s: %s"
                   (OpamFilename.Base.to_string f) (Printexc.to_string err))
              patching_errors)
       else "") ^
      (if subst_errs <> [] then
         Printf.sprintf "String expansion failed for these files:\n%s"
           (OpamStd.Format.itemize
              (fun (b,err) ->
                 Printf.sprintf "%s.in: %s" (OpamFilename.Base.to_string b)
                   (Printexc.to_string err))
           subst_errs)
       else "")
    in
    Done (Some (Failure msg))
  else
    Done None
