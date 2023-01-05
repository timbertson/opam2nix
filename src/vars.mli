val nixos_vars : unit -> OpamVariable.variable_contents OpamVariable.Full.Map.t

val path_var : ocaml_version: OpamPackage.Version.t option
	-> prefix: OpamFilename.Dir.t
	-> scope: OpamPackage.Name.t option
	-> string
	-> OpamVariable.variable_contents option

val simple_lookup : vars:OpamVariable.variable_contents OpamVariable.Full.Map.t
	-> OpamVariable.Full.t
	-> OpamVariable.variable_contents option

val implicit_package_var : OpamVariable.Full.t -> OpamPackage.Name.t option

val ocaml_name : OpamPackage.Name.t

(** Information about a pacakge. Post-solve, only name and version are known.
	Path is only known at build time (i.e. during Invoke) *)
type selected_package = {
	name: OpamPackage.Name.t;
	sel_version: OpamPackage.Version.t option;
	path: OpamFilename.Dir.t option;
}

val selected_package : ?version:OpamPackage.Version.t -> ?path:OpamFilename.Dir.t -> OpamPackage.Name.t -> selected_package

(** Pre-solve, only `ocaml` package is present, everything else comes from `vars *)
type state = {
	st_packages: selected_package OpamPackage.Name.Map.t;
	st_vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
	ocaml_version: OpamPackage.Version.t option;

	(** Only set to true from invoke, otherwise unix user & group will not be undefined *)
	is_building: bool;

	(** false simply suppressed unresolved warnings *)
	st_partial: bool; 
}

val lookup : state -> self:OpamPackage.Name.t option -> OpamVariable.Full.t -> OpamVariable.variable_contents option

val state : is_building:bool -> ?partial:bool -> selected_package OpamPackage.Name.Map.t -> state

val string_of_selected_package : selected_package -> string
