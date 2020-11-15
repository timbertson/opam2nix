(* Minimal fake to satisfy opamAction *)
type state = {
  fake: bool;
}

let r = ref { fake = false }
