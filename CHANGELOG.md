2020-09-06:

 - The builtin opam solver has been replaced with the 0install-solver. It runs faster and requires less dependencies (dropping mccs / dose).
 - commandline arguments only support exact version pinning (e.g. `lwt=1..3`), but not other comparisons (`>=`, etc)