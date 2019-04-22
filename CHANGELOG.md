0.5:

* Migrated to format version 3. Format changes are specific to the `opamEnv` JSON environment variable:
  * each non-pseudo package in `packages` is now an attrset of `{ path; version; }` instead of just a string. Version is null if the underlying derivation has no `version` attribute.
  * the `version` of the current package is passed alongside well as `name`
  * `ocamlVersion` is no longer specified at build time (although --ocaml-version is still used at selection time). At build time, ocaml's version is taken from the `ocaml` attribute of `packages`.

