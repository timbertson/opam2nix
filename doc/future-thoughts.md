# My thoughts on the future direction of opam2nix:

Opam2nix takes on a lot of functionality, but some of it is not nix specific, and some of it is not opam specific.

Broadly, there are 4 steps in most `something2nix` projects:

1. solving dependencies and producing a lockfile containing a map of chosen (pkg, version, dependencies)
2. reading the lockfile, resolving against a repository to extract source information, checksums and build instructions (i.e. `dune build -p %{name}%` in the case of most ocaml software ;))
3. generating nix expressions for each package based on the above information
4. the nix API that takes this lockfile and a bunch of user inputs (overrides, etc) and produces full derivations

In most package managers, (1) is part of the package manager itself.
(2, 3 and 4) can involve plenty of ecosystem-specific logic, but at the same time they can all benefit from shared code. All similar projects need to do things like maintaining caches of remote git checkouts, calculate nix hashes, and generate nix expressions from a tree of package specifications.

To explore this, I started working on [fetlock](https://github.com/timbertson/fetlock).

My long term hope is that (1) becomes the responsibility of opam. It's the most complex part of opam2nix, yet there's nothing nix specific about it.

### What about opam lock?

It's worth pointing out why `opam-lock` unfortunately isn't fit for this purpose:

 - it doesn't actually run a solve, it just captures the currently installed versions. This means it isn't consistent across machines, and you can't get a lockfile without first installing everything. Most other package managers have a way to just run a solve and produce a lockfile without touching the system. Running a system-independent solve is a job well suited to `opam-0install-solver`, which is why opam2nix uses it.

 - the output is a modified `opam` file, with strict versioning constraints. This is useful for opam, but it's a very complex file format. Exposing this via json / yaml / sexp etc would make it much more usable. Even a custom opam-like file format would be easier, since this format wouldn't need to support filter expressions, string interpolation, etc.

 - it doesn't expose dependency information. It pins the versions of all transitive packages, but you still need to load package specifications and parse dependency information to figure out the dependency tree. It also makes it look like the root package depends on all transitive dependencies, which is undesirable.

For opam2nix's case, what we need is:

 - root: requested package name(s)
 - selections, a list of:
   - package name
   - version
   - dependencies (names of direct dependencies which are included in selections)
     Need some way to flag build / test only deps

If we had this, opam2nix would still need to:
 - lookup each package in the repository(s)
 - load urls / checksums
 - load `opam` files and extract build commands / depexts.

There's still a fair bit of complexity in loading and evaluating `opam` files, so if there were some way to include this in the lockfile that'd be great. But that's potentially quite noisy for opam users, and you can't collapse the `build` expression down to simple lists of strings because many variables aren't known at lock time.

Given that we _do_ still need to evaluate opam files, leaving `dependencies` out of the lockfile probably isn't that big a deal, since evaluating those is easier than evaluating build commands.
