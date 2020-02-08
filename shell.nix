let base = (import ./default.nix {}); in
base.overrideAttrs (attrs: {
	buildInputs = attrs.buildInputs ++ base.devInputs;
})
