{ prsJSON, nixpkgs }:
let
  pkgs = import nixpkgs {};
  prs = builtins.fromJSON (builtins.readFile prsJSON);
  jobsets = pkgs.lib.mapAttrs (num: info: {
    name = "leaves";
    value = {
      enabled = 1;
      hidden = false;
      description = "PR ${num}: ${info.title}";
      nixexprinput = "src";
      nixexprpath = "release.nix";
      checkinterval = 30;
      schedulingshares = 20;
      enableemail = false;
      emailoverride = "";
      keepnr = 3;
      inputs = {
        src = {
          type = "git";
          value = "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";
          emailresponsible = false;
        };
        nixpkgs = {
          type = "git";
          value = "https://github.com/nixos/nixpkgs-channels.git nixos-unstable-small";
          emailresponsible = false;
        };
      };
    };
  }) prs;
in {
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsets);
}
