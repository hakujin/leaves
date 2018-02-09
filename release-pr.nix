# { prsJSON, nixpkgs }:
# let
#   pkgs = import nixpkgs {};
#   prs = builtins.fromJSON (builtins.readFile prsJSON);
#   jobsets = pkgs.lib.mapAttrs (num: info: {
#     name = "leaves";
#     value = {
#       enabled = 1;
#       hidden = false;
#       description = "PR ${num}: ${info.title}";
#       nixexprinput = "src";
#       nixexprpath = "release.nix";
#       checkinterval = 30;
#       schedulingshares = 20;
#       enableemail = false;
#       emailoverride = "";
#       keepnr = 3;
#       inputs = {
#         src = {
#           type = "git";
#           value = "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";
#           emailresponsible = false;
#         };
#         nixpkgs = {
#           type = "git";
#           value = "https://github.com/NixOS/nixpkgs master";
#           emailresponsible = false;
#         };
#       };
#     };
#   }) prs;
# in {
#   jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsets);
# }
#
#
#
{ prsJSON, nixpkgs }:

let
  pkgs = import nixpkgs {};
  prs = builtins.fromJSON (builtins.readFile prsJSON);
  nixpkgs-src = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
  defaultSettings = {
    enabled = 1;
    hidden = false;
    nixexprinput = "jobsets";
    keepnr = 5;
    schedulingshares = 42;
    checkinterval = 60;
    inputs = {
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
      jobsets = mkFetchGithub "https://github.com/hakujin/leaves.git master";
    };
    enableemail = false;
    emailoverride = "";
  };
  mkLeaves = branch: nixpkgsRev: {
    nixexprpath = "release.nix";
    nixexprinput = "src";
    description = "leaves";
    inputs = {
      src = mkFetchGithub "https://github.com/hakujin/leaves.git ${branch}";
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
    };
  };
  makeLeavesPR = num: info: {
    name = "leaves-pr-${num}";
    value = defaultSettings // {
      description = "PR ${num}: ${info.title}";
      nixexprinput = "src";
      nixexprpath = "release.nix";
      inputs = {
        nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git master";
        src = mkFetchGithub "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";
      };
    };
  };
  leavesPrJobsets = pkgs.lib.listToAttrs (pkgs.lib.mapAttrsToList makeLeavesPR prs);
  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    leaves = mkLeaves "master" nixpkgs-src.rev;
  });
  jobsetsAttrs =  leavesPrJobsets // mainJobsets;
  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cp ${jobsetJson} $out
  '';
}
