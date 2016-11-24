# Pin the nixpkgs version to a specific commit for reproducibility.
let
  pkgs =  (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";

    # 2016-Nov-21
    #
    # rev = "caa81f7bcf5dbb8cee441443ba8225ebae8b4b2c";
    # sha256 = "04530jl2chy0klima5mby3pqq2xwgnm3mhv7gxzc4wa5s6i42q6h";

    # 2017-Jan-12
    #
    # rev = "e91840cfb6b7778f8c29d455a2f24cffa1b4e43e";
    # sha256 = "1pdx9p4gil09766pvgrm7rmrfx1z1wh7li7lnwxck45lcnpbj2s0";

    rev = "bd46a375df572c838d6721ef0365e76227a735c7";
    sha256 = "084jx4z09bnbd0fg4f4yis8s31w8lxw55m390svcj96lzis4a92a";
  };
in import pkgs {}
