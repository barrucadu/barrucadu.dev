FROM nixos/nix:2.3

RUN nix-channel --add https://nixos.org/channels/nixos-20.09 nixpkgs && \
    nix-channel --add https://nixos.org/channels/nixos-unstable unstable && \
    nix-channel --update

ENV LANG=en_GB.UTF-8 \
    LC_ALL=en_GB.UTF-8 \
    LC_CTYPE=en_GB.UTF-8 \
    NIXPKGS_CONFIG=/config.nix

RUN nix-env -iA \
        nixpkgs.git \
        nixpkgs.gnum4 \
        nixpkgs.graphviz \
        unstable.haskellPackages.pandoc \
        nixpkgs.python3Packages.virtualenv \
        nixpkgs.stack \
        nixpkgs.texlive.combined.scheme-full
