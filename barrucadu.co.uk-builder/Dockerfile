FROM nixos/nix:2.3

RUN nix-channel --add https://nixos.org/channels/nixos-20.03 nixpkgs && \
    nix-channel --update

ENV LANG=en_GB.UTF-8 \
    LC_ALL=en_GB.UTF-8 \
    LC_CTYPE=en_GB.UTF-8

RUN nix-env -iA \
        nixpkgs.git \
        nixpkgs.gnum4 \
        nixpkgs.graphviz \
        nixpkgs.haskellPackages.pandoc \
        nixpkgs.haskellPackages.pandoc-sidenote \
        nixpkgs.python3Packages.virtualenv \
        nixpkgs.stack \
        nixpkgs.texlive.combined.scheme-full
