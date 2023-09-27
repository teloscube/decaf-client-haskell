{ sources ? import ./nix/sources.nix
, compiler ? "default"
, system ? builtins.currentSystem
, dockerImageRegistry ? "registry.docker.decafhub.com"
, dockerImageRepository ? null
, dockerImageTag ? null
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ##################
  ## LOAD HELPERS ##
  ##################

  ## Load the YAML reader:
  readYAML = pkgs.callPackage ./nix/lib/read-yaml.nix { };

  ## Load Haskell package factory:
  mkHaskell = pkgs.callPackage ./nix/lib/mk-haskell.nix { };

  ## Load Haskell application factory:
  mkHaskellApp = pkgs.callPackage ./nix/lib/mk-haskell-app.nix { };

  ## Load Docker image factory for Haskell application:
  mkHaskellDocker = pkgs.callPackage ./nix/lib/mk-haskell-docker.nix { };

  #############
  ## HASKELL ##
  #############

  ## Get Haskell packages in the project:
  thisHaskellPackages = {
    main = {
      name = "decaf-client";
      path = ./.;
    };
    subs = [ ];
  };

  ## Get Haskell packages in the project as a list:
  thisHaskellPackagesAll = [ thisHaskellPackages.main ] ++ thisHaskellPackages.subs;

  ## Get the main Haskell package specification:
  packageSpec = readYAML (thisHaskellPackages.main.path + "/package.yaml");

  ## Get base Haskell package set:
  baseHaskell = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  ## Get this Haskell package set:
  thisHaskell = mkHaskell {
    haskell = baseHaskell;
    packages = thisHaskellPackagesAll;
    overrides = self: super: { };
  };

  ###########
  ## SHELL ##
  ###########

  ## Prepare Nix shell:
  thisShell = thisHaskell.shellFor {
    ## Define packages for the shell:
    packages = p: builtins.map (x: p.${x.name}) thisHaskellPackagesAll;

    ## Enable Hoogle:
    withHoogle = false;

    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisHaskell.apply-refact
      thisHaskell.cabal-fmt
      thisHaskell.cabal-install
      thisHaskell.cabal2nix
      thisHaskell.fourmolu
      thisHaskell.haskell-language-server
      thisHaskell.hlint
      thisHaskell.hpack
      thisHaskell.weeder

      ## Other build inputs for various development requirements:
      pkgs.docker-client
      pkgs.git
      pkgs.git-chglog
    ];

    ## Shell hook for development shell:
    shellHook = ''
      ## Environment variables:
      export PROJECT_DEV_ROOT="$(git rev-parse --show-toplevel)"

      ## Shell aliases:
      alias riched="${pkgs.rich-cli}/bin/rich --emoji --center --width 120 --panel rounded --theme rrt --hyperlinks"
      alias devsh-help="riched --pager ''${PROJECT_DEV_ROOT}/README.md"
      alias devsh-welcome="riched ''${PROJECT_DEV_ROOT}/README.md"
      alias devsh-makedev="hpack && fourmolu -i app/ src/ test/ && hlint app/ src/ test/ && cabal build -O0 && cabal v1-test"

      ## Greet:
      devsh-welcome
      echo
      echo '**Run devsh-welcome to see Welcome notice again**' | riched -m -
      echo '**Run devsh-help to see help notice**' | riched -m -
    '';
  };

  #################
  ## APPLICATION ##
  #################

  ## Get the installable application (only static executable):
  thisApp = mkHaskellApp {
    drv = thisHaskell.${thisHaskellPackages.main.name};
  };

  ############
  ## DOCKER ##
  ############

  thisDocker = mkHaskellDocker {
    app = thisApp;
    registry = dockerImageRegistry;
    repository = if isNull dockerImageRepository then packageSpec.name else dockerImageRepository;
    tag = if isNull dockerImageTag then packageSpec.version else dockerImageTag;
  };
in
if pkgs.lib.inNixShell then thisShell else {
  shell = thisShell;
  app = thisApp;
  docker = thisDocker;
}
