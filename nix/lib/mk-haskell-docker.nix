{ pkgs, ... }:

## Function that makes a Docker image for the given Haskell application.
{ app
, name ? app.pname
, registry
, repository
, tag
, entrypoint ? "${app}/bin/${name}"
, cmd ? null
}:
pkgs.dockerTools.buildImage {
  name = registry + "/" + repository;
  tag = tag;
  created = "now";

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ pkgs.cacert ];
    pathsToLink = [ "/etc" ];
  };

  runAsRoot = ''
    #!${pkgs.runtimeShell}
    ${pkgs.dockerTools.shadowSetup}
    mkdir /tmp
    chmod 777 /tmp
    groupadd -r users
    useradd -r -g users patron
  '';

  config = {
    Entrypoint = [ "${app}/bin/${name}" ];
    Cmd = cmd;

    User = "patron";
  };
}
