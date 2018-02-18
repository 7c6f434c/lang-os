{pkgs ? import <nixpkgs> {}, nixos ? import <nixpkgs/nixos>}:
rec {
  serviceScript = name: config:
    (builtins.getAttr name (nixos {configuration = config;}).config.systemd.services).runner;

  etcSelectTarget = filename: config:
    let
      nixosInstance = nixos {configuration = config;};
      selected = (pkgs.lib.filterAttrs (k: v: v.target == filename) nixosInstance.config.environment.etc);
        source = if selected == {} then null else (builtins.getAttr (builtins.head (builtins.attrNames selected)) selected).source;
    in (if source == null then null else if pkgs.lib.isString source then source else source.outPath);

  etcSelectRenamed = newName: filename: config:
    let target = etcSelectTarget filename config; in
    if target == null then {} else builtins.listToAttrs [{name= newName; value= target;}];

  etcSelectComponent = filename: config:
    etcSelectRenamed filename filename config;

  etcSelectComponents = filenames: config:
    if filenames == [] then {} else
      pkgs.lib.foldr (a: b: a//b) {}
      (builtins.map (fn: etcSelectComponent fn config) filenames);

  etcSelectPrefix = filenamePrefix: config:
    let
      nixosInstance = nixos {configuration = config;};
      selected = (pkgs.lib.filterAttrs
                           (k: v: pkgs.lib.hasPrefix filenamePrefix v.target)
                           nixosInstance.config.environment.etc);
    in (pkgs.lib.mapAttrs'
             (k: v: let source = v.source; in 
                      {
                        value = (if pkgs.lib.isString source then source else source.outPath);
                        name = v.target;
                      })
             selected);
}
