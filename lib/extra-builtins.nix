{ exec, ... }:
{
  # NOTE: Do not use this for any secrets you do not want in the nix store.
  pass = name: exec [./nix-pass.sh name];
}
