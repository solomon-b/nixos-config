self: _: {
  passwordUtils = (self.callPackage ../lib/passwords.nix { }).passwordUtils;
}
