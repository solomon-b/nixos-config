{ python39Packages }:
with python39Packages;
buildPythonApplication {
  pname = "brightness-watcher";
  version = "1.0";
  propagatedBuildInputs = [ watchdog ];
  src = ./.;
}
