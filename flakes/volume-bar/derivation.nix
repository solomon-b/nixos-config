{ python39Packages }:
with python39Packages;
buildPythonApplication {
  pname = "pulse-volume-watcher";
  version = "1.0";
  propagatedBuildInputs = [ pulsectl ];
  src = ./.;
}
