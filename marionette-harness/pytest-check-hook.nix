{ makePythonHook, pythonCheckInterpreter, pytest }:
    makePythonHook {
      name = "pytest-check-hook";
      propagatedBuildInputs = [ pytest ];
      substitutions = {
        inherit pythonCheckInterpreter;
      };
    } ./pytest-check-hook.sh
