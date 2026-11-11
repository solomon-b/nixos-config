{ ... }:

{
  # Manage only the bespoke Claude Code files. Each file gets its own symlink,
  # so ~/.claude/{agents,commands,skills} stay writable and third-party
  # agents, skills, and plugins install the usual way. settings.json stays
  # unmanaged because Claude Code writes to it at runtime.
  programs.claude-code = {
    enable = true;

    # claude comes from the native installer, which updates itself.
    package = null;

    # A string, not a path, so a machine can append its own lines.
    context = builtins.readFile ./CLAUDE.md;

    agentsDir = ./agents;
    commandsDir = ./commands;
    skills.solomon-style = ./skills/solomon-style;
  };
}
