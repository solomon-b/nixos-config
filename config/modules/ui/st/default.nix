{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
      buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
      patches = [
        # (fetchpatch {
        #   url = "https://st.suckless.org/patches/scrollback/st-scrollback-ringbuffer-0.8.5.diff";
        #   sha256 = "sha256-oYbCQ1YC0SgPgVdMhV8w/aPTX3D4kjBubKDugpUfwy4=";
        # })
        (fetchpatch {
          url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.5.diff";
          sha256 = "sha256-ZZAbrWyIaYRtw+nqvXKw8eXRWf0beGNJgoupRKsr2lc=";
        })
        (fetchpatch {
          url = "https://st.suckless.org/patches/scrollback/st-scrollback-reflow-0.9.diff";
          sha256 = "sha256-gmCya4PzGNcFhadAqxgoq9ydAll2hl4BytPC5Cc9cag=";
        })
        (fetchpatch {
          url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20220127-2c5edf2.diff";
          sha256 = "sha256-CuNJ5FdKmAtEjwbgKeBKPJTdEfJvIdmeSAphbz0u3Uk=";
        })
        (fetchpatch {
          url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-altscreen-20220127-2c5edf2.diff";
          sha256 = "sha256-8oVLgbsYCfMhNEOGadb5DFajdDKPxwgf3P/4vOXfUFo=";
        })
        (fetchpatch {
          url = "https://st.suckless.org/patches/blinking_cursor/st-blinking_cursor-20230819-3a6d6d7.diff";
          sha256 = "sha256-f79RFl4JFKNF4cAl8WsfNC2Zx5nd8lQ8TuSxl2rBxfY==";
        })
        (fetchpatch {
          url = "https://st.suckless.org/patches/ligatures/0.9/st-ligatures-scrollback-20230105-0.9.diff";
          sha256 = "sha256-hcY+EatluHfKzp0kSAey6lwJvUMBkU5jWGLhrtGjVEw=";
        })
        ./0001-Adds-Fira-Code-Font.patch
        ./0002-Adds-Sanity-Inc-Tomorrow-Eighties-Theme.patch
        ./0003-Change-Font-Size-Shortcuts.patch
      ];
    }))
  ];
}
