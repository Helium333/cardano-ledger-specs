{ pkgs ? (import ../../../../../nix/lib.nix).pkgs
}:

with pkgs;

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic xcolor appendix

                      # bclogo and dependencies
                      bclogo mdframed xkeyval etoolbox needspace pgf

                      # font libraries `mathpazo` seems to depend on palatino
                      # , but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk

                      ;
                  })
                ];
  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Non-integer Calculations Specification";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
