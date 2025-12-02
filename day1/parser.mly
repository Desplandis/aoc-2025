%{

  let syntax_error spos cpos =
    let open Lexing in
    Format.eprintf "Line %d, characters %d-%d: Syntax Error\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol);
    exit(1)
%}


%token LEFT RIGHT
%token<int> NUMBER
%token EOF

%start<int list> parse

%%

parse: rs = rotation* EOF {
  rs
}
| error {
  syntax_error $startpos $endpos
}

rotation: LEFT nb = NUMBER { - nb }
| RIGHT nb = NUMBER { nb }
