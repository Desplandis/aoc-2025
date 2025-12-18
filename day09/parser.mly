%{

  let syntax_error spos cpos =
    let open Lexing in
    Format.eprintf "Line %d, characters %d-%d: Syntax Error\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol);
    exit(1)
%}


%token COMMA
%token<int> NUMBER
%token EOF

%start<(int * int) list> parse

%%

parse: rs = coordinate* EOF {
  rs
}
| error {
  syntax_error $startpos $endpos
}

coordinate: l = NUMBER COMMA r = NUMBER { (l, r) }
