open Lexer;;
open Parser;;
open Ast;;
let main () =

  let test_str =  "\\+ func1(X,Y, C), C is func2(X)." in
    try
      let lexbuf = Lexing.from_string test_str in
        let tree = Parser.program Lexer.token lexbuf in
          Ast.print_program tree;
    with End_of_file -> exit 0
  ;;
  let _ = Printexc.print main ()