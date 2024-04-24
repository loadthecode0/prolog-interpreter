
{
  open Parser;;
}

(* let validID = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' ]* | ['_']['a'-'z' 'A'-'Z' '0'-'9' '_' ]+ *)
let validID = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ]* 
let integers = ['0'] | ['1'-'9']['0'-'9']* | ['-']['1'-'9']['0'-'9']*
let invalidID = ['A'-'Z' ''' '0'-'9']+validID 
let var_fmt = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*| ['_']['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

rule token = parse
    

     [' ' '\t']
       { token lexbuf }
    | "is" {IS}
    | "\\+" {NOT}
    | validID as x
        {IDENTIFIER(x)}
    | var_fmt as x
        {VARIABLE(x)}
    | [',' ';']
        {SEPARATOR}
    | ['\n'] {EOL}
    | ['.'] {PERIOD}
    | ":-" {IF}

    | ['>']
       { GT }
    | "\\=="
       { NEQ }
    | ['+']
       { ADD }
    | ['=']
       { EQUALS }
    | ['*']
       { MUL }
    | ['0'-'9']+ as lxm
       { INT(int_of_string lxm) }
    | ['(']
       { LPAREN }
    | [')']
       { RPAREN }
    | ['[']
       { LSQ }
    | [']']
       { RSQ }   
    | ['|']
       { BAR }    
    | ['!']
       { CUT }
   | eof
       { EOF }