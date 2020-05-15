{
    open Parser
    let tfmt fmt t =
    let name, arg = match t with
        | SURVEY_TITLE s -> "SURVEY_TITLE", Some s
        | SEP -> "SEP", None
        | SECTION_TITLE s -> "SECTION_TITLE", Some s
        | QUESTION s -> "QUESTION", Some s
  | OTHER -> "OTHER", None
  | LINE s -> "LINE", Some s
  | LIMIT i -> "LIMIT", Some (string_of_int i)
  | FREE_INPUT -> "FREE_INPUT",None
  | EXCLUSIVE_CHOICE s ->  "EXCLUSIVE_CHOICE", Some s
  | EOF -> "EOF", None
  in
  Format.fprintf fmt "%s" name;
  match arg with
  None -> ()
  |  Some s -> Format.fprintf fmt "(%s)" s

}

let space = ('\012' | '\r' | ' ' | '\t')

rule token = parse
    | '\n' '\n'+ { 
        for _i = 0 to String.length (Lexing.lexeme lexbuf) - 1 do
            Lexing.new_line lexbuf;
        done; 
        SEP
    }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | space+ { token lexbuf }
    | space* "##" ([^'\n']+ as str) '\n' {
        Lexing.new_line lexbuf;
        SECTION_TITLE String.(trim str)
    }
    | space* "#" ([^'\n']+ as str) '\n' {
        Lexing.new_line lexbuf;
        SURVEY_TITLE String.(trim str)
    }
    | space* "*" space* "Other:\n" {
        Lexing.new_line lexbuf;
        OTHER
    }
    | space* "*" ([^'\n']+ as str) '\n' {
        Lexing.new_line lexbuf;
        QUESTION String.(trim str)
    }
    | space* "+" ([^'\n']+ as str) '\n' {
        Lexing.new_line lexbuf;
        EXCLUSIVE_CHOICE String.(trim str)
    }
    |  space* "(free input)" space* '\n' { FREE_INPUT }

    | space* "Please pick at most "(['0'-'9']+ as num) " choices." space* '\n' {
        Lexing.new_line lexbuf;
        LIMIT (int_of_string num)
    }
    | [^'#''*''+''\n'] [^'\n']* '\n' {
        Lexing.new_line lexbuf;
        LINE (Lexing.lexeme lexbuf);
    }
    | eof { EOF }
    | _  as s { failwith ("lexing error : " ^ (String.make 1 s)) }

{
let debug_lexer = true
let token =
    if debug_lexer then
     (fun lexbuf ->
          let t = token lexbuf in
          Format.eprintf ">>%a<<\n%!" tfmt t;
          t)
    else token

}