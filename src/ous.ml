
let main () =
    if Array.length Sys.argv != 2 then begin
        Format.eprintf "usage : %s <inputfile>\n" Sys.argv.(0);
        exit 1
    end else begin
        try
            let in_c = open_in Sys.argv.(1) in
            let lexbuf = Lexing.from_channel in_c in
            try
                let s = Parser.survey Lexer.token lexbuf in
                close_in in_c;
                Survey.to_js s
            with
                _ -> let p = Lexing.lexeme_start_p lexbuf
        in
            Format.eprintf "SYNTAX ERROR line %d\n" p.Lexing.pos_lnum
        with
            e -> Format.eprintf "ERROR: %s\n" (Printexc.to_string e);
                 exit 2
    end

let () = main ()
