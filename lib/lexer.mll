
  {
    open Ast

    exception Invalid_Input of string;; 
    let match_d x=
match x with 
| "A" ->Some Ad
| "M" ->Some Md
| "D" ->Some Dd
| "MD"->Some MDd
| "AM" ->Some AMd 
| "AMD" ->Some AMDd
| "AD" ->Some ADd
| _ -> raise (Invalid_Input "Invalid dest")

let match_c c = 
match c with
  | "0"      -> Zc
  | "-1"     -> MOc
  | "1"      -> POc
  | "D"      -> Dc
  | "A"      -> Ac
  | "!D"     -> NDc
  | "!A"     -> NAc
  | "-D"     -> NegDc
  | "-A"     -> NegAc
  | "D+1"    -> DPOc
  | "A+1"    -> APOc
  | "D-1"    -> DMOc
  | "A-1"    -> AMOc
  | "D+A"    -> DPAc
  | "D-A"    -> DMAc
  | "A-D"    -> AMDc
  | "D&A"    -> DandAc
  | "D|A"    -> DorAc
  | "M"      -> Mc
  | "!M"     -> NMc
  | "-M"     -> NegMc
  | "M+1"    -> MPOc
  | "M-1"    -> MMOc
  | "D+M"    -> DPMc
  | "D-M"    -> DMMc
  | "M-D"    -> MMDc
  | "D&M"    -> DandMc
  | "D|M"    -> DorMc
  | _ -> print_endline "Invalid bro!";raise (Invalid_Input "Invalid computation")

let match_j j=
match j with
  | "JGT" -> Some JGT
  | "JEQ" -> Some JEQ
  | "JGE" -> Some JGE
  | "JLT" -> Some JLT
  | "JNE" -> Some JNE
  | "JLE" -> Some JLE
  | "JMP" -> Some JMP
  | _ -> raise (Invalid_Input "Invalid Jump")

let counter = ref 0 ;;
}

let valid_string =  [ 'a'-'z' 'A'-'Z' '_' '.' '$' ';' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '.' '$' ';' ]*
let numbers = [ '0'-'9' ]+
let dest = "A" | "M" | "D" |"MD" | "AM" | "AD" | "AMD"
let white = [' ' '\t' '\r' ]+
let match_comp =  "0" | "1" | "-1"
    | "D" | "A" | "!D"| "!A" | "-D" | "-A"
    | "D+1" | "A+1" | "D-1" | "A-1"
    | "D+A" | "D-A" | "A-D" | "D&A" | "D|A"
    | "M" | "!M" | "-M" | "M+1" | "M-1"
    | "D+M" | "D-M" | "M-D" | "D&M" | "D|M"
let match_jmp = "JGT" | "JEQ" | "JGE" | "JLT" | "JNE" | "JLE" | "JMP"
let ignorable = "//" (_#'\n')*

rule token = parse
| '\n' white* '\n' { Ignore_token } 
| '\n' white* "//" (_#'\n')* '\n'  { Ignore_token } 
| '\n' white* "@" (numbers as number) white* ignorable? '\n' { (*print_endline "number token";*)ignore (counter := (!counter) + 1); Ainstruct (Intinstruct number) }
| '\n' white* "@" (valid_string as symbol) white* ignorable? '\n'  { ignore ((*print_endline "atsymbol token";*)counter := (!counter) + 1); Ainstruct (Syminstruct symbol) }
| '\n' white* "(" (valid_string as label) ")" white* ignorable? '\n' { ignore(if (Bool.not ((*print_endline "Label token";*)Hashtbl.mem symbol_tbl label)) then Hashtbl.add symbol_tbl label (!counter)); Label label   }
| '\n' white* ((dest as destination) "=")? (match_comp as computation) (";" (match_jmp as jump))? white* ignorable? '\n' {
    ignore(counter := (!counter) + 1); 
    let dest_opt = match destination with 
        | Some d -> match_d d
        | None -> None
    in
    let jump_opt = match jump with 
        | Some j -> match_j j
        | None -> None
    in
    Cinstruct (dest_opt, (match_c computation), jump_opt)
}
