open Assembler.Ast
open Assembler.Lexer

let symref = ref 15;;
let next_val() = 
symref := (!symref) + 1;
(!symref)



  let resolve_syminstruct sym_tbl = function
  | x -> if (Hashtbl.mem sym_tbl x) then (Hashtbl.find sym_tbl x)
  else
    next_val (Hashtbl.add sym_tbl x ((!symref)+1))
    

    

let resolve_tokens symbol_tbl x = 

  let resolve_intinstruct = 
    let zfill_finstr (inp : string) = if ((String.length inp) <15) then (String.make (16 - (String.length inp)) '0' ^ inp) else ("0" ^ (String.sub inp ((String.length inp)-15) 15)) 
    in
    let rec int_bin (a:int) = if a>0 then (int_bin (a/2))^string_of_int (a mod 2) else "0" 
    in
    fun  x -> zfill_finstr (int_bin (int_of_string x))
    
  in

  let resolve_dest = function
    | Some Md -> "001"
    | Some Dd -> "010"
    | Some MDd -> "011"
    | Some Ad -> "100"
    | Some AMd -> "101"
    | Some ADd -> "110"
    | Some AMDd -> "111"
    | None -> "000"

  in

  let resolve_jump = function
    | Some JGT -> "001"
    | Some JEQ -> "010"
    | Some JGE -> "011"
    | Some JLT -> "100"
    | Some JNE -> "101"
    | Some JLE -> "110"
    | Some JMP -> "111"
    | None -> "000"
  
  in

  let resolve_comp = function
    | Zc (*Zero*) -> "0101010"
    | MOc (*Minus one*)-> "0111010"
    | POc (*Plus one*)-> "0111111"
    | Dc -> "0001100"
    | Ac -> "0110000"
    | NDc (*Not D*) -> "0001101"
    | NAc (*Not A*) -> "0110001"
    | NegDc -> "0001111"
    | NegAc -> "0110011"
    | DPOc (*D plus one*)-> "0011111"
    | APOc (*A plus one*)-> "0110111"
    | DMOc (*D minus one*)-> "0001110"
    | AMOc (*A minus one*)-> "0110010"
    | DPAc (*D plus A*)-> "0000010"
    | DMAc (*D minus A*)-> "0010011"
    | AMDc (*A minus D*)-> "0000111"
    | DandAc -> "0000000"
    | DorAc -> "0010101"
    | Mc -> "1110000"
    | NMc -> "1110001"
    | NegMc -> "1110011"
    | MPOc -> "1110111"
    | MMOc -> "1110010"
    | DPMc -> "1000010"
    | DMMc -> "1010011"
    | MMDc -> "1000111"
    | DandMc -> "1000000"
    | DorMc -> "1010101"

    in

  match x with
  |Ainstruct (Intinstruct a) -> resolve_intinstruct a ^"\n"
  |Ainstruct (Syminstruct a) -> resolve_intinstruct(string_of_int(resolve_syminstruct symbol_tbl a)) ^"\n"
  |Cinstruct (d,c,j) -> "111"^(resolve_comp c)^(resolve_dest d)^(resolve_jump j)^"\n"
  |Label _label -> ""
  |Ignore_token -> ""
  (* |Label label -> resolve_intinstruct(string_of_int(Hashtbl.find symbol_tbl label)) *)



let input_list = String.split_on_char '\n' (String.trim (In_channel.input_all stdin))                             

let for_temp a = "\n"^a^"\n"

let inter_list = List.map for_temp input_list

let temp_list = List.map (Lexing.from_string) inter_list

let tokenized_list = List.map token temp_list

let fin_list = List.map (resolve_tokens symbol_tbl) tokenized_list

let () = List.iter (Printf.printf "%s") fin_list

