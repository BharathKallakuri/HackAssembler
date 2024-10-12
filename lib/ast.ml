type ainstruct = 
| Intinstruct of string
| Syminstruct of string

type dest = 
| Md
| Dd
| MDd
| Ad
| AMd
| ADd
| AMDd

type comp = 
| Zc (*Zero*)
| MOc (*Minus one*)
| POc (*Plus one*)
| Dc
| Ac
| NDc (*Not D*)
| NAc (*Not A*)
| NegDc
| NegAc
| DPOc (*D plus one*)
| APOc (*A plus one*)
| DMOc (*D minus one*)
| AMOc (*A minus one*)
| DPAc (*D plus A*)
| DMAc (*D minus A*)
| AMDc (*A minus D*)
| DandAc
| DorAc
| Mc
| NMc
| NegMc
| MPOc
| MMOc
| DPMc
| DMMc
| MMDc
| DandMc
| DorMc

type jump = 
| JGT
| JEQ
| JGE
| JLT
| JNE
| JLE
| JMP

type fin_token = 
| Label of string
| Ainstruct of ainstruct 
| Cinstruct of (dest option)*comp*(jump option)
| Ignore_token




(* 
let symbol_tbl = Hashtbl.create 100;;
Hashtbl.add symbol_tbl "R0" 1;;
Hashtbl.add symbol_tbl "R1" 2;;
Hashtbl.add symbol_tbl "R2" 3;;
Hashtbl.add symbol_tbl "R3" 4;;
Hashtbl.add symbol_tbl "R5" 5;;
Hashtbl.add symbol_tbl "R6" 6;;
Hashtbl.add symbol_tbl "R7" 7;;
Hashtbl.add symbol_tbl "R8" 8;;
Hashtbl.add symbol_tbl "R9" 9;;
Hashtbl.add symbol_tbl "R10" 10;;
Hashtbl.add symbol_tbl "R11" 11;;
Hashtbl.add symbol_tbl "R12" 12;;
Hashtbl.add symbol_tbl "R13" 13;;
Hashtbl.add symbol_tbl "R14" 14;;
Hashtbl.add symbol_tbl "R15" 15;;
Hashtbl.add symbol_tbl "SP" 0;;
Hashtbl.add symbol_tbl "LCL" 1;;
Hashtbl.add symbol_tbl "ARG" 2;;
Hashtbl.add symbol_tbl "THIS" 3;;
Hashtbl.add symbol_tbl "THAT" 4;;
Hashtbl.add symbol_tbl "KBD" 24576;;
Hashtbl.add symbol_tbl "SCR" 16384;;
 *)