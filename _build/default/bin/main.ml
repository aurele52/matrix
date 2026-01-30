open Matrix_project
open Module
open Type.Nat_number

module SI = MakeScalar.MakeScalar(Module.IntField.IntField)
module VF = MakeVecteur.MakeVecteur(FloatField.FloatField)
module VI = MakeVecteur.MakeVecteur(IntField.IntField)
module MI = MakeMatrix.MakeMatrix(IntField.IntField)

(* let n1 : z s nat = (Succ Zero) *)
let n2 : z s s nat = Succ (Succ Zero)
let n3 : z s s s nat = Succ (Succ (Succ Zero))
let n4 : z s s s s nat = Succ (Succ (Succ (Succ Zero)))



(* Ceci NE compile pas (et c'est le but) :
   let boom = V.add v3a v4
*)






let () = print_endline "Matrix 1"
let m23a = MI.make n2 n3 [|2;2;2;2;2;2|]
let () = MI.print m23a
let () = print_endline ""
let () = print_endline "Matrix 2"
let m23b = MI.make n2 n3 [|1;2;3;4;5;6|]
let () = MI.print m23b
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Vector 1"
let v3a = VF.make n3 [|1.0; 2.0; 3.0|]
let () = VF.print v3a
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Vector 2"
let v3b = VF.make n3 [|10.0; 20.0; 30.0|]
let () = VF.print v3b
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Add"
let () = VF.print (VF.add v3a v3b)
let () = print_endline ""
let () = MI.print (MI.add m23a m23b)
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Sub"
let () = VF.print (VF.sub v3a v3b)
let () = print_endline ""
let () = MI.print (MI.sub m23a m23b)
let () = print_endline ""
let () = print_endline "Mult scalar"
let () = VF.print (VF.scale 2. v3b)
let () = print_endline ""
let () = MI.print (MI.scale 2 m23b)
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Matrix 3"
let m23c = MI.make n2 n3 [|1;2;3;0;10;-100|]
let () = MI.print m23c
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Vector 1"
let v2a = VI.make n2 [|10;-2|]
let () = VI.print v2a
let () = print_endline ""
let () = print_endline ""
let () = print_endline "Linear Combination"
module VSMI = MakeLinear.MakeLinear(MI.Vspace)
module VSVF = MakeLinear.MakeLinear(VF.Vspace)
let () = MI.print ( VSMI.combinaison_lineaire_sep [m23a; m23a] [2; 3])
let () = VF.print ( VSVF.lerp (v3a) (v3b) 0.5)
let () = print_endline ""
let () = print_float (VF.dot v3a v3b)
(* let m32a = MI.make n3 n2 [|1;2;3;0;10;-100|] *)
let v4a = VI.make n4 [| 1; -2; 3; -4 |]
let () = print_endline ""
let () = print_float (VI.norm_1 v4a)
let () = print_endline ""
let () = print_float (VI.norm_2 v4a)
let () = print_endline ""
let () = print_float (VI.norm_inf v4a)
(* let () = MI.print (MI.add m32a m23a) *)



