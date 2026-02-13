open Matrix_project
open Module
open Type.Nat_number

module SI = MakeScalar.MakeScalar(Module.IntField.IntField)
module VF = MakeVecteur.MakeVecteur(FloatField.FloatField)
module VI = MakeVecteur.MakeVecteur(IntField.IntField)
module MI = MakeMatrix.MakeMatrix(IntField.IntField)
module MF = MakeMatrix.MakeMatrix(FloatField.FloatField)

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
let () = print_endline ""
let () = print_float (VF.angle_cos v3a v3b)
let () = print_endline ""

let m23anew = MI.make n2 n3 [|1;2;3;0;10;-100|]
let () = MI.print m23anew
let () = MI.print (MI.transpose m23anew)

let () = print_endline "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
(* let () = MI.print (MI.add m32a m23a) *)


(* exemples de tests, dans le même style que ton snippet *)

let m23a = MF.make n2 n3 [| 1.; 2.; 3.;
                           0.; 10.; -100. |]

let () = print_endline "m23a ="; MF.print m23a
let () = print_endline "transpose(m23a) ="; MF.print (MF.transpose m23a)

let () = print_endline "row_echelon(m23a) ="; MF.print (MF.row_echelon m23a)
let () = print_endline "rref(m23a) ="; MF.print (MF.rref m23a)


(* un autre test simple qui doit donner un truc “propre” en RREF *)
let m33 = MF.make n3 n3 [| 1.; 2.; 3.;
                          0.; 0.; 3.;
                          0.; 0.; 0. |]
let m22 = MF.make n2 n2 [| 3.; 2.;
                           2.; 2. |]

let () = print_endline "m33 ="; MF.print m33
let () = print_endline "rref(m33) ="; MF.print (MF.rref m33)
let () = print_endline "rang(m33) ="; print_int (MF.rang m33)
let () = print_endline "rang(m33) ="; print_float (MF.determinant m22)


