module MakeLinear (V : Vspace.VSPACE) = struct
  module K = V.K

  (* zip récursif sécurisé *)
  let rec zip cs vs =
    match cs, vs with
    | [], [] -> []
    | c :: cs', v :: vs' -> (c, v) :: zip cs' vs'
    | _ -> invalid_arg "combinaison_lineaire: tailles differentes"

  (* combinaison_lineaire avec 2 listes *)
  let combinaison_lineaire_sep
      (vs : 'd V.t list)
      (cs : K.t list) : 'd V.t =
    match vs, cs with
    | [], [] ->
        invalid_arg "combinaison_lineaire: listes vides"
    | v0 :: _, _ ->
        let terms = zip cs vs in
        let rec comb_rec acc = function
          | [] -> acc
          | (a, v) :: q ->
              comb_rec (V.add acc (V.scale a v)) q
        in
        comb_rec (V.zero_like v0) terms
    | _ ->
        invalid_arg "combinaison_lineaire: tailles differentes"

  (* lerp(a, b, t) = (1 − t) a + tb *)
  let lerp (m1 : 'd V.t) (m2 : 'd V.t) (t: K.t): 'd V.t =
    V.add (V.scale (K.sub K.one t) m1) (V.scale t m2 )

  (* combinaison_lineaire [ (a1,v1); (a2,v2); ... ] = a1*v1 + a2*v2 + ... *)
  let combinaison_lineaire (l : (K.t * 'd V.t) list) : 'd V.t =
    match l with
    | [] -> invalid_arg "combinaison_lineaire: liste vide"
    | (_, v0) :: _ ->
        let rec comb_rec acc = function
          | [] -> acc
          | (a, v) :: q ->
              comb_rec (V.add acc (V.scale a v)) q
        in
        comb_rec (V.zero_like v0) l
end
