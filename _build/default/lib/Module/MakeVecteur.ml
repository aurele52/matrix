module MakeVecteur (K : Type.Field.FIELD) = struct
  module K = K
  module T = MakeTensor.MakeTensorND(K)

  type ('n) dims1 = ('n, Type.Dims.nil) Type.Dims.cons

  type ('n) vec = (('n) dims1) T.t

  let make (nEl : 'n Type.Nat_number.nat) (data : K.t array) : 'n vec =
    T.make (DCons (nEl, DNil)) data


  let vector_with_vector (v1: 'n vec) (v2: 'n vec) (f: K.t -> K.t -> K.t): 'n vec =
    {v1 with data = Array.init (Array.length v1.data) (fun i ->
      f v1.data.(i) v2.data.(i)
    )}

  (* produit scalaire: somme_i (a_i * b_i) *)
  let dot (v1: 'n vec) (v2: 'n vec): K.t =
    T.fold K.add (vector_with_vector v1 v2 K.mul) K.zero

  let norm_1 (v : 'n vec) : float =
    K.to_float (T.fold K.add (T.map (fun x -> (K.abs x)) v) K.zero)

  let norm_2 (v: 'n vec): float =
    sqrt (K.to_float (T.fold K.add (vector_with_vector v v K.mul) K.zero))

  let norm_inf (v : 'n vec) : float =
    Array.fold_left (fun acc x ->
      Float.max acc (K.to_float (K.abs x))
      ) 0. v.data

  let angle_cos u v =
    let d = dot u v in
    let nu = norm_2 u in
    let nv = norm_2 v in
    if nu = 0. || nv = 0. then
      invalid_arg "zero vector"
    else
      K.to_float d /. (nu *. nv)

  let cross_product (v1 : Type.Nat_number.z Type.Nat_number.s Type.Nat_number.s Type.Nat_number.s Type.Nat_number.nat vec) (v2 : Type.Nat_number.z Type.Nat_number.s Type.Nat_number.s Type.Nat_number.s Type.Nat_number.nat vec) : Type.Nat_number.z Type.Nat_number.s Type.Nat_number.s Type.Nat_number.s Type.Nat_number.nat vec = 
    if Array.length v1.data = 3 && Array.length v2.data = 3 then     {v1 with data = [|
      K.sub (K.mul v1.data.(1) v2.data.(2)) (K.mul v1.data.(2) v2.data.(1));
      K.sub (K.mul v1.data.(2) v2.data.(0)) (K.mul v1.data.(0) v2.data.(2));
      K.sub (K.mul v1.data.(0) v2.data.(1)) (K.mul v1.data.(1) v2.data.(0))
    |]} else invalid_arg "Cross_Product: bad dimension"


  let print (v : 'n vec) : unit =
    let a = v.data in
    let rec print_el_rec i =
      if i < Array.length a then (
        if (i + 1) = Array.length a then (
          K.print a.(i);
          print_el_rec (i + 1)
        ) else (
          K.print a.(i);
          print_string ", ";
          print_el_rec (i + 1)
        )
      )
    in
    print_string "{";
    print_el_rec 0;
    print_string "}"

  (* opérations VSPACE (polymorphes en ('r,'c)) *)
  let add = T.add
  let sub = T.sub
  let scale = T.scale
  let zero_like = T.zero_like
  let equal = T.equal
  let map = T.map
  let fold = T.fold

  (* Exposer une vue VSPACE polymorphe : type 'd t = 'd T.t *)
  module Vspace : Type.Vspace.VSPACE with module K = K and type 'd t = 'd T.t = MakeVSpace.MakeVspace(struct
    module K = K
    type 'd t = 'd T.t
    let add = T.add
    let sub = T.sub
    let scale = T.scale
    let zero_like = T.zero_like
    let equal = T.equal
    let map = T.map
    let fold = T.fold
  end)
end
