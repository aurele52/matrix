module MakeMatrix (K : Type.Field.FIELD) = struct
  module K = K
  module T = MakeTensor.MakeTensorND(K)
  module V = MakeVecteur.MakeVecteur(K)

  (* shape type-level pour 2D *)
  type ('r,'c) dims2 = ('r, ('c, Type.Dims.nil) Type.Dims.cons) Type.Dims.cons

  (* matrice 2D = tenseur ND avec dims2 *)
  type ('r,'c) mat = (('r,'c) dims2) T.t




let rows_dim (m:('r,'c) mat) : 'r Type.Nat_number.nat =
  match m.T.dims with
  | DCons (r, _) -> r

let cols_dim (m:('r,'c) mat) : 'c Type.Nat_number.nat =
  match m.T.dims with
  | DCons (_, DCons (c, _)) -> c

  let rows_int (m:('r,'c) mat) : int =
    match m.T.dims with
    | DCons (r, _) -> T.int_of_nat r

  let cols_int (m:('r,'c) mat) : int =
    match m.T.dims with
    | DCons (_, DCons (c, _)) -> T.int_of_nat c

  let make (rows : 'r Type.Nat_number.nat) (cols : 'c Type.Nat_number.nat) (data : K.t array) : ('r,'c) mat =
    T.make (DCons (rows, DCons (cols, DNil))) data

  let diag (m : ('r,'r) mat): 'r V.vec =
    V.make (rows_dim m) (Array.init (rows_int m) (fun i -> m.data.(i + ((rows_int m) * i))))

  let trace (m : ('r,'r) mat): K.t =
    V.fold K.add (diag m) K.zero

  let transpose (m : ('r,'c) mat) : ('c,'r) mat =
    let r = rows_int m in
    let c = cols_int m in
    make (cols_dim m) (rows_dim m)
    (Array.init (r * c) (fun i ->
      m.data.((i mod r) * c + (i / r))
      ))


  let mult_vec (m : ('r,'c) mat) (v : 'c V.vec) : 'r V.vec =
    let r = rows_int m in
    let c = cols_int m in
    V.make (rows_dim m)
      (Array.init r (fun i ->
         let row = V.make (cols_dim m) (Array.sub m.data (i * c) c) in
         V.dot row v
      ))

  let mult_mat (m1 : ('r,'c) mat) (m2 : ('c,'p) mat) : ('r,'p) mat =
    let r = rows_int m1 in
    let c = cols_int m1 in
    let p = cols_int m2 in
    T.make (DCons ((rows_dim m1), DCons ((cols_dim m2), DNil)))
    (Array.init (r * p) (fun idx ->
      let i = idx / p in
      let j = idx mod p in
      let row = V.make (cols_dim m1) (Array.sub m1.data (i * c) c) in
       let col = V.make (cols_dim m1) (Array.init c (fun k -> m2.data.(k * p + j))) in
       V.dot row col
         ))




  let init (rows : 'r Type.Nat_number.nat) (cols : 'c Type.Nat_number.nat) (f:int -> int -> K.t) : ('r,'c) mat =
    T.init (DCons (rows, DCons (cols, DNil)))
      (fun idx -> f idx.(0) idx.(1))


  let get (m:('r,'c) mat) (i:int) (j:int) : K.t =
    let c = cols_int m in
    let r = rows_int m in
    if i < 0 || i >= r || j < 0 || j >= c then invalid_arg "Matrix.get: oob";
    m.T.data.(i * c + j)

  let set (m:('r,'c) mat) (i:int) (j:int) (x:K.t) : unit =
    let c = cols_int m in
    let r = rows_int m in
    if i < 0 || i >= r || j < 0 || j >= c then invalid_arg "Matrix.set: oob";
    m.T.data.(i * c + j) <- x

  let isSquare (m:('r,'c) mat) : bool = rows_int m = cols_int m

  let print (m : ('r,'c) mat) : unit =
    let r = rows_int m in
    let c = cols_int m in
    let print_row i =
      let rec print_el_rec j =
        if j < c then (
          (* element *)
          K.print (get m i j);
        (* séparateur *)
        if j + 1 < c then print_string ", ";
        print_el_rec (j + 1)
        )
      in
    print_string "{";
    print_el_rec 0;
    print_string "}"
      in
  let rec print_matrix_rec i =
    if i < r then (
      print_string "   ";
      print_row i;
      if i + 1 < r then print_endline ",";
      print_matrix_rec (i + 1)
      )
    in
  print_endline "[";
  print_matrix_rec 0;
  print_endline "";
  print_endline "]"




  (* opérations VSPACE (polymorphes en ('r,'c)) *)
  let add = T.add
  let sub = T.sub
  let scale = T.scale
  let zero_like = T.zero_like
  let equal = T.equal
  let map = T.map
  let fold = T.fold

  (* Exposer une vue VSPACE polymorphe : type 'd t = 'd T.t *)
  module Vspace : Type.Vspace.VSPACE with module K = K and type 'd t = 'd T.t =
    MakeVSpace.MakeVspace(struct
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
