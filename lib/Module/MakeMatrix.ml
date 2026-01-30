module MakeMatrix (K : Type.Field.FIELD) = struct
  module K = K
  module T = MakeTensor.MakeTensorND(K)

  (* shape type-level pour 2D *)
  type ('r,'c) dims2 = ('r, ('c, Type.Dims.nil) Type.Dims.cons) Type.Dims.cons

  (* matrice 2D = tenseur ND avec dims2 *)
  type ('r,'c) mat = (('r,'c) dims2) T.t

  let make (rows : 'r Type.Nat_number.nat) (cols : 'c Type.Nat_number.nat) (data : K.t array) : ('r,'c) mat =
    T.make (DCons (rows, DCons (cols, DNil))) data

  let init (rows : 'r Type.Nat_number.nat) (cols : 'c Type.Nat_number.nat) (f:int -> int -> K.t) : ('r,'c) mat =
    T.init (DCons (rows, DCons (cols, DNil)))
      (fun idx -> f idx.(0) idx.(1))

  let rows_int (m:('r,'c) mat) : int =
    match m.T.dims with
    | DCons (r, _) -> T.int_of_nat r

  let cols_int (m:('r,'c) mat) : int =
    match m.T.dims with
    | DCons (_, DCons (c, _)) -> T.int_of_nat c

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
