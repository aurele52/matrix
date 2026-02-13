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

  let set_copy (m:('r,'c) mat) (i:int) (j:int) (x:K.t) : ('r, 'c) mat = (
    let c = cols_int m in
    let r = rows_int m in (
      if i < 0 || i >= r || j < 0 || j >= c then invalid_arg "Matrix.set: oob";
  let new_data = Array.copy m.T.data in
  new_data.(i * c + j) <- x;
  T.make m.T.dims new_data))


  let copy (m:('r,'c) mat) : ('r,'c) mat = 
    T.make m.T.dims m.T.data

  let swap_rows (m : ('r,'c) mat) (i : int) (j : int) : ('r,'c) mat =
    let r = rows_int m in
    let c = cols_int m in
    if i < 0 || i >= r || j < 0 || j >= r then
      invalid_arg "Matrix.swap_rows: oob"
    else if i = j then
      m
    else
      let data =
        Array.init (r * c) (fun idx ->
          let row = idx / c in
          let col = idx mod c in
          let src_row =
            if row = i then j
          else if row = j then i
            else row
          in
        m.T.data.(src_row * c + col)
        )
          in
    T.make m.T.dims data

let scale_row (m : ('r,'c) mat) (i : int) (k : K.t) : ('r,'c) mat =
  let r = rows_int m in
  let c = cols_int m in
  if i < 0 || i >= r then invalid_arg "Matrix.scale_row: oob";
  let data =
    Array.init (r * c) (fun idx ->
      let row = idx / c in
      if row = i then K.mul m.T.data.(idx) k else m.T.data.(idx)
    )
  in
  T.make m.T.dims data

let add_row_multiple
    (m : ('r,'c) mat)
    (i : int)
    (j : int)
    (k : K.t)
  : ('r,'c) mat =
  (* L_i <- L_i + k * L_j *)
  let r = rows_int m in
  let c = cols_int m in
  if i < 0 || i >= r || j < 0 || j >= r then invalid_arg "Matrix.add_row_multiple: oob";
  if i = j then
    (* L_i <- L_i + k * L_i = (1+k)*L_i (ok, mais rare) *)
    let one_plus_k = K.add K.one k in
    scale_row m i one_plus_k
  else
    let data =
      Array.init (r * c) (fun idx ->
        let row = idx / c in
        let col = idx mod c in
        if row <> i then m.T.data.(idx)
        else
          let a = m.T.data.(i * c + col) in
          let b = m.T.data.(j * c + col) in
          K.add a (K.mul k b)
      )
    in
    T.make m.T.dims data

(* ---------- pivots / elimination ---------- *)
let find_pivot_row (m : ('r,'c) mat) (start_row : int) (col : int) : int option =
  let r = rows_int m in
  let c = cols_int m in
  if col < 0 || col >= c then invalid_arg "Matrix.find_pivot_row: bad col";
  let rec find_pivot_row_rec i =
    if i >= r then None
    else
      let x = get m i col in
      if K.zero = x then find_pivot_row_rec (i + 1) else Some i
  in
 find_pivot_row_rec start_row

let normalize_row (m : ('r,'c) mat) (row : int) (col : int) : ('r,'c) mat =
  let pivot = get m row col in
  if K.zero = pivot then invalid_arg "Matrix.normalize_row: zero pivot";
  let inv = K.div K.one pivot in
  scale_row m row inv

let eliminate_column (m : ('r,'c) mat) (pivot_row : int) (col : int) : ('r,'c) mat =
  let r = rows_int m in
  let c = cols_int m in
  if pivot_row < 0 || pivot_row >= r then invalid_arg "Matrix.eliminate_column: oob";
  if col < 0 || col >= c then invalid_arg "Matrix.eliminate_column: bad col";

  let pivot = Array.init c (fun j -> get m pivot_row j) in

  let data =
    Array.init (r * c) (fun idx ->
      let i = idx / c in
      let j = idx mod c in
      let x = get m i j in
      if i = pivot_row then x
      else
        let factor = get m i col in
        K.sub x  (K.mul factor pivot.(j))
    )
  in
  T.make m.T.dims data



let row_echelon (m : ('r,'c) mat) : ('r,'c) mat =
  let r = rows_int m in
  let c = cols_int m in

  let eliminate_below (m0:('r,'c) mat) (pivot_row:int) (col:int) : ('r,'c) mat =
    let pivot = Array.init c (fun j -> get m0 pivot_row j) in
    let data =
      Array.init (r * c) (fun idx ->
        let i = idx / c in
        let j = idx mod c in
        let x = get m0 i j in
        if i <= pivot_row then x
        else
          let factor = get m0 i col in
          K.sub x (K.mul factor pivot.(j))
      )
    in
    T.make m0.T.dims data
  in

  let rec row_echelon_rec (row:int) (col:int) (newM:('r,'c) mat) : ('r,'c) mat =
    if row >= r || col >= c then newM
    else
      match find_pivot_row newM row col with
      | None -> row_echelon_rec row (col + 1) newM
      | Some p ->
          let m1 = swap_rows newM row p in
          let m2 = eliminate_below m1 row col in
          row_echelon_rec (row + 1) (col + 1) m2
  in
  row_echelon_rec 0 0 (copy m)

let rref (m : ('r,'c) mat) : ('r,'c) mat =
  let r = rows_int m in
  let c = cols_int m in

  let rec rref_rec (row:int) (col:int) (newM:('r,'c) mat) : ('r,'c) mat =
    if row >= r || col >= c then newM
    else
      match find_pivot_row newM row col with
      | None -> rref_rec row (col + 1) newM
      | Some p ->
          let m1 = swap_rows newM row p in
          let m2 = normalize_row m1 row col in
          let m3 = eliminate_column m2 row col in
          rref_rec (row + 1) (col + 1) m3
  in
  rref_rec 0 0 (copy m)


  let rang (m:('r,'c) mat) : int =
    let rows = rows_int m in
    let cols = cols_int m in
    let m2 = rref m in
    let rec rang_rec i acc =
      if (i < rows)
      then rang_rec (i + 1) (if (not (Array.exists (fun x -> x <> K.zero) (Array.sub m2.data (i * cols) (cols)))) then acc + 1 else acc )
      else acc
    in rang_rec 0 0

  let isSquare (m:('r,'c) mat) : bool = rows_int m = cols_int m

  let determinant (m : ('r,'r) mat) : K.t =
    if not (isSquare m) then invalid_arg "determinant: matrix is not square";

  let n = rows_int m in
  let c = cols_int m in

  let eliminate_below_pivot (m0:('r,'c) mat) (pivot_row:int) (col:int) : ('r,'c) mat =
    let pivot_val = get m0 pivot_row col in
    let pivot = Array.init c (fun j -> get m0 pivot_row j) in

    let data =
      Array.init (n * c) (fun idx ->
        let i = idx / c in
        let j = idx mod c in
        let x = get m0 i j in
        if i <= pivot_row then x
        else
          let a_ic = get m0 i col in
          if a_ic = K.zero then x
          else
            let factor = K.div a_ic pivot_val in
            K.sub x (K.mul factor pivot.(j))
            )
            in
    T.make m0.T.dims data
    in

  let rec det_rec (k:int) (sign:K.t) (acc:K.t) (mm:('r,'c) mat) : K.t =
    if k >= n then K.mul sign acc
    else
      match find_pivot_row mm k k with
      | None -> K.zero
      | Some p ->
          let sign', mm1 =
            if p = k then (sign, mm)
            else (K.neg sign, swap_rows mm k p)
  in
          let pivot_val = get mm1 k k in
          if pivot_val = K.zero then K.zero
          else
            let mm2 = eliminate_below_pivot mm1 k k in
            det_rec (k + 1) sign' (K.mul acc pivot_val) mm2
            in

  det_rec 0 K.one K.one (copy m)

  let print (m : ('r,'c) mat) : unit =
    let r = rows_int m in
    let c = cols_int m in
    let print_row i =
      let rec print_el_rec j =
        if j < c then (
          K.print (get m i j);
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
