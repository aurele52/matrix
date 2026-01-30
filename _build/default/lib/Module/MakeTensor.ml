module MakeTensorND (K : Type.Field.FIELD) = struct
  module K = K

  type 'ds t = {
    dims : 'ds Type.Dims.dims;
    data : K.t array;
  }

let rec int_of_nat : type n. n Type.Nat_number.nat -> int = function
  | Zero -> 0
  | Succ n -> 1 + int_of_nat n

  let rec dims_to_ints : type ds. ds Type.Dims.dims -> int list = function
    | DNil -> []
    | DCons (d, rest) -> int_of_nat d :: dims_to_ints rest

  let numel (type ds) (ds : ds Type.Dims.dims) : int =
    List.fold_left ( * ) 1 (dims_to_ints ds)

  let make : type ds. ds Type.Dims.dims -> K.t array -> ds t =
    fun dims data ->
      let n = numel dims in
      if Array.length data <> n then invalid_arg "Tensor.make: bad dimension";
      { dims; data }

  let init (dims : 'ds Type.Dims.dims) (f : int array -> K.t) : 'ds t =
    let shape = Array.of_list (dims_to_ints dims) in (* [2, 5, 1]*)
    let n = Array.fold_left ( * ) 1 shape in (*10*)

    let idx_of_k k = (*0*)
      let len = Array.length shape in (*3*)
      let idx = Array.make len 0 in (*[0,0,0]*)
      let rec fill d x = (*fill 2 0*)
        if d < 0 then ()
        else
          let m = shape.(d) in
          idx.(d) <- x mod m;
        fill (d - 1) (x / m)
          in
    fill (len - 1) k;
    idx
      in
  { dims; data = Array.init n (fun k -> f (idx_of_k k)) }


  let fold (f:K.t -> 'a -> 'a) (m:'ds t) (init:'a) : 'a =
    Array.fold_right f m.data init

  let map (f:K.t -> K.t) (m:'ds t) : 'ds t =
    { m with data = Array.map f m.data }

  let same_shape (a:'ds t) (b:'ds t) : bool =
    dims_to_ints a.dims = dims_to_ints b.dims

  let add (a:'ds t) (b:'ds t) : 'ds t =
    if not (same_shape a b) then invalid_arg "Tensor.add: shape mismatch";
    { a with data = Array.init (Array.length a.data) (fun i -> K.add a.data.(i) b.data.(i)) }

  let sub (a:'ds t) (b:'ds t) : 'ds t =
    if not (same_shape a b) then invalid_arg "Tensor.sub: shape mismatch";
    { a with data = Array.init (Array.length a.data) (fun i -> K.sub a.data.(i) b.data.(i)) }

  let scale (k:K.t) (m:'ds t) : 'ds t =
    map (fun x -> K.mul x k) m

  let zero_like (m:'ds t) : 'ds t =
    { m with data = Array.make (Array.length m.data) K.zero }

  let equal (a:'ds t) (b:'ds t) : bool =
    if not (same_shape a b) then false
    else
      let n = Array.length a.data in
      let rec loop i =
        i = n || (K.egal a.data.(i) b.data.(i) && loop (i+1))
      in
      loop 0
end
