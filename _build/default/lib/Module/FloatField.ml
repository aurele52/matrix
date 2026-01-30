
module FloatField : Field.FIELD with type t = float = struct
  type t = float
  let zero = 0.0
  let one  = 1.0
  let add  = ( +. )
  let sub  = ( -. )
  let mul  = ( *. )
  let egal = (=)
  let abs = Float.abs
  let print = print_float
end
