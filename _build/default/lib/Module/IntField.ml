
module IntField : Field.FIELD with type t = int = struct
  type t = int
  let zero = 0
  let one  = 1
  let add  = ( + )
  let sub  = ( - )
  let mul  = ( * )
  let egal = (=)
  let abs = abs
  let print = print_int
end
