
module MakeScalar (K : Field.FIELD) = struct
  module K = K

  type 'd t = K.t

  let add = K.add
  let sub = K.sub

  let scale (k:K.t) (x:K.t) = K.mul k x

  let zero_like (_:'d t) = K.zero

  let equal = K.egal

  let map f x = f x
  let fold f x init = f x init

  let print (x:'d t) : unit =
    K.print x
end
