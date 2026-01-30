module type VSPACE = sig
  module K : Field.FIELD
  type 'd t

  val add  : 'd t -> 'd t -> 'd t
  val sub  : 'd t -> 'd t -> 'd t
  val scale : K.t -> 'd t -> 'd t
  val zero_like : 'd t -> 'd t
  val equal : 'd t -> 'd t -> bool

  val map  : (K.t -> K.t) -> 'd t -> 'd t
  val fold : (K.t -> 'a -> 'a) -> 'd t -> 'a -> 'a
end

