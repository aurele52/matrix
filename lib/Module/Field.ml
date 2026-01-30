module type FIELD = sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val egal : t -> t -> bool
  val abs  : t -> t
  val print : t -> unit
end
