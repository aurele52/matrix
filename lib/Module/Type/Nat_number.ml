type z = Z
type 'n s = S

type _ nat =
  | Zero : z nat
  | Succ : 'n nat -> 'n s nat
