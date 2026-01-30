type nil = Nil
type ('x,'xs) cons = Cons

type _ dims =
  | DNil  : nil dims
  | DCons : 'n Nat_number.nat * 'ds dims -> ('n, 'ds) cons dims

