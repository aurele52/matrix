module MakeVspace (X : Vspace.VSPACE)
  : Vspace.VSPACE with module K = X.K and type 'd t = 'd X.t
= struct
  include X
end

