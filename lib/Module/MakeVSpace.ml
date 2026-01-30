module MakeVspace (X : Type.Vspace.VSPACE)
  : Type.Vspace.VSPACE with module K = X.K and type 'd t = 'd X.t
= struct
  include X
end

