module Gensym

open AbstractSyntax

let private count = ref 0

let next (x' : varname) =
  incr count;
  let x = (x'.Split[|'^'|]).[0]
  in x + "^" + string !count
