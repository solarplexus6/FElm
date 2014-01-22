module Gensym

let private count = ref 0

let next () =
  incr count;
  "_var" + string !count
