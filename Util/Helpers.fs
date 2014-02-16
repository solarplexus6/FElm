module Helpers

let undefined<'T> : 'T = failwith "Not implemented yet"

let rec first p = function
    | [] -> failwith "ni mo"
    | (x :: xs) -> if p x then x else first p xs