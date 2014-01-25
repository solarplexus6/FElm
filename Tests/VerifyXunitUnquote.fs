module VerifyXunitUnquote

open Swensen.Unquote
open Xunit

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>

[<Fact>]
let ``test xunit support, this should pass`` () =
    test <@ 22 - 17 = 5 @>