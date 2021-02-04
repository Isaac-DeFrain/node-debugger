open Utils

let () = test_file "Property"

module Prop = struct
  let group = "Property tests"

  let desc = "description of property tests"

  let tests =
    [ ("fail1", fun () -> true);
      ("pass", fun () -> true);
      ("fail2", fun () -> true) ]
end

let () = run_tests (module Prop : Testable)

let () = print_all "property"

;;
print_newline ()

;;
print_newline ()
