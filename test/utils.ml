open Printf

let all_tests = ref true

let failed_tests = ref []

let printf_green = printf "\027[38;5;2m%s\027[0m"

let printf_red = printf "\027[38;5;1m%s\027[0m"

let print_res ~group name res =
  all_tests := !all_tests && res ;
  if res then printf_green @@ sprintf "    %s test PASSED\n" name
  else (
    printf_red @@ sprintf "    %s test FAILED\n" name ;
    failed_tests := (group, name) :: !failed_tests )

let rec print_all name =
  let name = String.uppercase_ascii name in
  if !all_tests then printf_green @@ sprintf "\nALL %s TESTS PASSED" name
  else (
    printf_red @@ sprintf "\nTHE FOLLOWING %s TEST(S) FAILED:\n" name ;
    print_failed () )

and print_failed () =
  let open List in
  iter (fun (grp, name) -> printf "\027[38;5;1m- %s: %s\027[0m\n" grp name)
  @@ rev !failed_tests

(** reset [all_tests] and [failed_tests] *)
let test_file name =
  all_tests := true ;
  failed_tests := [] ;
  printf "**%s**\n" name

let test_group ~group ~desc = printf "%s\n  %s\n" group desc

module type Testable = sig
  val group : string

  val desc : string

  val tests : (string * (unit -> bool)) list
end

let run_tests (module T : Testable) =
  test_group ~group:T.group ~desc:T.desc ;
  List.iter
    (fun (name, test) -> print_res ~group:T.group name @@ test ())
    T.tests
