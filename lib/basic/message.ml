type t = Msg of Id.t * Id.t * Msg.t

let compare (Msg (f1, t1, m1)) (Msg (f2, t2, m2)) =
  let from_cmp = Id.compare f1 f2 in
  let to_cmp = Id.compare t1 t2 in
  if from_cmp < 0 || from_cmp > 0 then from_cmp
  else if to_cmp < 0 || to_cmp > 0 then to_cmp
  else compare m1 m2

let view (Msg (sndr, rcvr, msg)) =
  String.concat ""
    [ "Msg(from: "; Id.view sndr; ", to: "; Id.view rcvr; ", "; Msg.view msg; ")" ]