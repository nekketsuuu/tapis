type 'a sbst = (string * 'a) list

(* (compose s t)(x) = s(t(x)) *)
let compose (sigma1 : 'a sbst) (sigma2 : 'a sbst) =
  (* Just use List.append since we assume it's searched from the head *)
  (* We can't use List.rev_append *)
  List.append sigma1 sigma2
