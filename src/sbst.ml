type 'a sbst = (string * 'a) list

let empty : 'a sbst = []

(* (compose s t)(x) = s(t(x)) *)
let compose (sigma1 : 'a sbst) (sigma2 : 'a sbst) =
  (* Just use List.append since we assume it's searched from the head *)
  (* We can't use List.rev_append *)
  List.append sigma1 sigma2

let rec precise_compose (sigma1 : Type.level sbst)
			(sigma2 : Type.level sbst) =
  let update x1 l1 (x2, l2) =
    match l2 with
    | Type.LVar(x) when x = x1 ->
       (x2, l1)
    | _ -> (x2, l2)
  in
  if sigma1 = empty then sigma2
  else
    let (x, l) = List.hd sigma1 in
    let sigma2 = List.map (update x l) sigma2 in
    (* TODO(nekketsuuu): 大丈夫だけど、同じ要素を消したい *)
    precise_compose (List.tl sigma1) ((x, l) :: sigma2)
