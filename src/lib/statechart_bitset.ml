type t = int array

(* Yeah this wastes space... oh well. it's portable *)
let bucket_size = 16
(* nth power of 2 *)
let bucket_addr = 4
let bucket_mask = bucket_size - 1

let make size =
  Array.make (size / bucket_size + 1) 0

let get a idx =
  (a.(idx lsr bucket_addr) land (1 lsl (idx land bucket_mask))) != 0

let set a idx =
  let a_idx = idx lsr bucket_addr in
  a.(a_idx) <- a.(a_idx) lor (1 lsl (idx land bucket_mask));
  ()

let clear a idx =
  let a_idx = idx lsr bucket_addr in
  a.(a_idx) <- a.(a_idx) lxor (1 lsl (idx land bucket_mask));
  ()

let has_and a b =
  let rec search i =
    if i < 0 then false else (
    if a.(i) land b.(i) > 0
    then true
    else (search (i - 1))
  ) in
  search (Array.length(a) - 1)

let clear_all a =
  for i = 0 to (Array.length(a) - 1) do
    a.(i) <- 0
  done

let has_any a =
  let rec search i =
    if i < 0 then false else (
    if a.(i) > 0
    then true
    else (search (i - 1))
  ) in
  search (Array.length(a) - 1)

let copy a =
  Array.copy a

let copy_clear a =
  Array.make (Array.length a) 0

let bor a b =
  for i = 0 to (Array.length(a) - 1) do
    a.(i) <- a.(i) lor b.(i)
  done

let bxor a b =
  for i = 0 to (Array.length(a) - 1) do
    a.(i) <- a.(i) lxor b.(i)
  done

let band a b =
  for i = 0 to (Array.length(a) - 1) do
    a.(i) <- a.(i) land b.(i)
  done

let iter_left fn a =
  for i = 0 to (Array.length(a) - 1) do
    let v = a.(i) in
    for j = 0 to bucket_mask do
      if v land (1 lsl (j land bucket_mask)) > 0
      then fn ((i * bucket_size) + j)
    done
  done

let iter_right fn a =
  for i = (Array.length(a) - 1) downto 0 do
    let v = a.(i) in
    for j = bucket_mask downto 0 do
      if v land (1 lsl (j land bucket_mask)) > 0
      then fn ((i * bucket_size) + j)
    done
  done

let fold_left fn acc a =
  let acc = ref acc in
  for i = 0 to (Array.length(a) - 1) do
    let v = a.(i) in
    for j = 0 to bucket_mask do
      if v land (1 lsl (j land bucket_mask)) > 0
      then acc := fn !acc ((i * bucket_size) + j)
    done
  done;
  !acc

let fold_right fn acc a =
  let acc = ref acc in
  for i = (Array.length(a) - 1) downto 0 do
    let v = a.(i) in
    for j = bucket_mask downto 0 do
      if v land (1 lsl (j land bucket_mask)) > 0
      then acc := fn !acc ((i * bucket_size) + j)
    done
  done;
  !acc

let first a =
  let n = Array.length(a) * bucket_size in
  let rec search i =
    if i > n then None else (
    if get a i
    then Some i
    else search (i + 1)
  ) in
  search 0

let of_list l =
  Array.of_list l

let to_list a =
  Array.to_list a

let to_idx_array a =
  let l = fold_right (fun acc idx ->
    idx :: acc
  ) [] a in
  Array.of_list l

let of_idx_array a size =
  let bs = make size in
  Array.iter (set bs) a;
  bs