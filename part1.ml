let rec fib (n : int) : int =
  if n < 1 then 0 else if n = 1 then 1 else
    fib (n-1) + fib (n-2)

let fib_refs n=
 if (n=0) then 0 else
 if (n=1) then 1 else
 let s0=ref 0 in
 let s1=ref 1 in
 let i=ref 1 in
 while ((!i)!=n) do (
   let tmp=(!s0) in 
   s0:=(!s1);
   s1:=(tmp+(!s1));
   i:=(!i)+1;) done;
 !s1;;

let tabulate (f : int -> 'a) (n : int) : 'a list =
  let rec helper index accum =
    if index < 0 then accum
    else let new_lst = (f index)::accum in
    helper (index - 1) new_lst in
  helper (n-1) []

let tabulate_refs (f : (int -> 'a)) (n : int) : 'a list =
 let i=ref 0 in
 let res = ref [] in
 while ((!i)<n) do (
  res:=(f (!i))::(!res);
  i:=(!i)+1;) done;
 List.rev (!res);;

let string_rev (str : string) :string =
 let i=ref 0 in
 let j= ref (String.create (String.length str)) in
 while ((!i)<(String.length str)) do (
  String.set (!j) (!i) (String.get str ((String.length str)-(!i)-1));
  i:=(!i)+1; ) done;
 !j;;

let inflate_fn (purchases : float list) (money : float) (rate : float) =
  let helper (act, hld, mlt) p = 
    (act -. p, hld +. (p /. mlt), mlt *. (1. +. rate)) in
  let (a, h, m) = List.fold_left helper (money, 0., 1.) purchases in
  (a /. m, h)

let inflate (purchases : float list) (money : float) (rate : float) =
 let i = ref 0 in
 let act = ref money in
 let hld = ref 0. in
 let mlt = ref 1. in
 while ((!i)!=(List.length purchases)) do (
  let ele = List.nth purchases (!i) in
  hld:=(!hld)+.(ele/.(!mlt));
  act:=(!act)-.ele;
  mlt:=(!mlt)*.(1.+.rate); 
  i:=(!i)+1;) done;
 (((!act)/.(!mlt)),(!hld));;
