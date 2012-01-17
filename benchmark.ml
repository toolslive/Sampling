open S
open S0
open S1
open S2

let modules = ["S0", (module S0: S);
               "S1", (module S1: S);
               "S2", (module S2: S);
              ];;

let micro_bench (which:string) ns k p  = 
  let module Sampler = (val List.assoc which modules : S) in
  let test_one n =
    let sample = Sampler.create n p in
    let use_choice _ = () in
    let rec loop k = 
      if k = 0 
      then ()
      else 
        begin
          if k mod 1000 = 0 then Printf.eprintf ".%!";
          let () = Sampler.fill_sample sample n p in
          let () = Sampler.iter sample use_choice in
          let () = Sampler.clear_sample sample in
          loop (k-1)
        end
    in
    let t0 = Unix.gettimeofday() in
    let () = loop k in
    let t1 = Unix.gettimeofday() in
    let d = t1 -. t0 in
    Printf.printf "%i | %f \n" n d
  in
  List.iter test_one ns;;


let main () =  
  let which = ref "S0" in
  let () = Arg.parse
    [("--module", Arg.Set_string which, Printf.sprintf "which module to use (%s)" !which)]
    (fun s -> raise (Arg.Bad s))
    ("usage: read the code")
  in
  let k = 100 * 1000 in
  let p = 10000 in
  micro_bench !which [1;2;3;4;5;6;7;8;9;10;20;40;80;160;320;640;1000;2000;2500] k p;;

let () = main ();;




