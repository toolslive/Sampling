open S
(* after a suggestion of Fabrice Le Fessant on our blog *)
open R

module SFabrice = (struct 
  type t = { pop: int array ; 
             smp: int array;
             mutable counter: int
           }

  let create n p = { pop = Array.init p (fun i -> i);
                     smp = Array.make n 0;
                     counter = p;
                   }
  let iter t f = Array.iter f t.smp

  let fill_sample sample n p =
    let stop = p - n in
    let rec loop i j = 
      if j = stop 
      then ()
      else
        let x = random_range sample.counter in
        let () = sample.smp.(i) <- sample.pop.(x) in
        let () = sample.counter <- sample.counter -1 in
        let () = sample.pop.(x) <- sample.pop.(sample.counter) in
        loop (i+1) (j-1)
    in
    loop 0 p

  let clear_sample sample = 
    let pop = sample.pop in
    let p = Array.length pop in
    let rec loop i = 
      if i = p then ()
      else
        let () = pop.(i) <- i in
        loop (i+1)
    in
    loop 0;
    sample.counter <- p
    
end : S)
