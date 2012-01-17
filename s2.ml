open S

module S2 = (struct
  type t = { mutable n: int;
             mutable dense: int array;
             mutable sparse: int array;}

  let create n p = 
    { n = 0;
      dense = Array.make p 0;
      sparse = Array.make p 0;
    }

  let add2_sample t x = 
    let n = t.n in
    t.dense.(n) <- x;
    t.sparse.(x) <- n;
    t.n <- (n+1)

  let in_sample t x = 
    let rsi = t.sparse.(x) in
    let ok = rsi < t.n in
    ok && (t.dense.(rsi) = x)

  let iter t f =
    let n = t.n in
    let rec loop i =
      if i = n then ()
      else
        let x = t.dense.(i) in
        let () = f x in
        loop (i+1) 
    in
    loop 0

  let clear_sample t = t.n <- 0

  let fill_sample sample n p = 
    let rec loop i = 
      if i = 0 
      then ()
      else
        let rec find_new () = 
          let x = R.random_range p in
          if in_sample sample x 
          then find_new()
          else add2_sample sample x
        in
        let () = find_new () in
        loop (i-1)
    in
    loop n
    
end : S)


