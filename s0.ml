open R
open S
module S0 = (struct 
    type t = {mutable c: int; es:int array}
    
    let create n p = {c = 0; es = Array.make n 0}

    let in_sample t x = 
      let rec loop i = 
        if i < 0 then false
        else
          if t.es.(i) = x 
          then true
          else loop (i-1)
      in 
      loop (t.c -1)

    let add2_sample t x = 
      let i = t.c in
      t.es.(i) <- x;
      t.c <- i+1        

    let fill_sample sample n p = 
      let rec loop i = 
        if i = 0 
        then ()
        else
          let rec find_new () = 
            let x = random_range p in
            if in_sample sample x 
            then find_new()
            else add2_sample sample x
          in
          let () = find_new () in
          loop (i-1)
      in
      loop n

    let clear_sample t = t.c <- 0
  
    let iter t f = 
      let rec loop i =
        if i = t.c 
        then ()
        else 
          let () = f t.es.(i) in
          loop (i+1)
      in
      loop 0
end : S)
