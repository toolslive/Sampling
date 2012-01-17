open S
open R

module S1 = (struct
  type t = bool array
  let create n p = Array.make p false
  let in_sample t x = t.(x) 

  let add2_sample t x = t.(x) <- true

  let clear_sample t = 
    let rec loop i = 
      if i < 0 then ()
      else
        let () = t.(i) <- false in
        loop (i-1) 
    in
    loop (Array.length t -1)

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
  let iter t f =
    let s = Array.length t in
    let rec loop i = 
      if i = s then ()
      else
        let () = if t.(i) then f i in
        loop (i+1)
    in
    loop 0

end : S)
