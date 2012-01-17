module type S = sig
  type t
  val create : int -> int -> t
  val fill_sample: t -> int -> int -> unit
  val clear_sample: t -> unit
  val iter: t -> (int -> unit) -> unit
end
