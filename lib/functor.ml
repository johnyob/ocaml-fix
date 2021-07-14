module type S = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type S2 = sig
  type ('a, 'b) t

  val map : ('a, 'c) t -> f:('a -> 'b) -> ('b, 'c) t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val map : ('a, 'c, 'd) t -> f:('a -> 'b) -> ('b, 'c, 'd) t
end