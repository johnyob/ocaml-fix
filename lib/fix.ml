module type S = sig
  module F : Functor.S

  type t = Fix of t F.t

  val fix : t F.t -> t
  val unfix : t -> t F.t
  val hoist : t -> f:(t F.t -> t F.t) -> t
  val fold : t -> f:('a F.t -> 'a) -> 'a
  val unfold : 'a -> f:('a -> 'a F.t) -> t
end

module type S1 = sig
  module F : Functor.S2

  type 'a t = Fix of ('a t, 'a) F.t

  val fix : ('a t, 'a) F.t -> 'a t
  val unfix : 'a t -> ('a t, 'a) F.t
  val hoist : 'a t -> f:(('a t, 'a) F.t -> ('a t, 'a) F.t) -> 'a t
  val fold : 'a t -> f:(('b, 'a) F.t -> 'b) -> 'b
  val unfold : 'a -> f:('a -> ('a, 'b) F.t) -> 'b t
end

module type S2 = sig
  module F : Functor.S3

  type ('a, 'b) t = Fix of (('a, 'b) t, 'a, 'b) F.t

  val fix : (('a, 'b) t, 'a, 'b) F.t -> ('a, 'b) t
  val unfix : ('a, 'b) t -> (('a, 'b) t, 'a, 'b) F.t
  val hoist : ('a, 'b) t -> f:((('a, 'b) t, 'a, 'b) F.t -> (('a, 'b) t, 'a, 'b) F.t) -> ('a, 'b) t
  val fold : ('a, 'b) t -> f:(('c, 'a, 'b) F.t -> 'c) -> 'c
  val unfold : 'a -> f:('a -> ('a, 'b, 'c) F.t) -> ('b, 'c) t
end

module Make2 (F : Functor.S3) : S2 with module F := F = struct
  type ('a, 'b) t = Fix of (('a, 'b) t, 'a, 'b) F.t

  let fix t = Fix t [@@inline]
  let unfix (Fix t) = t [@@inline]

  let hoist t ~f =
    let rec loop t = unfix t |> F.map ~f:loop |> f |> fix in
    loop t


  let fold t ~f =
    let rec loop t = unfix t |> F.map ~f:loop |> f in
    loop t


  let unfold t ~f =
    let rec loop t = f t |> F.map ~f:loop |> fix in
    loop t
end

module Make1 (F : Functor.S2) = Make2 (struct
  type ('a, 'b, _) t = ('a, 'b) F.t

  include (F : Functor.S3 with type ('a, 'b, _) t := ('a, 'b) F.t)
end)

module Make (F : Functor.S) = Make1 (struct
  type ('a, _) t = 'a F.t

  include (F : Functor.S2 with type ('a, _) t := 'a F.t)
end)

