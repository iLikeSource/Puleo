module MatrixComponent :
  sig type t = int * float val multiply : t list -> t list -> float end
module MatrixSource : sig type t = int * int * float end
module type MatrixType =
  sig
    type t
    val get : t -> int * int -> float
    val set : t -> int * int * float -> unit
    val add : t -> int * int * float -> unit
    val rows : t -> MatrixComponent.t list array
    val columns : t -> MatrixComponent.t list array
    val row : t -> int -> MatrixComponent.t list
    val column : t -> int -> MatrixComponent.t list
    val to_sources : t -> MatrixSource.t list
    val of_sources : MatrixSource.t list -> t
  end
module Matrix :
  functor (M : MatrixType) ->
    sig
      type t = M.t
      val add_mm : t -> t -> M.t
      val multiply_mm : t -> t -> float array array
    end
