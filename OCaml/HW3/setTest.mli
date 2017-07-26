module SetTest :
  functor (SetImpl : SetInterface.SET) ->
    sig
      val run_test : string -> (unit -> bool) -> unit
      val run_failing_test : string -> (unit -> bool) -> unit
      val test : unit -> bool
    end
module TestOLSet :
  sig
    val run_test : string -> (unit -> bool) -> unit
    val run_failing_test : string -> (unit -> bool) -> unit
    val test : unit -> bool
  end
module TestBSTSet :
  sig
    val run_test : string -> (unit -> bool) -> unit
    val run_failing_test : string -> (unit -> bool) -> unit
    val test : unit -> bool
  end
