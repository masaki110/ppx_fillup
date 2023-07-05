[@@@warnerror "-34"]

(* A convenient name for a unary type constructor *)
module type TyCon = sig
  type 'a tc
end

(* An interface to a weak form of equality that doesn't support Leibniz's law *)
module type WeakEQ = sig
  (* A value of type (s, t) eq is a proof that types s and t are the same. *)
  type ('a, 'b) eq

  (* The reflexivity axiom. *)
  val refl : unit -> ('a, 'a) eq

  (* The symmetry axiom *)
  val symm : ('a, 'b) eq -> ('b, 'a) eq

  (* The transitivity axiom *)
  val trans : ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq

  (* Given a proof that type s and type t are equal, we can convert s to t. *)
  val cast : ('a, 'b) eq -> 'a -> 'b
end

(* An implementation of the weak form of equality.  This is the same as the
   Haskell implementation given in

     Implementing Cut Elimination: A Case Study of Simulating Dependent
     Types in Haskell
     Chiyan Chen, Dengping Zhu and Hongwei Xi
     PADL 2004
*)
module WeakEq : WeakEQ = struct
  type ('a, 'b) eq = ('a -> 'b) * ('b -> 'a)

  let refl () = ((fun x -> x), fun x -> x)
  let symm (f, g) = (g, f)
  let trans (f, g) (j, k) = ((fun x -> j (f x)), fun x -> g (k x))
  let cast (f, _) = f
end

(* An interface to equality with Leibniz's law as an axiom.  We don't include
   symm and trans, since they are derivable from refl, Subst and cast (see
   below) *)
module type EQ = sig
  (* A value of type (s, t) eq is a proof that types s and t are the same. *)
  type ('a, 'b) eq

  (* The reflexivity axiom. *)
  val refl : unit -> ('a, 'a) eq

  (* Leibniz's substitution axiom. *)
  module Subst (TC : TyCon) : sig
    val subst : ('a, 'b) eq -> ('a TC.tc, 'b TC.tc) eq
  end

  (* Given a proof that type s and type t are equal, we can convert s to t. *)
  val cast : ('a, 'b) eq -> 'a -> 'b
end

(* An implementation of Leibniz equality.  This is analogous to the Haskell
   implementation given in

     Typing Dynamic Typing
     Arthur I. Baars and S. Doaitse Swierstra
     ACM SIGPLAN Notices, volume 37 issue 9, 2002
*)
module Eq : EQ = struct
  (* In Haskell: data EqTC a b = Cast{cast :: forall tc. tc a -> tc b} *)
  module type EqTC = sig
    type a
    type b

    module Cast : functor (TC : TyCon) -> sig
      val cast : a TC.tc -> b TC.tc
    end
  end

  type ('a, 'b) eq = (module EqTC with type a = 'a and type b = 'b)

  let refl (type t) () =
    (module struct
      type a = t
      type b = t

      module Cast (TC : TyCon) = struct
        let cast v = v
      end
    end : EqTC
      with type a = t
       and type b = t)

  let cast (type s t) s_eq_t =
    let module S_eqtc = (val s_eq_t : EqTC with type a = s and type b = t) in
    let module C = S_eqtc.Cast (struct
      type 'a tc = 'a
    end) in
    C.cast

  module Subst (TC : TyCon) = struct
    let subst (type s t) s_eq_t =
      (module struct
        type a = s TC.tc
        type b = t TC.tc

        module S_eqtc = (val s_eq_t : EqTC with type a = s and type b = t)

        module Cast (SC : TyCon) = struct
          module C = S_eqtc.Cast (struct
            type 'a tc = 'a TC.tc SC.tc
          end)

          let cast = C.cast
        end
      end : EqTC
        with type a = s TC.tc
         and type b = t TC.tc)
  end
end

include Eq

(* We can obtain the symmetry property from subst, refl and cast. *)
let symm : 'a 'b. ('a, 'b) eq -> ('b, 'a) eq =
  fun (type a b) a_eq_b ->
   let module S = Subst (struct
     type 'a tc = ('a, a) eq
   end) in
   cast (S.subst a_eq_b) (refl ())

(* We can obtain the transitivity property from subst and cast. *)
let trans : 'a 'b 'c. ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq =
  fun (type a b c) a_eq_b b_eq_c ->
   let module S = Subst (struct
     type 'a tc = (a, 'a) eq
   end) in
   cast (S.subst b_eq_c) a_eq_b

(* Our implementation of equality seems sufficient for the common examples,
   but has one apparent limitation, described below.

   A few examples seem to require an inverse of Leibniz's law.  For injectivty
   type constructors t, we would like to have

   ('a t, 'b t) eq -> ('a, 'b) eq

   For example, given a proof that two function types are equal, we would like
   to extract proofs that the domain and codomain types are equal:

   ('a -> 'b, 'c -> 'd) eq -> ('a, 'c) eq * ('b, 'd) eq

   GADTs themselves support type decomposition in this way.
*)

(* Unfortunately, injectivity is supported only for WeakEq.eq.
   We may always get WeakEq.eq from EQ.eq.
*)
let degrade : 'r 's. ('r, 's) eq -> ('r, 's) WeakEq.eq =
  fun (type r s) r_eq_s ->
   let module M = Eq.Subst (struct
     type 'a tc = ('a, r) WeakEq.eq
   end) in
   WeakEq.symm (cast (M.subst r_eq_s) (WeakEq.refl ()))
