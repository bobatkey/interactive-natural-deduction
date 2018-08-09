let index_of x xs =
  let rec find idx = function
    | []      -> None
    | y :: xs -> if x = y then Some idx else find (idx+1) xs
  in
  find 0 xs

let index_of_exn x xs =
  match index_of x xs with
    | None     -> raise Not_found
    | Some idx -> idx

let numbers n =
  let rec loop i = if i = n then [] else i :: loop (i+1) in
  loop 0

(**********************************************************************)
type mvar = string

let mvar_equal = String.equal

module MVarMap = Map.Make (String)

(* FIXME: thread the fresh name generation through the
   computations. *)
let freshname =
  let next = ref 0 in
  fun () ->
    let name = "X" ^ string_of_int !next in incr next; name

(**********************************************************************)
type constr = string

let constr_equal = String.equal

(**********************************************************************)
type tm =
  | Var  of int
  | Con of constr * binding_tm list
  | Unk of mvar * int list (* all the variables must be distinct *)

and binding_tm =
  { binders : int
  ; term    : tm
  }

let nobind term =
  { binders = 0; term }

module Term = struct
  type t = tm

  let rec to_string = function
    | Var i          -> "b" ^ string_of_int i
    | Con (c, [])    -> c
    | Con (c, tms)   -> c ^ "(" ^ String.concat "," (List.map bindings_tm_to_string tms) ^ ")"
    | Unk (mv, [])   -> "?" ^ mv
    | Unk (mv, vars) -> "?" ^ mv ^ "[" ^ String.concat "," (List.map (Printf.sprintf "b%d") vars) ^ "]"
  and bindings_tm_to_string = function
    | {binders = 0; term} -> to_string term
    | {binders = n; term} -> Printf.sprintf "%d.%s" n (to_string term)
end

(**********************************************************************)
(* Lambda-calculus *)
let lam m   = Con ("lam", [{binders = 1; term = m}])
let var i   = Var i
let app m n = Con ("app", [nobind m; nobind n])
let const c = Con (c, [])

(* The ground term '(\x. f x) (x y)' *)
let redex =
  app (lam (app (const "f") (var 0))) (app (const "x") (const "y"))

(* The pattern *)
let redex_pat =
  app (lam (Unk ("X", [0]))) (Unk ("Y", []))

let pat =
  app (Unk ("X", [])) (Unk ("X", []))

let tm =
  app (lam (var 0)) (lam (var 0))

(**********************************************************************)
(* Nipkow's example *)
let lhs = lam (lam (Unk ("F", [1])))
let rhs = lam (lam (Con ("c", [nobind (Unk ("G", [0; 1]))])))

(**********************************************************************)
(* Instantiation of metavariable definitions: applying a renaming.  *)
let instantiate vars tm =
  let instantiate_var offset i =
    if i < offset then i
    else List.nth vars (i - offset) + offset
  in
  let rec instantiate offset = function
    | Var i ->
       Var (instantiate_var offset i)
    | Con (c, ms) ->
       Con (c, List.map (instantiate_binding_tm offset) ms)
    | Unk (mv, vars) ->
       Unk (mv, List.map (instantiate_var offset) vars)

  and instantiate_binding_tm offset { binders; term } =
    { binders; term = instantiate (offset+binders) term }
  in
  instantiate 0 tm

(**********************************************************************)
(* Substituting for a metavariable. FIXME: should also do beta
   reduction / other normalisation, or should that happen later on? *)
let rec subst_mv (mv, m as defn) = function
  | Var i ->
     Var i
  | Con (c, ms) ->
     Con (c, List.map (subst_mv_binding defn) ms)
  | Unk (mv', vs) ->
     if mvar_equal mv mv' then instantiate vs m else Unk (mv', vs)

and subst_mv_binding defn { binders; term } =
  { binders; term = subst_mv defn term }

let apply_subst subst f =
  List.fold_right subst_mv subst f

(**********************************************************************)
let rec occurs mv = function
  | Var i        -> false
  | Con (_, tms) -> List.exists (occurs_binder mv) tms
  | Unk (mv', _) -> mvar_equal mv mv'
and occurs_binder mv {term} =
  occurs mv term

(**********************************************************************)
(* Unification, following Nipkow's "Functional Unification of
   Higher-Order Patterns", LICS'93. This implementation follows the
   "Unification by transformation" rules in Figure 1 quite closely,
   except that (FIXME) the current substitution and problem set are
   updated eagerly as metavariables are solved. *)

let update_subst binding =
  List.map (fun (mv, m) -> (mv, subst_mv binding m))

let update_problems binding =
  List.map (fun (m, m') -> (subst_mv binding m, subst_mv binding m'))

(* FIXME:
   - lazy updating of the substitution and problem list
   - factor out the worklist loop and production of new problems
   - use Maps to represent substitutions
   - thread fresh metavariable substitution throughout, instead of using side effects
   - Abstract constructor names, instead of strings
   - Generic sort checking algorithm

   FIXME (harder: might be ways of dealing with rigid-rigid unification errors):
   - Incorporation of eta rules (might be able to make progress when one constructor is a lam or a pair)
   - Incorporation of beta rules (instantiation of a metavariable might cause some computation, but this might generate constraints)
   - Incorporation of constraints (?X[xs] := ?Y[m1,...,mk]) -- if the rhs ever reduces to a pattern term, then we can make progress.
   - Incorporation of type information; blocking problems pending type info.
*)

(* If unification fails, and one side is an elimination, try reducing
   it? *)

let rec unify subst = function
  | [] ->
     Ok subst

  | (Unk (mv, args1), Unk (mv', args2)) :: problems when mvar_equal mv mv' ->
     if args1 = args2 then
       unify subst problems
     else
       let h = freshname () in
       let rev_args, _ =
         List.fold_left2
           (fun (rev_args, idx) arg1 arg2 ->
              (if arg1 = arg2 then idx :: rev_args else rev_args), idx+1)
           ([], 0)
           args1
           args2
       in
       let defn  = (mv, Unk (h, List.rev rev_args)) in
       let subst = defn :: update_subst defn subst in
       unify subst problems

  | (Unk (mv1, args1), Unk (mv2, args2)) :: problems ->
     (* not (mvar_equal mv1 mv2) *)
     (* intersect args1 and args2, generate a new metavariable that
        stands for both. *)
     let h      = freshname () in
     let args   = List.filter (fun i -> List.mem i args2) args1 in
     (* FIXME: fold these extractions into the production of args *)
     let args'1 = List.map (fun i -> index_of_exn i args1) args in
     let args'2 = List.map (fun i -> index_of_exn i args2) args in
     let defn1  = (mv1, Unk (h, args'1)) in
     let defn2  = (mv2, Unk (h, args'2)) in
     let subst  =
       defn1 :: defn2 :: update_subst defn1 (update_subst defn2 subst)
     in
     unify subst problems

  (* ?X[xs] =?= Var i  ==> ?X[xs] := Var i, assuming that i \in xs

     ?X[xs] =?= c(ys1.m1, ..., ysk.mk) ==>
     ?X[xs] := c(ys1.?H1[xs,ys1], ..., ysk.?Hk[xs,ysk])
     ?H1[xs,ys1] =?= m1, ..., ?Hk[xs,ysk] =?= mk
     assuming X \not\in m1, ..., mk
  *)

  | (Unk (mv, args), Var i | Var i, Unk (mv, args)) :: problems ->
     (match index_of i args with
       | None ->
          Error `VariableOutOfScope
       | Some idx ->
          let defn     = (mv, Var idx) in
          let subst    = defn :: update_subst defn subst in
          let problems = update_problems defn problems in
          unify subst problems)

  | (Unk (mv, args), (Con (c, ms) as tm) | (Con (c, ms) as tm), Unk (mv, args)) :: problems ->
     if occurs mv tm then Error `OccursCheck
     else
       (* FIXME: occurs check *)
       let rev_ms', new_problems =
         List.fold_left
           (fun (rev_ms', new_problems) { binders; term } ->
              let h = freshname () in
              let problem =
                Unk (h, numbers binders @ List.map (fun i -> i + binders) args),
                term
              in
              let m' = { binders
                       ; term = Unk (h, numbers (binders + List.length args)) }
              in
              m' :: rev_ms', problem :: new_problems)
           ([], [])
           ms
       in
       let defn     = (mv, Con (c, List.rev rev_ms')) in
       let subst    = defn :: update_subst defn subst in
       let problems = List.rev_append new_problems (update_problems defn problems) in
       unify subst problems

  (* Rigid-rigid cases *)
  | (Var i, Var j) :: problems ->
     if i = j then
       unify subst problems
     else
       Error `VarMismatch

  | (Con (c1, ms1), Con (c2, ms2)) :: problems ->
     if constr_equal c1 c2 then
       zip_problems subst ms1 ms2 problems
     else
       Error `ConstructorMismatch

  (* Rigid-rigid failures *)
  | (Var _, Con _ | Con _, Var _) :: _ ->
     Error `VarConstructorMismatch

and zip_problems subst ms ms' problems = match ms, ms' with
  | [], [] ->
     unify subst problems

  | {binders = b1; term = m1}::ms1, {binders = b2; term = m2}::ms2 ->
     if b1 = b2 then
       zip_problems subst ms1 ms2 ((m1,m2)::problems)
     else
       Error `IllSortedTerms

  | _, _ ->
     Error `IllSortedTerms
