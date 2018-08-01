type mvar  = string
type const = string

type head =
  | Var   of int
  | Const of const

(* FIXME: should we allow variables to be applied to anything? Why not
   just plain variables on their own? Then defining substitution would
   be easier. *)
type tm =
  | Bind of tm
  | Appl of head * tm list
  | Meta of mvar * int list


(**********************************************************************)
let lam m   = Appl (Const "lam", [Bind m])
let var i   = Appl (Var i, [])
let app m n = Appl (Const "app", [m; n])
let const c = Appl (Const c, [])

(* The ground term '(\x. f x) (x y)' *)
let redex =
  app (lam (app (const "f") (var 0))) (app (const "x") (const "y"))

(* The pattern *)
let redex_pat =
  app (lam (Meta ("X", []))) (Meta ("Y", []))

(**********************************************************************)
(* Preparation of a term to be a definition *)
let prepare_var vars offset i =
  if i < offset then i
  else
    let i = i - offset in
    let rec find idx = function
      | [] ->
         failwith "preparation failed: variables don't match up"
      | j :: js ->
         if i = j then idx else find (idx+1) js
    in
    find 0 vars + offset

let prepare_head vars offset = function
  | Var i   -> Var (prepare_var vars offset i)
  | Const c -> Const c

let rec prepare vars offset = function
  | Bind m ->
     Bind (prepare vars (offset+1) m)
  | Appl (h, ms) ->
     Appl (prepare_head vars offset h,
           List.map (prepare vars offset) ms)
  | Meta (mv, vars) ->
     (* FIXME: do an occurs check here *)
     (* FIXME: could do pruning here: if any of the variables required
        are not accessble, then create new metavariables to replace
        the old ones. *)
     Meta (mv, List.map (prepare_var vars offset) vars)

(**********************************************************************)
(* Instantiation of metavariable definitions *)
let instantiate_var vars offset i =
  if i < offset then i
  else List.nth vars (i - offset) + offset

let instantiate_head vars offset = function
  | Var i   -> Var (instantiate_var vars offset i)
  | Const c -> Const c

let rec instantiate vars offset = function
  | Bind m ->
     Bind (instantiate vars (offset+1) m)
  | Appl (h, ms) ->
     Appl (instantiate_head vars offset h,
           List.map (instantiate vars offset) ms)
  | Meta (mv, vars) ->
     Meta (mv, List.map (instantiate_var vars offset) vars)

(**********************************************************************)
(* Substituting for a metavariable *)
let rec subst_mv (mv, defn as binding) = function
  | Bind m ->
     Bind (subst_mv binding m)
  | Appl (h, ms) ->
     Appl (h, List.map (subst_mv binding) ms)
  | Meta (mv', vs) ->
     if mv = mv' then instantiate vs 0 defn else Meta (mv', vs)

(* ?X[x,y,z] := c(x,y)

   ?X[a,b,c] ==> c(a,b)

   each metavariable has an arity, which ought to be respected. When
   turning a term into a definition for a metavariable, we renumber
   the variables to be easily substitutable: go through the definition
   and look up that variable in the instantiation list. *)

(**********************************************************************)
(* Unification:

   ?X[xs]    =?= M : n           ==>   ?X := M   if xs <= fv(M), ?X \notin fmv(M)
      .. and symmetrically
   Lam M     =?= Lam N : n       ==>   M =?= N : n+1
   App(h,Ms) =?= App (h,Ns) : n  ==>   zipWith (=?= : n) Ms Ns
   _         =?= _               ==>   _|_
*)

let head_equal h1 h2 =
  match h1, h2 with
    | Var i,   Var j    -> i = j
    | Const c, Const c' -> String.equal c c'
    | _,       _        -> false

let rec zip_problems ms ms' problems =
  match ms, ms' with
    | [],    []      -> Ok problems
    | m::ms, m'::ms' ->
       (match zip_problems ms ms' problems with
         | Ok problems -> Ok ((m,m')::problems)
         | Error err   -> Error err)
    | _,     _       ->
       Error "length mismatch"

let update_subst binding =
  List.map (fun (mv, m) -> (mv, subst_mv binding m))

let update_problems binding =
  List.map (fun (m, m') -> (subst_mv binding m, subst_mv binding m'))

let rec unify subst = function
  | [] ->
     Ok subst
  | (Meta (mv, mvvs), m) :: problems
  | (m, Meta (mv, mvvs)) :: problems ->
     (* FIXME: occurs check *)
     let m        = prepare mvvs 0 m in
     let binding  = (mv, m) in
     let subst    = binding :: update_subst binding subst in
     let problems = update_problems binding problems in
     unify subst problems
  | (Bind m, Bind m') :: problems ->
     unify subst ((m,m') :: problems)
  | (Appl (h, ms), Appl (h', ms')) :: problems when head_equal h h' ->
     (match zip_problems ms ms' problems with
       | Ok problems -> unify subst problems
       | Error err   -> Error err)
  | _ ->
     Error "unification failed"
