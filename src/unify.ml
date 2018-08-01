type mvar = string

type const = string

type tm =
  | Var  of int
  | Con of const * binding_tm list
  | Unk of mvar * int list

and binding_tm =
  { binders : int
  ; term    : tm
  }

let nobind term = { binders = 0; term }

(**********************************************************************)
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
(* Preparation of a term to be a definition *)
let prepare_var vars offset i =
  if i < offset then i
  else
    let i = i - offset in
    let rec find idx = function
      | [] ->
         failwith "preparation failed: inaccessible variable"
      | j :: js ->
         if i = j then idx else find (idx+1) js
    in
    find 0 vars + offset

let rec prepare vars offset = function
  | Var i ->
     Var (prepare_var vars offset i)
  | Con (c, ms) ->
     Con (c, List.map (prepare_binding_tm vars offset) ms)
  | Unk (mv, vars) ->
     (* FIXME: do an occurs check here *)
     (* FIXME: could do pruning here: if any of the variables required
        are not accessble, then create new metavariables to replace
        the old ones. *)
     Unk (mv, List.map (prepare_var vars offset) vars)

and prepare_binding_tm vars offset { binders; term } =
  { binders; term = prepare vars (offset+binders) term }

(**********************************************************************)
(* Instantiation of metavariable definitions *)
let instantiate_var vars offset i =
  if i < offset then i
  else List.nth vars (i - offset) + offset

let rec instantiate vars offset = function
  | Var i ->
     Var (instantiate_var vars offset i)
  | Con (c, ms) ->
     Con (c, List.map (instantiate_binding_tm vars offset) ms)
  | Unk (mv, vars) ->
     Unk (mv, List.map (instantiate_var vars offset) vars)

and instantiate_binding_tm vars offset { binders; term } =
  { binders; term = instantiate vars (offset+binders) term }

(**********************************************************************)
(* Substituting for a metavariable *)
let rec subst_mv (mv, m as defn) = function
  | Var i ->
     Var i
  | Con (c, ms) ->
     Con (c, List.map (subst_mv_binding defn) ms)
  | Unk (mv', vs) ->
     if mv = mv' then instantiate vs 0 m else Unk (mv', vs)

and subst_mv_binding defn { binders; term } =
  { binders; term = subst_mv defn term }

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

let rec zip_problems ms ms' problems =
  match ms, ms' with
    | [],    []      -> Ok problems
    | {binders = b1; term = m1}::ms, {binders = b2; term = m2}::ms' ->
       if b1 <> b2 then Error "binder mismatch"
       else
         (match zip_problems ms ms' problems with
           | Ok problems -> Ok ((m1,m2)::problems)
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
  | (Unk (mv, mvvs), m) :: problems
  | (m, Unk (mv, mvvs)) :: problems ->
     (* FIXME: occurs check *)
     let m        = prepare mvvs 0 m in
     let defn     = (mv, m) in
     let subst    = defn :: update_subst defn subst in
     let problems = update_problems defn problems in
     unify subst problems
  | (Var i, Var j) :: problems when i = j ->
     unify subst problems
  | (Con (c, ms), Con (c', ms')) :: problems when String.equal c c' ->
     (match zip_problems ms ms' problems with
       | Ok problems -> unify subst problems
       | Error err   -> Error err)
  | _ ->
     Error "unification failed: term mismatch"
