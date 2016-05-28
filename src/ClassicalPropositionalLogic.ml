open Result

type formula = Formula.t

type assumption = Formula.t

type rule =
  | Assumption
  | Implies_intro
  | Implies_elim of Formula.t
  | Conj_intro
  | Conj_elim1 of Formula.t
  | Conj_elim2 of Formula.t
  | Disj_intro1
  | Disj_intro2
  | Disj_elim of Formula.t * Formula.t
  | False_elim
  | Not_intro
  | Not_elim of Formula.t
  | RAA

let name_of_rule = function
  | Assumption     -> "assumption"
  | Implies_intro  -> "→-I"
  | Implies_elim _ -> "→-E"
  | Conj_intro     -> "∧-I"
  | Conj_elim1 _   -> "∧-E1"
  | Conj_elim2 _   -> "∧-E2"
  | Disj_intro1    -> "∨-I1"
  | Disj_intro2    -> "∨-I2"
  | Disj_elim _    -> "∨-E"
  | False_elim     -> "⊥-E"
  | Not_intro      -> "¬-I"
  | Not_elim _     -> "¬-E"
  | RAA            -> "RAA"

let apply rule assumptions formula = match rule with
  | Assumption ->
     if List.mem formula assumptions then
       Ok []
     else
       Error (`Msg "assumption: no such assumption")

  | Implies_intro ->
     begin
       match formula with
         | Formula.Implies (f1, f2) ->
            Ok [ (Some f1, f2) ]
         | _ ->
            Error (`Msg "implies_intro: formula is not an implication")
     end
     
  | Implies_elim f ->
     Ok [ (None, Formula.Implies (f, formula))
        ; (None, f)
        ]

  | Conj_intro ->
     begin
       match formula with
         | Formula.And (f1, f2) ->
            Ok [ (None, f1)
               ; (None, f2)
               ]
         | _ ->
            Error (`Msg "conj_intro: formula is not a conjunction")
     end

  | Conj_elim1 f ->
     Ok [ (None, Formula.And (formula, f)) ]

  | Conj_elim2 f ->
     Ok [ (None, Formula.And (f, formula)) ]

  | Disj_intro1 ->
     begin
       match formula with
         | Formula.Or (f1, f2) ->
            Ok [ (None, f1) ]
         | _ ->
            Error (`Msg "disj_intro1: formula is not a disjunction")
     end

  | Disj_intro2 ->
     begin
       match formula with
         | Formula.Or (f1, f2) ->
            Ok [ (None, f2) ]
         | _ ->
            Error (`Msg "disj_intro2: formula is not a disjunction")
     end

  | Disj_elim (f1, f2) ->
     Ok [ (None, Formula.Or (f1, f2))
        ; (Some f1, formula)
        ; (Some f2, formula)
        ]

  | False_elim ->
     Ok [ (None, Formula.False) ]

  | Not_intro ->
     begin
       match formula with
         | Formula.Not f ->
            Ok [ (Some f, Formula.False) ]
         | _ ->
            Error (`Msg "not_intro: formula is not a negation")
     end

  | Not_elim f ->
     Ok [ (None, f)
        ; (None, Formula.Not f)
        ]

  | RAA ->
     Ok [ (Some (Formula.Not formula), Formula.False) ]
