open Generalities

module type SubComponent = sig
  type state
  type internal_action
  type external_action

  val initial : state
  val render : state -> [`Internal of internal_action | `External of external_action] Dynamic_HTML.html
  val update : internal_action -> state -> state
end

module App : Component.S = struct

  type state =
    { prooftree : ProofTree.prooftree
    ; selection : ProofTree.goal option

    ; conjE1_parameter : string
    ; conjE2_parameter : string
    ; implI_parameter  : string
    ; disjE_parameter1 : string
    ; disjE_parameter2 : string
    }

  type rule =
    | Assumption
    | Implies_Intro
    | Implies_Elim

    | Conj_Intro
    | Conj_Elim1
    | Conj_Elim2

    | Disj_Intro1
    | Disj_Intro2
    | Disj_Elim

  type action =
    | Select of ProofTree.goal
    | Reopen
    | ApplyRule of rule

    | Update_ConjE1 of string
    | Update_ConjE2 of string
    | Update_ImplI of string
    | Update_DisjE1 of string
    | Update_DisjE2 of string

  let initial =
    { prooftree =
        ProofTree.(initial (And (a, b) @-> (a @-> c) @-> (b @-> d) @-> Or (c,d)))
    ; selection = None
    ; conjE1_parameter = ""
    ; conjE2_parameter = ""
    ; implI_parameter  = ""
    ; disjE_parameter1 = ""
    ; disjE_parameter2 = ""
    }

  module R =
    ProofTree.Renderer
      (Dynamic_HTML)
      (struct type nonrec action = action
        let makeselection path = Dynamic_HTML.E.onclick (Select path)
      end)

  let render ({prooftree;selection} as state) =
    let open Dynamic_HTML in
    begin%concat
      div (R.render selection prooftree);
      div ~attrs:[A.class_ "buttonbox"] begin%concat
        button ~attrs:[E.onclick Reopen]
          (text "Reset selected goal");
        button ~attrs:[E.onclick (ApplyRule Assumption)]
          (text "Assumption");
        button ~attrs:[E.onclick (ApplyRule Implies_Intro)]
          (text "→-Intro");
        div begin%concat
          input
            ~attrs:[ E.oninput (fun ~value -> Update_ImplI value)
                   ; A.type_ "text"
                   ; A.value state.implI_parameter
                   ; E.onkeypress (fun _ _ -> None)
                   ];
          button ~attrs:[E.onclick (ApplyRule Implies_Elim)]
            (text "→-Elim");
        end;
        button ~attrs:[E.onclick (ApplyRule Conj_Intro)]
          (text "∧-Intro");
        div begin%concat
          input
            ~attrs:[ E.oninput (fun ~value -> Update_ConjE1 value)
                   ; A.type_ "text"
                   ; A.value state.conjE1_parameter
                   ; E.onkeypress (fun _ _ -> None)
                   ];
          button ~attrs:[E.onclick (ApplyRule Conj_Elim1)]
            (text "∧-Elim-1")
        end;
        div begin%concat
          input
            ~attrs:[ E.oninput (fun ~value -> Update_ConjE2 value)
                   ; A.type_ "text"
                   ; A.value state.conjE2_parameter
                   ; E.onkeypress (fun _ _ -> None)
                   ];
          button ~attrs:[E.onclick (ApplyRule Conj_Elim2)]
            (text "∧-Elim-2")
        end;
        button ~attrs:[E.onclick (ApplyRule Disj_Intro1)]
          (text "∨-Intro-1");
        button ~attrs:[E.onclick (ApplyRule Disj_Intro2)]
          (text "∨-Intro-2");
        div begin%concat
          input
            ~attrs:[ E.oninput (fun ~value -> Update_DisjE1 value)
                   ; A.type_ "text"
                   ; A.value state.disjE_parameter1
                   ];
          input
            ~attrs:[ E.oninput (fun ~value -> Update_DisjE2 value)
                   ; A.type_ "text"
                   ; A.value state.disjE_parameter2
                   ];
          button ~attrs:[E.onclick (ApplyRule Disj_Elim)]
            (text "∨-Elim")
        end
      end
    end

  let apply_rule ({prooftree; selection} as state) rule =
    match rule, selection with
      | _, None ->
         state
      | Assumption, Some selection ->
         (try { state with
                  prooftree = ProofTree.by_assumption selection prooftree
                ; selection = None
              }
          with Invalid_argument _ ->
            state)
      | Implies_Intro, Some selection ->
         (try { state with
                  prooftree = ProofTree.implies_intro selection prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Conj_Intro, Some selection ->
         (try { state with
                  prooftree = ProofTree.conj_intro selection prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Conj_Elim1, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.conj_elim1
                      (ProofTree.Atom state.conjE1_parameter)
                      selection
                      prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Conj_Elim2, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.conj_elim2
                      (ProofTree.Atom state.conjE2_parameter)
                      selection
                      prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Implies_Elim, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.implies_elim
                      (ProofTree.Atom state.implI_parameter)
                      selection
                      prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Disj_Intro1, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.disj_intro1
                      selection
                      prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Disj_Intro2, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.disj_intro2
                      selection
                      prooftree
                ; selection = None
              }
          with Invalid_argument _ -> state)
      | Disj_Elim, Some selection ->
         (try { state with
                  prooftree =
                    ProofTree.(disj_elim
                                 (Atom state.disjE_parameter1)
                                 (Atom state.disjE_parameter2)
                                 selection
                                 prooftree)
                ; selection = None
              }
          with Invalid_argument _ -> state)

  let update action state = match action with
    | Select path ->
       {state with selection = Some path}
    | ApplyRule rule ->
       apply_rule state rule
    | Reopen ->
       (match state.selection with
         | None -> state
         | Some selection ->
            {state with
               prooftree = ProofTree.makeopen selection state.prooftree })

    | Update_ConjE1 str ->
       {state with conjE1_parameter = str}
    | Update_ConjE2 str ->
       {state with conjE2_parameter = str}
    | Update_ImplI str ->
       {state with implI_parameter = str}
    | Update_DisjE1 str ->
       {state with disjE_parameter1 = str}
    | Update_DisjE2 str ->
       {state with disjE_parameter2 = str}
end

let _ =
  Component.attach ~parent_id:"main" (module App)
