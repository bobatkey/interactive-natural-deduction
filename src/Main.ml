let a = Formula.Atom "A"
let b = Formula.Atom "B"
let c = Formula.Atom "C"
let d = Formula.Atom "D"

let f1 = Formula.(Or (a, b) @-> (a @-> c) @-> (b @-> d) @-> Or (c,d))
let f2 = Formula.(Or (a @-> b, a @-> c) @-> a @-> Or (b,c))

module App = struct
  module PTU = ProofTree.UI

  type state =
    | Proving     of ProofTree.prooftree list * ProofTree.prooftree
    | StartScreen of string

  type action =
    | ChangeFormula of string
    | StartProving  of Formula.t
    | ProofAction   of PTU.action
    | Undo

  let initial =
    StartScreen (Formula.to_string ~unicode:false f1)

  let render =
    let open Dynamic_HTML in
    function
      | Proving (history, prooftree) ->
         div ~attrs:[A.style "display: flex; flex-direction: column; height: 100%; justify-content: space-between"] begin%concat
           div ~attrs:[A.style "align-self: flex-start; width:100%"] begin
             div ~attrs:[A.style "display: flex; justify-content: center; align-items: flex-start; width:100%"] begin%concat
               div ~attrs:[A.style "flex: none"] begin
                 match history with
                   | [] -> button ~attrs:[A.disabled true] (text "Undo")
                   | _  -> button ~attrs:[E.onclick Undo] (text "Undo")
               end
             end
           end;
           div ~attrs:[A.style "align-self: flex-end"] begin
             map (fun a -> ProofAction a) (PTU.render prooftree)
           end
         end
      | StartScreen string ->
         div ~attrs:[A.style "align-self: flex-start"] begin%concat
           div begin%concat
             h1 (text "Interactive Natural Deduction Proof Editor");
             p (text "Enter a formula and click the button to start building a proof.");
           end;
           div begin%concat
             input ~attrs:[ A.value string
                          ; A.class_ "initialformulaentry"
                          ; E.oninput (fun ~value -> ChangeFormula value) ];
             (match ProofTree.parse_formula string with
               | None ->
                  button ~attrs:[ A.disabled true ] (text "Start Proving...")
               | Some f ->
                  button ~attrs:[ E.onclick (StartProving f) ] (text "Start Proving..."))
           end
         end

  let update action state = match state, action with
    | StartScreen _,     ChangeFormula s -> StartScreen s
    | StartScreen _,     StartProving f  -> Proving ([], ProofTree.initial f)
    | Proving (h, t),    ProofAction a   -> Proving (t::h, PTU.update a t)
    | Proving (t::h, _), Undo            -> Proving (h, t)
    | _,                 _               -> state

end

let _ =
  Component.attach ~parent_id:"main" (module App)
