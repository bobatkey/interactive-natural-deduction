let a = Formula.Atom "A"
let b = Formula.Atom "B"
let c = Formula.Atom "C"
let d = Formula.Atom "D"

let f1 = Formula.(Or (a, b) @-> (a @-> c) @-> (b @-> d) @-> Or (c,d))
let f2 = Formula.(Or (a @-> b, a @-> c) @-> a @-> Or (b,c))

module App = struct
  module PTU =
    ProofTree_UI.Make
      (struct
        type t = Formula.t
        let to_string x = Formula.to_string x
      end)
      (ClassicalPropositionalLogic.System)
      (ClassicalPropositionalLogic.Partials)

  type state =
    | Proving     of PTU.state list * PTU.state
    | StartScreen of string

  type action =
    | ChangeFormula of string
    | StartProving  of Formula.t
    | StartAgain
    | ProofAction   of PTU.action
    | Undo

  let initial =
    StartScreen (Formula.to_string ~unicode:false f1)

  let render = function
    | Proving (history, prooftree) ->
       let open Ulmus.Dynamic_HTML in
       div ~attrs:[A.style "display: flex; flex-direction: column; height: 100%; justify-content: space-between"] begin%concat
         div ~attrs:[A.style "align-self: flex-start; width:100%"] begin
           div ~attrs:[A.style "display: flex; justify-content: center; align-items: flex-start; width:100%"] begin%concat
             div ~attrs:[A.style "flex: none"] begin%concat
               (match history with
                 | [] -> button ~attrs:[A.disabled true] (text "Undo")
                 | _  -> button ~attrs:[E.onclick Undo] (text "Undo"));
               button ~attrs:[E.onclick StartAgain] (text "Enter new formula")
             end
           end
         end;
         div ~attrs:[A.style "align-self: flex-end"] begin
           map (fun a -> ProofAction a) (PTU.render prooftree)
         end
       end
    | StartScreen string ->
       let open Ulmus.Dynamic_HTML in
       div ~attrs:[A.style "align-self: flex-start"] begin%concat
         div begin%concat
           h1 (text "Interactive Natural Deduction Proof Editor");
           p (text "Enter a formula and click the button to start building a proof.");
         end;
         div begin%concat
           input ~attrs:[ A.value string
                        ; A.class_ "initialformulaentry"
                        ; E.oninput (fun value -> ChangeFormula value) ];
           (match Formula.of_string string with
             | None ->
                button ~attrs:[ A.disabled true ] (text "Start Proving...")
             | Some f ->
                button ~attrs:[ E.onclick (StartProving f) ] (text "Start Proving..."))
         end
       end

  let update action state = match state, action with
    | StartScreen _,     ChangeFormula s -> StartScreen s
    | StartScreen _,     StartProving f  -> Proving ([], PTU.initial f)
    | Proving (h, t),    ProofAction a   -> Proving (t::h, PTU.update a t)
    | Proving (t::h, _), Undo            -> Proving (h, t)
    | Proving _,         StartAgain      -> initial
    | _,                 _               -> state

end

let _ =
  Ulmus.Component.attach ~parent_id:"main" (module App)
