module Component = struct
  let question =
    "Does a swan sing its best song just before it dies?"

  let expected_answer =
    true
  
  type state =
    | NoAnswer
    | Answer of bool

  type action =
    | NoAction
    | Update of bool

  let styles l =
    Ulmus.Dynamic_HTML.A.style (String.concat "; " l)
  
  let render st =
    let open Ulmus.Dynamic_HTML in
    begin
      div ~attrs:[ styles [ "display:flex"
                          ; "width: 100%"
                          ; "justify-content: space-between"
                          ; "border-bottom: 1px solid #ddd"
                          ; "border-right: 1px solid #ddd"
                          ; "padding: 0.5em"
                          ]
                 ] begin
        div begin
          text question
        end
        ^^
        div begin
          let open Ulmus.DropDown in
          make
            [ option (text "Select...") ~action:NoAction
            ; option (text "True") ~action:(Update true)
            ; option (text "False") ~action:(Update false)
            ]
        end
      end
    end

  let update () st = st

  let initial =
    NoAnswer

end

let () =
  Ulmus.Component.attach
    ~parent_id:"quiz"
    (module Component)
    Component.initial

