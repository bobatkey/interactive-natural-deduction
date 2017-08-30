let _ =
  Lib_mvc.Component.attach ~parent_id:"editor" (module Editor)

(*    (module VCR.Of
        (Editor)
        (struct
          type t = Editor.action
          let relevant = function
            | Editor.Movement _ -> false
            | _                 -> true
         end))
*)
