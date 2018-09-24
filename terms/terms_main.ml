let () =
  Ulmus.Component.attach
    ~parent_id:"terms"
    (module Terms)
    Terms.initial
