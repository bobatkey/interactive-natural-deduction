let initial_text =
      {|Text editor

- Movement works
- Insertion and deletion of text works
- (* Syntax *) highlighting
- Not done:
  - Selections
  - Search and replace
  - More "Semantic" features
    - Automatic indentation
    - 
  - Unicode support
  - Viewports
|}


let () =
  Ulmus.Component.attach
    ~parent_id:"editor"
    (module Editor)
    (Editor.initial initial_text)
