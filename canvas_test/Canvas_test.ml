module App = struct
  type state = { radius : float }

  type action = Increase | Decrease

  let initial = { radius = 0.2 }

  let update action {radius} = match action with
    | Increase -> {radius = if radius < 0.45 then radius +. 0.01 else radius }
    | Decrease -> {radius = if radius > 0.1 then radius -. 0.01 else radius }

  open Gg
  open Vg

  let aspect = 1.618
  let size   = Size2.v (aspect *. 100.) 100.
  let view   = Box2.v P2.o (Size2.v aspect 1.)
  let bg_colour = I.const (Color.v_srgb 0.314 0.784 0.471)

  let render {radius} =
    let image =
      I.cut
        (P.empty >> P.circle (P2.v 0.5 0.5) radius)
        bg_colour
    in
    let open Dynamic_HTML in
    div ~attrs:[A.style "display: flex; flex-direction: column"] begin%concat
      vg_image size view image;
      button ~attrs:[E.onclick Increase] (text "Increase");
      button ~attrs:[E.onclick Decrease] (text "Decrease");
    end
end

let _ =
  Component.attach ~parent_id:"main" (module App)
