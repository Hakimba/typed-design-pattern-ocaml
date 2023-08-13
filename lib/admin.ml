type user = {name : string; is_admin : bool}
type context = {current_user: user}
exception Render404

let is_admin user = user.is_admin

module Admin : sig
  type t = private Admin of user
  val from_user : user -> t option
end = struct
  type t = Admin of user
  let from_user user =
    if is_admin user then Some (Admin user)
    else None
end

(* View *)

let render_admin_panel (Admin.Admin { name ; _}) =
  let open Tyxml.Html in
  div [
    h1 [ txt ("Hello dear Administrator, " ^ name) ] ]


(* Controller *)

let admin_panel context =
  match Admin.from_user context.current_user with
  | Some admin_user ->
    render_admin_panel admin_user
  | None ->
    raise Render404


let main () =
  let user = {name="hakim";is_admin=true} in

  (* I can't create an Admin token a la main
  let doesnt_compile = render_admin_panel (Admin.Admin user) *)

  (* Correct usage *)
  admin_panel {current_user=user}
