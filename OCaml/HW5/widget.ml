(** A library of widgets for building GUIs. *)

(************)
(** Widgets *)
(************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions
*)
type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(** {1 Layout Widgets } *)
(************************)

(** The simplest widget just occupies space *)
let space (p: Gctx.dimension) : widget =
  { repaint = (fun _ -> ());
    handle = (fun _ _ -> ());
    size = (fun _ -> p);
  }

(** Adds a one-pixel border to an existing widget. *)
let border (w: widget) : widget =
  { repaint = (fun (g: Gctx.gctx) ->
        let (width, height) = w.size () in
        let x = width + 3 in    (* not + 4 because we start at 0 *)
        let y = height + 3 in
        Gctx.draw_line g (0,0) (x,0);
        Gctx.draw_line g (0,0) (0, y);
        Gctx.draw_line g (x,0) (x, y);
        Gctx.draw_line g (0, y) (x, y);
        let g = Gctx.translate g (2,2) in
        w.repaint g);

    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
            w.handle (Gctx.translate g (2,2)) e);

    size = (fun () ->
            let (width, height) = w.size () in
            width + 4, height + 4);
  }

(* Determines whether a given event is within a region of a widget whose*)
(* upper-left hand corner is (0,0) with width w and height h.           *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally.  They
    are aligned at their top edge. *)
let hpair (w1:widget) (w2:widget) : widget = {
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (fst (w1.size ()),0) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (fst (w1.size ()), 0)) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (x1 + x2, max y1 y2))
}

let vpair (w1: widget) (w2: widget) : widget = {
  (* Repaint translates on the y-axis for the second inner widget, handle *)
  (* determines if the event is within each inner widget or drops it, and size*)
  (* determines the dimensions based on the maximum of the x-values and the*)
  (* sum of the y-values*)
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (0, snd (w1.size ())) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (0, snd (w1.size ()))) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (max x1 x2, y1 + y2))
}

(* Note: the OCaml List module has a function fold_right (List.fold_right).
   Note the order of the arguments (which is different
   from previous homeworks).
   Also, you will see that there is a fold_left function, you
   may want to think about what this does, and how it's different
   from the fold you're used to.
*)
let list_layout
    (pair: widget -> widget -> widget)
    (ws: widget list) : widget =
  (* Creates an accumulator out of a space of dimensions 0x0 and cumulatively*)
  (* adds elements to it in hpairs using List.fold_right*)
  let acc : widget = space (0, 0) in
  List.fold_right pair ws acc

let vlist (ws: widget list) : widget = list_layout vpair ws
  (* Calls list_layout with a vpair*)
let hlist (ws: widget list) : widget = list_layout hpair ws
  (* Calls list_layout with an hpair*)


(*****************************)
(** {1    Label Widgets    } *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The mechanism
   for this is called the "value_controller", which is generic to accomodate
   values of different types. (Don't worry about add_change_listener for now.)

  Because both the widget and the controller share the same, mutable value,
  the constructor must create both together. For label widgets, the value
  we're dealing with is of type string. *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }

(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  let r = { contents = s } in
  { repaint = (fun (g: Gctx.gctx) ->
            Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }

(*****************************************)
(** {1   Event Listeners and Notifiers } *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()


(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
      begin match Gctx.event_type e with
        | Gctx.KeyPress key -> action key
        | _ -> ()
      end

(** A notifier_controller is associated with a notifier widget.
    It allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
    extra screen space -- it extends an existing widget with the
    ability to react to events. It maintains a list of of "listeners"
    that eavesdrop on the events propagated through the notifier
    widget.

    When an event comes in to the notifier, it is passed to each
    event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
          List.iter (fun h -> h g e) listeners.contents;
          w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl: event_listener) ->
          listeners.contents <- newl :: listeners.contents
  }

(*****************************************)
(** {1   Button                        } *)
(*****************************************)

(** A button has a string, which can be controlled by the
    corresponding value_controller, and an event listener, which can be
    controlled by the notifier_controller to add listeners (e.g. a
    mouseclick_listener) that will perform an action when the button is
    pressed. *)
let button (s: string)
    : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)

(*****************************************)
(** {1   Canvas                        } *)
(*****************************************)

(** A Canvas is a bordered widget with a notifier. New event listeners
    can be added by the notifier_controller.
    The repaint method of a canvas is a parameter of the widget constructor. *)
let canvas (dim: Gctx.dimension) (paint : Gctx.gctx -> unit)
    : widget * notifier_controller =
  let w =
    { repaint = paint;
      handle = (fun _ _ -> ());
      size = (fun _ -> dim) }
  in
  notifier (border w)

(*****************************************)
(** {1   Checkbox                      } *)
(*****************************************)
(* TODO: Task 5 requires you to develop a checkbox widget *)

(** A checkbox is a controller for a value associated with a widget.

    This controller can read and write the value. It also allows
    change listeners to be registered by the application. These listeners are
    run whenever this value is set. *)
type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  set_value           : 'a -> unit
}

let checkbox (init: bool) (s: string)
    : widget * bool value_controller =
  let listeners = { contents = [] } in
  let current = ref init in
  let new_controller : 'a value_controller =
    { add_change_listener =
      (fun newl -> listeners.contents <- newl :: listeners.contents);
    get_value = (fun () -> !current);
    set_value = (fun (x : bool) -> current.contents <- x;)
    } in
  (* Defines the 'a value_controller to be used in the checkbox *)
  { repaint = (fun (g: Gctx.gctx) ->
                if !current then Gctx.draw_string g (0,0) (s ^ " On ")
                else Gctx.draw_string g (0,0) (s ^ " Off"));
    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
             if Gctx.event_type e = Gctx.MouseDown
             then new_controller.set_value (not (new_controller.get_value ()));
             List.iter (fun ent -> ent !current) listeners.contents);
    size = fun () -> (fst (Graphics.text_size s) +
      fst (Graphics.text_size "    "), snd (Graphics.text_size s))
  }, new_controller
  (* Creates a checkbox using a widget and the above 'a value_controller *)

(*****************************************)
(** {1   Additional widgets            } *)
(*****************************************)

(* TODO: In Task 6 you may choose to add a radio_button widget, color slider *)
(* or text_box widget.*)

let rec string_of_chars (cl: char list) : string =
  begin match cl with
  | [] -> ""
  | x :: xs -> "x" ^ string_of_chars xs
  end

let text_controller : string value_controller =
  let listeners = { contents = [] } in
  let current = { contents = "" } in
  { add_change_listener =
    (fun newl -> listeners.contents <- newl :: listeners.contents);
    get_value = (fun () -> !current);
    set_value = (fun (x : string) -> current.contents <- x;)
  }
(* Defines a text_controller to store the user-inputted string and perform*)
(* operations on it *)

let textbox (s: string) : widget * string value_controller * notifier_controller =
  let textwidget : widget =
  { repaint = (fun (g: Gctx.gctx) -> Graphics.draw_string
      (s ^ text_controller.get_value () ));
    handle = (fun (g: Gctx.gctx) (e: Gctx.event) -> print_endline "test1";
      begin match Gctx.event_type e with
      | Gctx.KeyPress x ->
          begin match x with
        (*| -backspace- -> ...*)
          | _ -> text_controller.set_value (text_controller.get_value () ^ "x");
                 print_endline "test2"
          end
      | _ -> ()
      end);
    size = (fun g -> (fst (Graphics.text_size s) + fst (Graphics.text_size
      (text_controller.get_value () ))), snd (Graphics.text_size s))
  } in
  let (w, nc) =  notifier textwidget in
  (w, text_controller, nc)
(* Creates a textbox from a widget, a string value_controller and a*)
(* notifier_controller to handle text inputs*)