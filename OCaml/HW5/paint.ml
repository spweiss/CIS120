(** Paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES and PROGRAM STATE    *)
(******************************************)

(** The paint program uses the mutable record (called [state] below)
to store its state.  *)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 2, 4, 5 and maybe 6. *)
type shape = 
  | Line of color * int * point * point
  | Points of Gctx.color * point list
  | Ellipse of Gctx.color * int * point * point
  | Text of color * string * point

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

    - LineStartMode means the paint program is waiting for the user to make
    the first click to start a line.

    - LineEndMode means that the paint program is waiting for the user's
    second click. The point associated with this mode stores the location of
    the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point
  | TextStartMode
  | TextEndMode

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
  least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 3, 5, and *)
  (* possibly 6 *) 
  mutable preview : shape option;
  (* Extends the state type definition to include a mutable preview *)
  
  mutable thickness : int;
  (* Extends the state type definition to include a mutable thickness *)
  
  mutable text : string;
  (* Extends the state type definition to include mutable text *)
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  (* TODO: You will need to add new state for Tasks 3, 5, and maybe 6 *)
  preview = None;
  thickness = 1;
  text = "";
}  

(** This function creates a graphics context with the appropriate
    pen color.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) : gctx =
  let g1 = with_color g c in
  g1

let with_params2 (g: gctx) (t: int) (c: color) : gctx =
  let g1 = with_color g c in
  let g2 = with_thickness g1 t in
  g2
(* Defines a new with_params2 function that alters the thickness as well as *)
(* the color in a gctx *)

(*********************************)
(** PAINT CANVAS REPAINTING      *)
(*********************************)
(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)
    
(* TODO: You will need to modify this repaint function in Tasks 2, 3, 4,   *)
(* and possibly 6. For example, if the user is performing some operation   *)
(* that provides "preview" (see Task 2) the repaint function must also     *)
(* show the preview.                                                       *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line (c, t, p1, p2) -> draw_line (with_params2 g t c) p1 p2
      | Points (c, ps) -> draw_points (with_params g c) ps
      | Ellipse (c, t, p1, p2) ->
          let center : position = (((fst p1 + fst p2)/2), ((snd p1 + snd p2)/2)) in
          let rx : int = (abs ((fst center) - (fst p1))) in
          let ry : int = (abs ((snd center) - (snd p1))) in
          draw_ellipse (with_params2 g t c) center rx ry
      | Text (c, s, p) -> Graphics.draw_string s
    end in
  Deque.iterate draw_shape paint.shapes;
  (* Draws the preview shape after drawing the existing shapes *)
  begin match paint.preview with
  | None -> ()
  | Some n -> draw_shape n
  end

(** Create the actual paint_canvas widget and its associated
notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
    (* This case occurs when the mouse has been clicked in the canvas, but *)
    (* before the button has been released. How we process the event       *)
    (* depends on the current mode of the paint canvas.                    *)
        (begin match paint.mode with 
            | LineStartMode ->
                (* The paint_canvas was waiting for the first click of a line,   *)
                (* so change it to LineEndMode, recording the starting point of  *)
                (* the line.                                                     *)
                paint.mode <- LineEndMode p
            | LineEndMode p1 ->
                (* The paint_canvas was waiting for the second click of a line,  *)
                (* so create the line and add it to the deque of shapes. Go back *)
                (* to waiting for the first click. *)
                (*(if paint.thickness = 4 then Graphics.set_line_width 4
                else Graphics.set_line_width 1;*)
                (Deque.insert_tail (Line
                  (paint.color, paint.thickness, p1, p)) paint.shapes;
                (*Graphics.set_line_width 1;*)
                paint.mode <- LineStartMode;
                (* Sets the preview to None once the user selects a location*)
                (* for the line *)
                paint.preview <- None)
            | PointMode -> paint.preview <- Some (Points (paint.color, [p]))
            | EllipseStartMode -> paint.mode <- EllipseEndMode p
            | EllipseEndMode p2 ->
                (*(if paint.thickness = 4 then Graphics.set_line_width 4
                else Graphics.set_line_width 1;*)
                (Deque.insert_tail (Ellipse
                  (paint.color, paint.thickness, p2, p)) paint.shapes;
                (*Graphics.set_line_width 1;*)
                paint.mode <- EllipseStartMode;
                paint.preview <- None;)
            | TextStartMode -> ()
            | TextEndMode ->
                (paint.mode <- TextStartMode; paint.preview <- None)
          end)
    | MouseDrag ->
    (* In this case, the mouse has been clicked, and it's being dragged    *)
    (* with the button down. Initially there is nothing to do, but you'll  *)
    (* need to update this part for Task 2, 3, 4 and maybe 6.                 *)
        begin match paint.mode with
        | LineStartMode -> ()
        | LineEndMode p1 -> paint.preview <- Some (Line
            (paint.color, paint.thickness, p1, p))
          (* Updates the preview to include a line while dragging *)
        | PointMode -> let points =
                         begin match paint.preview with
                         | None -> []
                         | Some (Points (_, ps)) -> ps
                         | Some _ -> []
                         end in
                       paint.preview <- Some (Points (paint.color, p :: points))
          (* Successively adds points to a list to be previewed while dragging *)
        | EllipseStartMode -> ()
        | EllipseEndMode p2 -> paint.preview <- Some (Ellipse
            (paint.color, paint.thickness, p2, p))
          (* Updates the preview to include an ellipse while dragging *)
        | TextStartMode -> ()
        | TextEndMode -> ()
        end
    | MouseUp ->
    (* In this case there was a mouse button release event. TODO: Tasks 2, *)
    (* 3, 4, and possibly 6 need to do something different here.           *)
        begin match paint.mode with
        | LineStartMode -> ()
        | LineEndMode p1 ->
            (paint.preview <- None;
            (*if paint.thickness = 4 then Graphics.set_line_width 4
            else Graphics.set_line_width 1;*)
            Deque.insert_tail (Line
              (paint.color, paint.thickness, p1, p)) paint.shapes;
            (*Graphics.set_line_width 1;*)
            paint.mode <- LineStartMode)
        | PointMode -> let points =
                         begin match paint.preview with
                         | None -> []
                         | Some (Points (_, ps)) -> ps
                         | Some _ -> []
                         end in
                       paint.preview <- None; 
                       Deque.insert_tail (Points
                         (paint.color, points)) paint.shapes;
                       (* Adds the points stored in the preview to the *)
                       (* shapes deque *)
        | EllipseStartMode -> ()
        | EllipseEndMode p2 ->
            (paint.preview <- None;
            (*if paint.thickness = 4 then Graphics.set_line_width 4
            else Graphics.set_line_width 1;*)
            Deque.insert_tail (Ellipse
              (paint.color, paint.thickness, p2, p)) paint.shapes;
            (*Graphics.set_line_width 1;*)
            paint.mode <- EllipseStartMode)
        | TextStartMode -> ()
        | TextEndMode -> ()
        end
	 
    | MouseMove ->
    (* This catches the MouseMove event (where the user moved the mouse over *) 
    (* the canvas without pushing any buttons) and the KeyPress event (where *)
    (* the user typed a key when the mouse was over the canvas).             *)
    ()
    | _ -> ()
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(**
This part of the program creates the other widgets for the
paint program -- the buttons, color selectors, etc., and
lays them out in the top - level window.

*)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or changing  *)
(* the layout of the Paint GUI. Initially the layout is very ugly because  *)
(* we use only the hpair widget demonstrated in Lecture. Task 1 is to make *)
(* improvements to make the layout more appealing. You may choose to       *)
(* arrange the buttons and other GUI elements of the paint program however *)
(* you like (so long as it is easily apparent how to use the interface ).  *)
(* The sample screen shot of our solution provides one possible design.    *)
(* Also, feel free to improve the visual components of the GUI, for        *)
(* example, our solution puts borders around the buttons and uses a custom *)
(* "color button" that changes its appearance based on whether or not the  *)
(* color is currently selected.                                            *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(**
This function runs when the Undo button is clicked.
It simply removes the last shape from the shapes deque.
*)
(* TODO: You need to modify this in Task 3 and 4. *)
let undo () : unit =
  (if Deque.is_empty paint.shapes then () else
     ignore (Deque.remove_tail paint.shapes);
  paint.preview <- None;
  begin match paint.mode with
  | LineStartMode -> ()
  | LineEndMode p -> paint.mode <- LineStartMode
  | PointMode -> ()
  | EllipseStartMode -> ()
  | EllipseEndMode p -> paint.mode <- EllipseStartMode
  | TextStartMode -> ()
  | TextEndMode -> paint.mode <- TextStartMode
  end)
  (* Removes the tail of the shapes deque, sets the preview to None and sets*)
  (* endmodes to startmodes *)

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** The Quit button, with associated functionality. *)
let w_quit, lc_quit, nc_quit = button "Quit"

;; nc_quit.add_event_listener (mouseclick_listener (fun () -> exit 0))

(** A spacer widget *)
let spacer : widget = space (10,10)

let (w_line, lc_line, nc_line) = button "Line"

let line () : unit =
  begin match paint.mode with
  | LineStartMode -> ()
  | LineEndMode p -> (paint.mode <- LineStartMode; paint.preview <- None)
  | PointMode -> (paint.mode <-LineStartMode; paint.preview <-None)
  | EllipseStartMode -> (paint.mode <- LineStartMode; paint.preview <- None)
  | EllipseEndMode p -> (paint.mode <- LineStartMode; paint.preview <- None)
  | TextStartMode -> (paint.mode <- LineStartMode; paint.preview <- None)
  | TextEndMode -> (paint.mode <- LineStartMode; paint.preview <- None)
  end

;; nc_line.add_event_listener (mouseclick_listener line)
(* Creates a button that enters LineStartMode when pressed *)

let (w_point, lc_point, nc_point) = button "Point"

let point () : unit =
  begin match paint.mode with
  | LineStartMode -> (paint.mode <- PointMode; paint.preview <- None)
  | LineEndMode p -> (paint.mode <- PointMode; paint.preview <- None)
  | PointMode -> ()
  | EllipseStartMode -> (paint.mode <- PointMode; paint.preview <- None)
  | EllipseEndMode p -> (paint.mode <- PointMode; paint.preview <- None)
  | TextStartMode -> (paint.mode <- PointMode; paint.preview <- None)
  | TextEndMode -> (paint.mode <- PointMode; paint.preview <- None)
  end

;; nc_point.add_event_listener (mouseclick_listener point)
(* Creates a button that enters PointMode when pressed *)

let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

let ellipse () : unit =
  begin match paint.mode with
  | LineStartMode -> (paint.mode <- EllipseStartMode; paint.preview <- None)
  | LineEndMode p -> (paint.mode <- EllipseStartMode; paint.preview <- None)
  | PointMode -> (paint.mode <- EllipseStartMode; paint.preview <-None)
  | EllipseStartMode -> ()
  | EllipseEndMode p1 -> (paint.mode <- EllipseStartMode; paint.preview <- None)
  | TextStartMode -> (paint.mode <- EllipseStartMode; paint.preview <- None)
  | TextEndMode -> (paint.mode <- EllipseStartMode; paint.preview <- None)
  end

;; nc_ellipse.add_event_listener (mouseclick_listener ellipse)
(* Creates a button that enters EllipseStartMode when pressed *)

let (w_checkbox, checkbox_cb) =
   Widget.checkbox true "Thick Line"

;; checkbox_cb.add_change_listener
     (fun x -> if x then paint.thickness <- 4 else paint.thickness <- 1)

let (w_textbox, vc_textbox, nc_textbox) = button ("Text Buffer: ")

let textbox () : unit =
  begin match paint.mode with
  | LineStartMode -> (paint.mode <- TextStartMode; paint.preview <- None)
  | LineEndMode p -> (paint.mode <- TextStartMode; paint.preview <- None)
  | PointMode -> (paint.mode <- TextStartMode; paint.preview <-None)
  | EllipseStartMode -> (paint.mode <- TextStartMode; paint.preview <-None)
  | EllipseEndMode p1 -> (paint.mode <- TextStartMode; paint.preview <- None)
  | TextStartMode -> ()
  | TextEndMode -> (paint.mode <- TextStartMode; paint.preview <- None)
  end

;; nc_textbox.add_event_listener (mouseclick_listener textbox)
(* Creates a button that enters TextStartMode when pressed *)

(** The mode toolbar, initially containing just the Undo and Quit buttons. *)
(*  TODO: you will need to add more buttons to the toolbar in *)
(*  Tasks 2,5, and 6. *)
let mode_toolbar : widget =
  Widget.hlist [border w_point; spacer; border w_line; spacer; border w_ellipse;
    spacer; border w_checkbox; spacer; border w_undo;
    spacer; border w_quit]
  (* Modified to group using an hlist*)

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
    : widget * notifier_controller =
  let repaint_square (gc:gctx) =
	 let c = get_color () in
    fill_rect (with_color gc c) (0, width-1) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
   color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
   with. They are also installed with a mouseclick listener
   that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
    paint.color <- c ));
  w
(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
let color_toolbar : widget =
  Widget.hlist [color_indicator; spacer;
                color_button black; spacer;
                color_button white; spacer;
                color_button red; spacer;
                color_button green; spacer;
                color_button blue; spacer;
                color_button yellow; spacer;
                color_button cyan; spacer;
                color_button magenta; spacer]
  (* Modified to group using an hlist*)

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) modify the layout to add new buttons and make *)
(* the layout more aesthetically appealing.                                *)
let paint_widget = vlist [paint_canvas; spacer; mode_toolbar;
  spacer; color_toolbar; spacer; border w_textbox]
  (* Modified to group using a vlist*)
  
(**************************************)
(** Start the application             *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
