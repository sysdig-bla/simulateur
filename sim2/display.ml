
let segment = [| (0, 0); (5,-5); (45,-5); (50,0); (45,5); (5,5) |]
       
let offsets = [|  (10,60,true); (10,60,false); (10,10,true); (10,10,false);
(60,10,true); (60,60,true); (10,110,false); |]

let modpos = [| (150,0); (150,70); (150,140); (150,210); (150,310);
(150,380);
 (150,480); (150,550); (10,0); (10,70); (10,170); (10,240);
 (10,340); (10,410)|]

let display7 (pos_x, pos_y) values =
    let curr_poly = Array.make 6 (0,0) in
    for i = 0 to Array.length values - 1 do
        let (ox,oy,flip) = offsets.(i) in
        let (dx,dy) = (ox + pos_x, oy + pos_y) in
        for j = 0 to 5 do
            let (x,y) = segment.(j) in
            curr_poly.(j) <- if flip then (y + dx, x + dy) else (x + dx, y + dy)
        done;
        if flip then
            Graphics.moveto dx (25+dy)
        else Graphics.moveto (25+dx) dy;
        if values.(i) then
            Graphics.fill_poly curr_poly
        else 
            Graphics.draw_poly curr_poly
    done

let nb_values = 14
let screen_width = 650
let screen_height = 300

let invert_pair (x,y) = (y,x)

let display_values values =
    Graphics.clear_graph ();
    for i = 0 to nb_values - 1 do
        display7 (invert_pair modpos.(i)) values.(nb_values - 1 - i)
    done

    
let open_display () =
    Graphics.open_graph (Printf.sprintf " %dx%d" screen_width screen_height);
    display_values (Array.make nb_values (Array.make 7 false))

let update tab =
    let bound = min (Array.length tab) (7*nb_values) in
    let values =
        Array.init nb_values
        (fun k -> Array.init 7
        (fun j -> if (7*k + j) < bound then tab.(7*k+j) else false)) in
    display_values values


