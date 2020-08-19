let mk_hexagon name r (cx, cy) =
  let points = ((cx-30, cy), (cx-15, cy-26), (cx+15, cy-26),
    (cx+30, cy), (cx+15, cy+26), (cx-15, cy+26))
  in
  let (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5, y5) = points in
  let points:string = Printf.sprintf
    "%d,%d %d,%d %d,%d %d,%d %d,%d %d,%d %d,%d"
    x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x0 y0
  in
  Printf.sprintf "<polygon id='%s' class='%s' points='%s'></polygon>"
    name r points

let mk_text center txt =
  Printf.sprintf "<text x='%d' y='%d'>%s</text>" (fst center) (snd center) txt

let mk_line (x1,y1) (x2,y2) =
  Printf.sprintf "<line x1='%f' y1='%f' x2='%f' y2='%f' class='river' />"
    x1 y1 x2 y2

let mk_arc_line (x1,y1) (x2,y2) (cx,cy) =
  Printf.sprintf "<path d='M %f %f Q %f %f, %f %f' style='stroke:blue; stroke-width:2'/>"
    x1 y1 cx cy x2 y2

let mk_use svgname (cx, cy) =
  Printf.sprintf "<use href='/dist/res/%s.svg#main' x='%d' y='%d' width='30' height='30'/>" svgname cx cy

let mk_group name inner_html =
  Printf.sprintf "<g id='%s'>%s</g>" name inner_html
