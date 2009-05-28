(*-*-coding: utf-8;-*-*)

let dir = "/home/pps/vouillon/public_html/seminaire"

let parse_only = false

(****)

let read_file f =
  let c = open_in f in
  let b = Buffer.create 1024 in
  let s = String.create 1024 in
  begin try
    while true do
      let n = input c s 0 1024 in
      if n = 0 then raise End_of_file;
      Buffer.add_substring b s 0 n
    done
  with End_of_file -> () end;
  Buffer.contents b

(****)

let code =
  [| 0x0000; 0x0001; 0x0002; 0x0003; 0x0004; 0x0005; 0x0006; 0x0007;
     0x0008; 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F;
     0x0010; 0x0011; 0x0012; 0x0013; 0x0014; 0x0015; 0x0016; 0x0017;
     0x0018; 0x0019; 0x001A; 0x001B; 0x001C; 0x001D; 0x001E; 0x001F;
     0x0020; 0x0021; 0x0022; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027;
     0x0028; 0x0029; 0x002A; 0x002B; 0x002C; 0x002D; 0x002E; 0x002F;
     0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
     0x0038; 0x0039; 0x003A; 0x003B; 0x003C; 0x003D; 0x003E; 0x003F;
     0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
     0x0048; 0x0049; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F;
     0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057;
     0x0058; 0x0059; 0x005A; 0x005B; 0x005C; 0x005D; 0x005E; 0x005F;
     0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
     0x0068; 0x0069; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F;
     0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077;
     0x0078; 0x0079; 0x007A; 0x007B; 0x007C; 0x007D; 0x007E; 0x007F;
     0x20AC; 0x1234; 0x201A; 0x0192; 0x201E; 0x2026; 0x2020; 0x2021;
     0x02C6; 0x2030; 0x0160; 0x2039; 0x0152; 0x1234; 0x017D; 0x1234;
     0x1234; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2013; 0x2014;
     0x02DC; 0x2122; 0x0161; 0x203A; 0x0153; 0x1234; 0x017E; 0x0178;
     0x00A0; 0x00A1; 0x00A2; 0x00A3; 0x00A4; 0x00A5; 0x00A6; 0x00A7;
     0x00A8; 0x00A9; 0x00AA; 0x00AB; 0x00AC; 0x00AD; 0x00AE; 0x00AF;
     0x00B0; 0x00B1; 0x00B2; 0x00B3; 0x00B4; 0x00B5; 0x00B6; 0x00B7;
     0x00B8; 0x00B9; 0x00BA; 0x00BB; 0x00BC; 0x00BD; 0x00BE; 0x00BF;
     0x00C0; 0x00C1; 0x00C2; 0x00C3; 0x00C4; 0x00C5; 0x00C6; 0x00C7;
     0x00C8; 0x00C9; 0x00CA; 0x00CB; 0x00CC; 0x00CD; 0x00CE; 0x00CF;
     0x00D0; 0x00D1; 0x00D2; 0x00D3; 0x00D4; 0x00D5; 0x00D6; 0x00D7;
     0x00D8; 0x00D9; 0x00DA; 0x00DB; 0x00DC; 0x00DD; 0x00DE; 0x00DF;
     0x00E0; 0x00E1; 0x00E2; 0x00E3; 0x00E4; 0x00E5; 0x00E6; 0x00E7;
     0x00E8; 0x00E9; 0x00EA; 0x00EB; 0x00EC; 0x00ED; 0x00EE; 0x00EF;
     0x00F0; 0x00F1; 0x00F2; 0x00F3; 0x00F4; 0x00F5; 0x00F6; 0x00F7;
     0x00F8; 0x00F9; 0x00FA; 0x00FB; 0x00FC; 0x00FD; 0x00FE; 0x00FF |]
(*
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18;
     19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34;
     35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50;
     51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63; 64; 65; 66;
     67; 68; 69; 70; 71; 72; 73; 74; 75; 76; 77; 78; 79; 80; 81; 82;
     83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96; 97; 98;
     99; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111;
     112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122; 123; 124;
     125; 126; 127; 8364; 129; 8218; 131; 8222; 8230; 8224; 8225; 136;
     8240; 352; 8249; 346; 356; 381; 377; 144; 8216; 8217; 8220; 8221;
     8226; 8211; 8212; 152; 8482; 353; 8250; 347; 357; 382; 378; 160;
     711; 728; 321; 164; 260; 166; 167; 168; 169; 350; 171; 172; 173;
     174; 379; 176; 177; 731; 322; 180; 181; 182; 183; 184; 261; 351;
     187; 376; 733; 317; 380; 340; 193; 194; 258; 196; 313; 262; 199;
     268; 201; 280; 203; 282; 205; 206; 270; 272; 323; 327; 211; 212;
     336; 214; 215; 344; 366; 218; 368; 220; 221; 354; 223; 341; 225;
     226; 259; 228; 314; 263; 231; 269; 233; 281; 235; 283; 237; 238;
     271; 273; 324; 328; 243; 244; 337; 246; 247; 345; 367; 250; 369;
     252; 253; 355; 729 |]
*)

let rec transcode_rec buf s i l =
  if i < l then begin
    let c = code.(Char.code s.[i]) in
    if c < 0x80 then
      Buffer.add_char buf (Char.chr c)
    else if c < 0x800 then begin
      Buffer.add_char buf (Char.chr (c lsr 6 + 0xC0));
      Buffer.add_char buf (Char.chr (c land 0x3f + 0x80))
    end else if c < 0x10000 then begin
      Buffer.add_char buf (Char.chr (c lsr 12 + 0xE0));
      Buffer.add_char buf (Char.chr ((c lsr 6) land 0x3f + 0x80));
      Buffer.add_char buf (Char.chr (c land 0x3f + 0x80))
    end;
    transcode_rec buf s (i + 1) l
  end

let entity_re = Str.regexp "&\\([^;]+\\);"

let entities =
  ["agrave", "à";
   "and", "&";
   "auml", "ä";
   "eacute", "é";
   "ecirc", "ê";
   "acirc", "â";
   "egrave", "è";
   "eta", "η";
   "forall", "∀";
   "infin", "∞";
   "lambda", "λ";
   "ldquo", "“";
   "le", "≤";
   "lowast", "∗";
   "lsquo", "‘";
   "lt", "<";
   "mdash", "—";
   "mu", "μ";
   "nabla", "∇";
   "nu", "ν";
   "omega", "ω";
   "ouml", "ö";
   "oumlaut", "ö";
   "pi", "π";
   "Pi", "Π";
   "rarr", "→";
   "rdquo", "”";
   "rsquo", "’";
   "sigma", "σ";
   "times", "×";
   "ugrave", "ù"]

let transcode s =
  let buf = Buffer.create 32 in
  transcode_rec buf s 0 (String.length s);
  let s = Buffer.contents buf in
  Str.global_substitute entity_re
    (fun s ->
       let e = Str.matched_group 1 s in
       try
         List.assoc e entities
       with Not_found ->
         Format.eprintf "xxxxxxxx %s@." e;
         Format.sprintf "&%s;" e)
    s

(****)

let spaces = "[ \t\r\n]*"
let uint = "[0-9]+"
let group s = "\\(" ^ s ^ "\\)"
let any = group "[^a]\\|a"
let non_space = "[^ \t\r\n]"
let date = uint ^ "/" ^ uint ^ "/" ^ uint

(****)

open CalendarLib

let start_time = Time.make 11 00 00
let end_time = Time.make 12 30 00

(****)

let date_re = Str.regexp (group uint ^ "/" ^ group uint  ^ "/" ^ group uint)

let parse_date s =
  ignore (Str.string_match date_re s 0);
  let d = int_of_string (Str.matched_group 1 s) in
  let m = int_of_string (Str.matched_group 2 s) in
  let y = int_of_string (Str.matched_group 3 s) in
  let date = Date.make y m d in
  (Calendar.create date start_time, Calendar.create date end_time)

(****)

let trim_re = Str.regexp (spaces ^ group (any ^ "*" ^ non_space))
let trim s =
  try
    ignore (Str.search_forward trim_re s 0);
    Str.matched_group 1 s
  with Not_found ->
    ""
let spaces_re = Str.regexp ("\\([ \t\r\n]\\|<br>\\)+")

let normalize s = transcode (trim (Str.global_replace spaces_re " " s))

(****)

let newlines = Str.regexp "\n\n+"
let btag s = "<" ^ s ^ ">"
let etag s = "</" ^ s ^ ">"
let bpar = Str.regexp (spaces ^ btag "p\\( [^>]+\\)?" ^ spaces)
let epar = Str.regexp (spaces ^ etag "p" ^ spaces)
let br = Str.regexp (spaces ^ btag "br" ^ spaces)
let em = Str.regexp (btag "em" ^ "\\(\\([^<]\\|<[^/]\\|</[^e]\\)*\\)" ^ etag "em")
let it = Str.regexp (btag "i" ^ "\\(\\([^<]\\|<[^/]\\|</[^i]\\)*\\)" ^ etag "i")
let sub = Str.regexp (btag "sub" ^ "\\([^<]*\\)" ^ etag "sub")
let sup = Str.regexp (btag "sup" ^ "\\([^<]*\\)" ^ etag "sup")
let code = Str.regexp (btag "code" ^ "\\([^<]*\\)" ^ etag "code")
let anchor =
  Str.regexp ("<a" ^ spaces ^ "href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>")
let ul = Str.regexp (spaces ^ btag "ul" ^ "\\(\\([^<]\\|<[^/]\\|</[^u]\\)*\\)" ^ etag "ul" ^ spaces)
let ol = Str.regexp (spaces ^ btag "ol" ^ "\\(\\([^<]\\|<[^/]\\|</[^o]\\)*\\)" ^ etag "ol" ^ spaces)
let eli = Str.regexp (spaces ^ etag "li" ^ spaces)
let li = Str.regexp (spaces ^ btag "li" ^ spaces)

let subst_li b s =
  let s = Str.replace_matched "\\1" s in
  Str.global_replace li ("\n" ^ b ^ " ") s ^ "\n"

let wikify s =
  let s = Str.global_replace epar "\n" s in
  let s = Str.global_replace newlines "\n" s in
  let s = Str.global_replace br "\\\\\\\\" s in
  let s = String.concat "\n\n" (Str.split bpar s) in
  let s = Str.global_replace em "//\\1//" s in
  let s = Str.global_replace it "//\\1//" s in
  let s = Str.global_replace sub "_\\1" s in
  let s = Str.global_replace sup "^\\1" s in
  let s = Str.global_replace code "{{{\\1}}}" s in
  let s = Str.global_replace anchor "[[\\1|\\2]]" s in
  let s = Str.global_replace eli "" s in
  let s = Str.global_substitute ul (subst_li "*") s in
  let s = Str.global_substitute ol (subst_li "#") s in
  s

(****)

let header =
  Str.regexp
    (String.concat spaces
       ["<h3>"; "\\(<p>\\)?"; group date; ":"; group "[^<(,]+";
        group ("[(,]" ^ group "[^<)]+" ^ ")?") ^ "?"; "\\(<br>\\|---\\)";
        group "\\([^<]\\|<br>\\)*"; "</h3>";
        group "\\([^<]\\|<br>\\)+" ^ "?";
        "<center>"; "<strong>"; "[^<]+"; "</strong>"; "</center>"])

let trailer = Str.regexp "<BR><HR>"

let remove_dot s =
  let l = String.length s in
  if s <> "" && s.[l - 1] = '.' then String.sub s 0 (l - 1) else s

let parse_old_format s =
  ignore (Str.search_forward header s 0);
  let date = Str.matched_group 2 s in
  let speaker = Str.matched_group 3 s in
  let affiliation = try Str.matched_group 5 s with Not_found -> "" in
  let title = Str.matched_group 7 s in
(*
  begin try
    Format.eprintf "====> <%s>@." (Str.matched_group 9 s)
  with Not_found -> () end;
*)
  let start_sum = Str.group_end 0 in
  let end_sum = Str.search_backward trailer s (String.length s) in
  let abstract = String.sub s start_sum (end_sum - start_sum) in
  let speaker = normalize speaker in
  let affiliation = normalize affiliation in
  let title = remove_dot (normalize title) in
  let abstract = wikify (transcode (trim abstract)) in
(*
  Format.eprintf "%s {%s} {%s} {%s}@." date speaker affiliation title;
*)
  let date = parse_date date in
(*
  Format.eprintf "%s@." (Printer.Calendar.to_string (fst date));
  Format.eprintf "{%s}@." abstract;
*)
  (date, speaker, affiliation, title, abstract)

(****)

let header =
  Str.regexp
    (String.concat spaces
       ["<h2>"; group date; "<br>"; group "[^<(]+";
        group ("(" ^ group "[^<)]+" ^ ")") ^ "?"; "<br>";
        "<span[^>]+>"; group "\\([^<]\\|<br>\\)*"; "</span>"; "</h2>"])
let trailer = Str.regexp "</body>"

let parse_new_format s =
  ignore (Str.search_forward header s 0);
  let date = Str.matched_group 1 s in
  let speaker = Str.matched_group 2 s in
  let affiliation = try Str.matched_group 4 s with Not_found -> "" in
  let title = Str.matched_group 5 s in
  let start_sum = Str.group_end 0 in
  let end_sum = Str.search_backward trailer s (String.length s) in
  let abstract = String.sub s start_sum (end_sum - start_sum) in
  let speaker = normalize speaker in
  let affiliation = normalize affiliation in
  let title = normalize title in
  let abstract = wikify (transcode (trim abstract)) in
(*
  Format.eprintf "%s {%s} {%s} {%s}@." date speaker affiliation title;
*)
  let date = parse_date date in
(*
  Format.eprintf "%s@." (Printer.Calendar.to_string (fst date));
  Format.eprintf "{%s}@." abstract;
*)
  (date, speaker, affiliation, title, abstract)

(****)

let ignored_files =
  ["provasem.html"; "sem.html"; "mon_seminaire.html"; "seminaire.html"]

include Load_sql

let create_wiki () =
  let wikibox_widget = Ocsisite.wikibox_widget in
  Lwt_unix.run
    (Wiki_services.create_and_register_wiki
       ~title:"Announcements" ~descr:"Announcement manager" ~wikibox_widget ())

let et = Str.regexp_string " et "
let persons = Hashtbl.create 101
let person_id dbh speaker affiliation =
  try Hashtbl.find persons (speaker, affiliation) with Not_found ->
  let id = insert_person dbh speaker affiliation in
  Hashtbl.add persons (speaker, affiliation) id;
  id
let person_ids dbh speakers affiliation =
  List.map (fun speaker -> person_id dbh speaker affiliation)
     (Str.split et speakers)

let _ =
try
let author = Users.admin.Users.id in
let wiki = create_wiki () in
let dbh = PGOCaml.connect () in
let category = if parse_only then 0l else create_categories dbh wiki author in
for year = 1999 to 2008 do
  let subdir =
    if year = 1999 then "sem1999/" else Format.sprintf "sem%d/abstracts/" year
  in
  let dir = Filename.concat dir subdir in
  let files = Sys.readdir dir in
  Array.iter
    (fun f ->
       if
         Filename.check_suffix f ".html" && not (List.mem f ignored_files)
       then begin
         let f = Filename.concat dir f in
(*
         Format.eprintf "%s@." f;
*)
         let s = read_file f in
         let ((start, finish), speakers, affiliation, title, abstract) =
           if year >= 2002 then parse_new_format s else
           parse_old_format s
         in
         if not parse_only then begin
           let person_ids = person_ids dbh speakers affiliation in
           let abstract = insert_text wiki author abstract in
           let event =
             insert dbh category start finish person_id title abstract
           in
           List.iter (fun id -> insert_speaker dbh event id) person_ids
         end
       end)
    files
done
with e ->
Printexc.catch (fun () -> raise e) ()

let _ = exit 0
