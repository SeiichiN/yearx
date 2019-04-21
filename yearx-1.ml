(*
 * 年号変換プログラム ver.0.1
 *
 *     2019 Seiichi Nukayama
 *)
let ver = "0.1";;

(* ware = true --> グレゴリオ暦から和暦を求める処理
 * gre  = true --> 和暦からグレゴリオ暦を求める処理 *)
let ware = ref false
and gre = ref false;;

(* 和暦の換算リスト *)
let wareki_list =
    [("M", 1867); ("m", 1867); ("T", 1911); ("t", 1911);
     ("S", 1925); ("s", 1925); ("H", 1988); ("h", 1988);
     ("R", 2018); ("r", 2018)];;

(*
 * 和暦からグレゴリオ暦を求める
 * @param: nengo -- (string) たとえば "h23" などの文字列
 *         l     -- (list)   wareki_list を想定している
 * @return: suuji + num -- (int) gregorian year
 *)
let rec gregorio nengo l = 
    let nengo_len = String.length nengo in
    let alpha = String.sub nengo 0 1 in
    let suuji = String.sub nengo 1 (nengo_len - 1) in
    let num = int_of_string suuji in
    match l with
    [] -> 0
    | (gengo, suuji) :: rest ->
            if alpha = gengo then suuji + num
            else gregorio nengo rest;;
            
(*
 * グレゴリオ暦から和暦を求める
 * @param:  year -- (int) ex.1985 
 * @return:      -- ex.("昭和", 31)
 *)
let wareki year =
    if year <= 1867 then ("Edo", (year - 1603))
    else
        if (year > 1867 && year <= 1911) 
        then ("明治", (year - 1867))
        else
            if (year > 1911 && year <= 1925)
            then ("大正", (year - 1911))
            else
                if (year > 1925 && year <= 1988)
                then ("昭和", (year - 1925))
                else
                    if (year > 1988 && year <= 2018)
                    then ("平成", (year - 1988))
                    else
                        ("令和", (year - 2018));;

(*
 * コマンドライン引数処理
 * Arg.parse に渡す第1引数 spec の中身
 *)
let spec = [("-w", Arg.Set ware, "グレゴリオ暦（西暦） -> 和暦");
            ("-wareki" , Arg.Set ware, "グレゴリオ暦（西暦） -> 和暦");
            ("-g", Arg.Set gre, "和暦 -> グレゴリオ暦（西暦）");
            ("-gregorian", Arg.Set gre, "和暦 -> グレゴリオ暦（西暦）");
            ("-version",
            Arg.Unit (fun () -> Printf.printf "年号変換 ver: %s\n" ver),
            "Display version number")];;

(*
 * 画面処理 -- wareki 関数を呼び出す
 *)
let gre_wareki () =
    print_string "西暦を数字で入力 > ";
    flush stdout;
    let line = input_line stdin
    and y = ref "" in
    let year = int_of_string line in
    let (nengo, nen) = wareki year in
    if nen = 1 then  y := "元" else y := string_of_int nen;
    print_endline (nengo ^ " " ^ !y ^ "年");;

(*
 * 画面処理 -- gregorio 関数を呼び出す
 *)
let wareki_gre () =
    print_string "和暦を入力（例：h23）> ";
    flush stdout;
    let wa_year = input_line stdin in
    let gre_year = string_of_int (gregorio wa_year wareki_list) in
    print_endline ("西暦 " ^ gre_year ^ "年");;

(*
 * 引数が何も指定されなかった場合、これを画面に表示する
 *)
let pr_help () =
    print_endline "yearx の使い方";
    print_endline "yearx [-w | -wareki] [-g | -gregorian]";
    print_endline "yearx [-version] [-help | --help]";
    print_newline ();
    print_endline "オプション";
    print_string "-w | -wareki   ";
    print_endline "  グレゴリオ暦（西暦）-> 和暦";
    print_string "-g | -gregorian";
    print_endline "  和暦 -> グレゴリオ暦（西暦）";
    print_newline ();
    print_endline "    Copyright 2019 Seiichi Nukayama";
    flush stdout;;

(*
 * コマンド実行
 *)
let _ =
    Arg.parse spec
    (* 引数が何もなかった場合 *)
    (fun y -> if y = "" then pr_help() else ())
    (* 想定外の引数、あるいはエラーの場合 *)
    "Usage: yearx [-w] [-wareki] [-g] [-gregorian] [-version] ";
    if !ware then gre_wareki ()  (* !ware = true *)
    else
        if !gre then wareki_gre () else ();  (* !gre = true *)
    if (Array.length Sys.argv) = 1 then pr_help ();;

