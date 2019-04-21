(*
 * 年号変換プログラム ver 0.2
 *
 *     2019 Seiichi Nukayama
 *)
let ver = "0.2";;

(* ware = true --> グレゴリオ暦から和暦を求める処理
 * gre  = true --> 和暦からグレゴリオ暦を求める処理 *)
let ware = ref false
and gre = ref false;;

(* 和暦の換算リスト *)
let wareki_list =
    [
      ("文禄", 1591);
      ("慶長", 1594);
      ("元和", 1614);
      ("寛永", 1623);
      ("正保", 1643);
      ("慶安", 1647);
      ("承応", 1651);
      ("明暦", 1654);
      ("万治", 1657);
      ("寛文", 1660);
      ("延宝", 1672);
      ("天和", 1680);
      ("貞享", 1683);
      ("元禄", 1687);
      ("宝永", 1703);
      ("正徳", 1710);
      ("享保", 1715);
      ("元文", 1735);
      ("寛保", 1740);
      ("延享", 1743);
      ("寛延", 1747);
      ("宝歴", 1750);
      ("明和", 1763);
      ("安永", 1771);
      ("天明", 1780);
      ("寛政", 1788);
      ("享和", 1800);
      ("文化", 1803);
      ("文政", 1817);
      ("天保", 1829);
      ("弘化", 1843);
      ("嘉永", 1847);
      ("安政", 1853);
      ("万延", 1859);
      ("文久", 1860);
      ("元治", 1863);
      ("慶応", 1864);
      ("明治", 1866);
      ("大正", 1911);
      ("昭和", 1925);
      ("平成", 1988);
      ("令和", 2018)];;

(*
 * 和暦からグレゴリオ暦を求める
 * @param: nengo -- (string) たとえば "元禄14" などの文字列
 *         l     -- (list)   wareki_list を想定している
 * @return: suuji + num -- (int) gregorian year
 *)
let rec gregorio nengo l = 
    let nengo_len = String.length nengo in
    let moji = String.sub nengo 0 6 in
    let suuji = String.sub nengo 6 (nengo_len - 6) in
    let num = int_of_string suuji in
    match l with
    [] -> 0
    | (gengo, suuji) :: rest ->
            if moji = gengo then suuji + num
            else gregorio nengo rest;;
            
(*
 * グレゴリオ暦から和暦を求める
 * @param:  year -- (int) ex.1985 
 * @return:      -- ex.("昭和", 31)
 *)
let this_gengo = ref "" and this_year = ref 0;;

let rec wareki year = function
    [] -> ("", 0)
    | (gengo, nen) :: rest ->
        if nen < year 
        then
            (this_gengo := gengo;
            this_year := nen;
            wareki year rest)
        else
            (!this_gengo, year - !this_year);;

  
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
    let (nengo, nen) = wareki year wareki_list in
    if nen = 1 then  y := "元" else y := string_of_int nen;
    print_endline (nengo ^ " " ^ !y ^ "年");;

(*
 * 画面処理 -- gregorio 関数を呼び出す
 *)
let wareki_gre () =
    print_string "和暦を入力（例：元禄14）> ";
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

