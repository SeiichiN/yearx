(*
 * 年号変換プログラム ver 0.4 -- labltk バージョン
 *
 * ChangeLog 
 *   0.1: 明治以降の年号と西暦を相互変換できるようにした。
 *   0.2: 文禄以降の年号にも対応できるようにした。
 *   0.3: GUIバージョンを作成。(2019.04.29)
 *   0.4: 和暦の選択をリストから選べるようにした。(2019.05.01)
 *
 * Copyright 2019 Seiichi Nukayama
 *)

open Tk

let ver = "0.4"

(* ware = true --> グレゴリオ暦から和暦を求める処理
 * gre  = true --> 和暦からグレゴリオ暦を求める処理 *)
let ware = ref false
and gre = ref false

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
      ("令和", 2018);
      ("未来", 2100)]

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
            else gregorio nengo rest
            
(*
 * グレゴリオ暦から和暦を求める
 * @param:  year -- (int) ex.1985
 *          (list) -- wareki_list 
 * @return:      -- ex.("昭和", 31)
 *)
let this_gengo = ref "" and this_year = ref 0

let rec wareki year = function
    [] -> ("", 0)
    | (gengo, nen) :: rest ->
        if nen < year 
        then
            (this_gengo := gengo;
            this_year := nen;
            wareki year rest)
        else
            (!this_gengo, year - !this_year)

let kotae = ref ""  (* 答えの文字 *)

let wareki_select = ref "";;  (* 和暦選択で選択した和暦 *)

let top = openTk ()  (* 一番基礎のウィンドウ -- Tcl/Tk initialize *)

let tv_kotae = Textvariable.create ()  (* 答えをいれる場所 *)

let print_kotae tv =
  Textvariable.set tv !kotae

let tv_wareki = Textvariable.create ()  (* 和暦を入れる場所 *)

let print_wareki tv =                (* tv に和暦をセットする *)
    Textvariable.set tv !wareki_select

(* GUIから和暦を読み取って、西暦変換関数を呼び出し、tvにセットする *)
let chg2seireki nengo entry_year =
  let year = Entry.get entry_year in
  let nengo_year = nengo ^ year in
  let seireki_year = gregorio nengo_year wareki_list in
  kotae := Printf.sprintf "西暦 %d年" seireki_year;
  print_kotae tv_kotae;;
  
(* GUIから西暦を読み取って、和暦変換関数を呼び出し、tvにセットする *)
let chg2nengo entry =
  let seireki_year = int_of_string (Entry.get entry) in
  let (nengo, year) = wareki seireki_year wareki_list in
  kotae := Printf.sprintf "%s %d 年" nengo year;
  print_kotae tv_kotae


(* -------------------- 和暦選択メニュー作成 ------------ *)

(*
 * メニューに年号リストを加える
 * @param -- menu         menuオブジェクト
 *           wareki_list  和暦リスト
 *)
let rec add_menu menu = function
    [] -> []
    | (nengo, year) :: rest ->
            Menu.add_command
            ~label:nengo
            ~command:(fun () -> wareki_select := nengo; print_wareki tv_wareki) menu;
            add_menu menu rest;;

(* -------------------- 1段目 ------------------------- *)
  
let label_title =
  let s = Printf.sprintf "年号<=>西暦 変換プログラム ver: %s" ver in
  Label.create top ~text:s ~relief:`Raised

let label_notice =
  let s = "どちらかに入力して右のボタンをクリックしてください。" in
  Label.create top ~text:s

(* -------------------- 2段目 ------------------------- *)
  
let btn1_frame = Frame.create top

let label_L_btn1 = Label.create btn1_frame ~text:"西暦"
and entry_btn1 = Entry.create btn1_frame ~width:6
and label_R_btn1 = Label.create btn1_frame ~text:"年"
  
let button_btn1 = Button.create btn1_frame ~text:"年号に変換"
                                ~command:(fun () -> chg2nengo entry_btn1)

(* -------------------- 3段目 ------------------------- *)
  
let btn2_frame = Frame.create top

let mb = Menubutton.create btn2_frame ~text:"年号選択" ~relief:`Raised
and label_L_btn2 = Label.create btn2_frame ~textvariable:tv_wareki ~width:6 ~background:`White
and label_C_btn2 = Label.create btn2_frame ~text:"-"
and entry2_btn2 = Entry.create btn2_frame ~width:4
and label_R_btn2 = Label.create btn2_frame ~text:"年"

let menu = Menu.create mb;;

let button_btn2 = Button.create btn2_frame ~text:"西暦に変換"
                 ~command:(fun () -> chg2seireki !wareki_select entry2_btn2)
  

(* -------------------- 4段目 ------------------------- *)
  
let btn3_frame = Frame.create top
  
let label_kotae = Label.create btn3_frame ~textvariable:tv_kotae ~width:30

let button_end = Button.create btn3_frame ~text:"終了" ~command:(fun () -> closeTk(); exit 0)

(* ----------------------- コマンド --------------------------- *)
let () =
    add_menu menu wareki_list;

    Menubutton.configure ~menu:menu mb;

    (* -------------------------- pack -------------------------------------- *)
    pack [coe label_L_btn1; coe entry_btn1; coe label_R_btn1; coe button_btn1] ~side:`Left;

    pack [coe mb; coe label_L_btn2; coe label_C_btn2; coe entry2_btn2; coe label_R_btn2; coe button_btn2] ~side:`Left;

    pack [coe label_kotae; coe button_end] ~side:`Left;

    pack [coe label_title; coe label_notice; coe btn1_frame; coe btn2_frame; coe btn3_frame] ~side:`Top;

    mainLoop()
