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


let () =
    print_string "西暦を数字で入力 > ";
    flush stdout;
    let line = input_line stdin
    and y = ref "" in
    let year = int_of_string line in
    let (nengo, nen) = wareki year wareki_list in
    if nen = 1 then  y := "元" else y := string_of_int nen;
    print_endline (nengo ^ " " ^ !y ^ "年");;
