open Printf
open ExtLib
open OUnit
open Sqlgg
open Sql
(* open Sql.Type *)
open Stmt

(* let cmp_params p1 p2 =
  try
    List.for_all2 (fun p1 p2 ->
      p1.id.label = p2.id.label && Type.equal p1.typ p2.typ && p1.id.pos = (0,0) && snd p2.id.pos > fst p2.id.pos)
    p1 p2
  with
    _ -> false

let parse sql =
  match Main.parse_one (sql,[]) with
  | exception exn -> assert_failure @@ sprintf "failed : %s : %s" (Printexc.to_string exn) sql
  | None -> assert_failure @@ sprintf "Failed to parse : %s" sql
  | Some stmt -> stmt

let do_test sql ?kind schema params =
  let stmt = parse sql in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string schema stmt.schema;
  assert_equal ~msg:"params" ~cmp:cmp_params ~printer:Sql.show_params params
    (List.map (function Single p -> p | SingleIn _ | Choice _ | ChoiceIn _ | TupleList _ -> assert false) stmt.vars);
  match kind with
  | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
  | None -> ()

let tt sql ?kind schema params =
  let test () = do_test sql ?kind schema params in
  sql >:: test

let wrong sql =
  sql >:: (fun () -> ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one' (sql,[])); false with _ -> true))

let attr ?(extra=[]) n d = make_attribute n (Some d) (Constraints.of_list extra)
let attr' ?(extra=[]) ?(nullability=Type.Strict) name kind =
  let domain: Type.t = { t = kind; nullability; } in
  {name;domain;extra=Constraints.of_list extra }

let named s t = new_param { label = Some s; pos = (0,0) } (Type.strict t)
let named_nullable s t = new_param { label = Some s; pos = (0,0) } (Type.nullable t)
let param_nullable t = new_param { label = None; pos = (0,0) } (Type.nullable t)
let param t = new_param { label = None; pos = (0,0) } (Type.strict t)

let test = Type.[
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)" [] [];
  tt "SELECT str FROM test WHERE id=?"
     [attr' ~nullability:(Nullable) "str" Text]
     [param_nullable Int];
   tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr' "x" Text; attr' ~nullability:(Nullable) "z" Int]
     [param_nullable Int; named "id" Int; named "lim" Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [  attr' ~nullability:(Nullable) "name" Text;
        attr' ~nullability:(Nullable) "other_name" Text
     ]
     [named_nullable "delta" Int];
  tt "select test.name from test where test.id + @x = ? or test.id - @x = ?"
     [attr' ~nullability:(Nullable) "name" Text;]
     [named_nullable "x" Int; param_nullable Int; named_nullable "x" Int; param_nullable Int;];
  tt "insert into test values"
     []
     [named_nullable "id" Int; named_nullable "str" Text; named_nullable "name" Text];
  tt "insert into test (str,name) values"
     []
     [named_nullable "str" Text; named_nullable "name" Text];
  tt "insert into test values (2,'hello' || ' world',@name)"
     []
     [named_nullable "name" Text];
  tt "insert or replace into test values (2,?,?)" [] [param_nullable Text; param_nullable Text;];
  tt "replace into test values (2,?,?)" [] [param_nullable Text; param_nullable Text;];
 tt "select str, case when id > @id then name when id < @id then 'qqq' else @def end as q from test"
    [attr' ~nullability:(Nullable) "str" Text; attr' ~nullability:(Nullable) "q" Text]
    [named_nullable "id" Int; named_nullable "id" Int; named_nullable "def" Text];
   wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr' ~nullability:(Nullable) "str" Text;]
    [named_nullable "id" Int; named_nullable "x" Int; named_nullable "id" Int];
  tt "select 3/5"
    [attr' ~nullability:(Strict) "" Float;]
    [];
]

let test2 = [
  tt "CREATE TABLE test2 (id INT, str TEXT)" [] [];
  tt    "update test, (select * from test2) as x set str = x.str where test.id=x.id" [] [];
  tt    "update test, (select * from test2) as x set name = x.str where test.id=x.id" [] [];
  tt    "update test, (select * from test2) as x set test.str = x.str where test.id=x.id" [] [];
  wrong "update test, (select * from test2) as x set test.name = x.name where test.id=x.id";
  wrong "update test, (select * from test2) as x set test.str = str where test.id=x.id";
]

let test3 = [
  tt "SELECT id FROM test WHERE str IN ( SELECT str FROM test2 )" [attr "id" Int] [];
  "tuples" >:: (fun () -> todo "tuples");
  (* from http://stackoverflow.com/questions/1063866/sql-portability-gotchas/1063946#1063946 *)
(*   tt "SELECT id FROM test WHERE (id, str) IN ( SELECT id, str FROM test2)" [attr "id" Int] []; *)
]

let test4 =
  let a = [attr "" Int] in
  [
  tt "CREATE TABLE test4 (x INT, y INT)" [] [];
  tt "select max(x) as q from test4" [attr "q" Int] [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x+y) from test4 limit 1" a [] ~kind:(Select `One);
  tt "select max(y) from test4 limit 2" a [] ~kind:(Select `One);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4 limit 1" a [] ~kind:(Select `Zero_one);
  tt "select max(x,y) from test4 limit 2" a [] ~kind:(Select `Nat);
  tt "select 1" [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10)"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10) where 1 = 2"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Zero_one);
  tt "select 1 from test4"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test"
    [attr' ~nullability:(Strict)  "" Int; attr' ~nullability:(Strict) "" Text] [] ~kind:(Select `Nat);
  tt "select greatest(10,x) from test4" [attr' ~nullability:(Nullable) "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr' ~nullability:(Nullable) "q" Int] [named_nullable "n" Int] ~kind:(Select `One);
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr' ~nullability:(Strict) "q" Int] [named_nullable "n" Int] ~kind:(Select `One); 
]

let test_parsing = [
  tt "CREATE TABLE test5_1 (x INT NOT NULL, y INT NOT NULL DEFAULT -1) ENGINE=MEMORY" [] [];
  tt "SELECT 2+3, 2+-3, -10 FROM test5_1" [attr' "" Int; attr' "" Int; attr' "" Int] [];
]

(*
  see MySQL 5.4 refman -- 12.2.8.1. JOIN Syntax
  see SQL:2008 -- 7.7 <joined table>
*)
let test_join_result_cols () =
  Tables.reset ();
  let ints = List.map (fun name ->
    if String.ends_with name "?" then
      Sql.{ name = String.slice ~last:(-1) name; domain = Type.(nullable Int); extra = Constraints.empty; }
    else
      attr name Int)
  in
  do_test "CREATE TABLE t1 (i INT, j INT)" [] [];
  do_test "CREATE TABLE t2 (k INT, j INT)" [] [];
  do_test "SELECT * FROM t1 JOIN t2 ON t1.j=t2.j" (ints ["i";"j";"k";"j"]) [];
  do_test "SELECT * FROM t1 LEFT JOIN t2 ON t1.j=t2.j" (ints ["i";"j";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 RIGHT JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k";"j"]) [];
  do_test "SELECT * FROM t1 FULL JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 NATURAL JOIN t2" (ints ["j";"i";"k"]) [];
  do_test "SELECT * FROM t1 JOIN t2 USING (j)" (ints ["j";"i";"k"]) [];
(*   NATURAL JOIN with common column in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE j > @x"
    (ints ["j";"i";"k"])
    [named_nullable "x" Int];
(*   NATURAL JOIN with common column qualified in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x"
    (ints ["j";"i";"k"])
    [named_nullable "x" Int];
  ()

let test_enum = [
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" Text ~extra:[NotNull;]; attr ~extra:[WithDefault;] "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" Text ~extra:[NotNull;]; attr "" Int] [];
]

let test_manual_param = [
  tt "CREATE TABLE test7 (x INT NULL DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test7 WHERE x = @x_arg" [attr "x" Int ~extra:[Null; WithDefault];] [
    named_nullable "x_arg" Int
  ];
  tt "SELECT * FROM test7 WHERE x = @x_arg::Int" [attr "x" Int ~extra:[Null; WithDefault];] [
    named "x_arg" Int
  ];
  tt "INSERT INTO test7 VALUES (@x_arg)" [] [
    named_nullable "x_arg" Int
  ];
  tt "UPDATE test7 SET x = @x_arg WHERE x = @x_arg_2" [] [
    named_nullable "x_arg" Int;
    named_nullable "x_arg_2" Int
  ];
  tt "UPDATE test7 SET x = @x_arg ::Int WHERE x = @x_arg_2 :: Int" [] [
    named "x_arg" Int;
    named "x_arg_2" Int
  ];
]

let test_left_join = [
  tt "CREATE TABLE account_types ( type_id INT NOT NULL PRIMARY KEY, type_name VARCHAR(255) NOT NULL )" [] [];
  tt "CREATE TABLE users (id INT NOT NULL, user_id INT NOT NULL PRIMARY KEY, name VARCHAR(255), email VARCHAR(255), account_type_id INT NULL, FOREIGN KEY (account_type_id) REFERENCES account_types(type_id))" [][];
  tt "SELECT users.name, users.email, account_types.type_name FROM users LEFT JOIN account_types ON users.account_type_id = account_types.type_id"
  [attr "name" Text ~extra:[]; attr "email" Text ~extra:[]; 
  {name="type_name"; domain=Type.nullable Text; extra=(Constraints.of_list [Constraint.NotNull]);}] [];
]

let test_coalesce = [
  tt "CREATE TABLE test8 (x integer unsigned null)" [] [];
  tt "SELECT COALESCE(x, null, null) as x FROM test8" [attr' ~nullability:(Nullable) "x" Int;] [];
  tt "SELECT COALESCE(x, coalesce(null, null, 75, null), null) as x FROM test8" [attr' ~nullability:Strict "x" Int;] [];
]

let test_coalesce = [
  tt "CREATE TABLE test9 (x BIGINT UNSIGNED PRIMARY KEY)" [] [];
  tt "SELECT x FROM test9 WHERE x > 100" [attr' ~extra:[PrimaryKey] ~nullability:(Strict) "x" Int;] [];
]

let test_not_null_default_field = [
  tt "CREATE TABLE test10 (id INT PRIMARY KEY, name VARCHAR(255) NOT NULL)" [] [];
  wrong "INSERT INTO test10 (id) VALUES (1)";
  tt "INSERT INTO test10 (id, name) VALUES (1, '2')" [] [];
  tt "CREATE TABLE test11 (aa int(10) unsigned NOT NULL DEFAULT 2, b TEXT NOT NULL)" [][];
  tt "INSERT INTO test11 (b) VALUES ('abcd')" [][];
]

let test_update_join = [
  tt "CREATE TABLE test12 (c_id INT PRIMARY KEY, c_name VARCHAR(50) NOT NULL)" [] [];
  tt "CREATE TABLE test13 (s_id INT PRIMARY KEY, s_name VARCHAR(50) NOT NULL, c_id INT NOT NULL)" [] [];
  tt "CREATE TABLE test14 (s_id INT PRIMARY KEY, g INT NOT NULL)" [] [];

  tt {|
    UPDATE test12
    JOIN test13 t13 ON t13.c_id = test12.c_id
    JOIN test14 t14 ON t14.s_id = t13.s_id
    SET t14.g = t14.g + 100, 
    test12.c_name = @c_name,
    t13.s_name = @s_name
    WHERE test12.c_id = @c_id
  |} [] [
    named "c_name" Text;
    named "s_name" Text;
    named "c_id" Int
  ]; *)
(* ] *)

let run () =
  Gen.params_mode := Some Named;
  let tests =
  [

  ]
  in
  let test_suite = "main" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
