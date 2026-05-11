numeric types — signed
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a TINYINT NOT NULL, b SMALLINT NOT NULL, c MEDIUMINT NOT NULL,
  >   d INT NOT NULL,     e BIGINT NOT NULL);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` TINYINT NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` SMALLINT NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` MEDIUMINT NOT NULL") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` INT NOT NULL") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` BIGINT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
    ]
  
  end (* module Mig *)

numeric types — unsigned
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a TINYINT UNSIGNED NOT NULL, b SMALLINT UNSIGNED NOT NULL,
  >   c MEDIUMINT UNSIGNED NOT NULL, d INT UNSIGNED NOT NULL,
  >   e BIGINT UNSIGNED NOT NULL);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` TINYINT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` SMALLINT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` MEDIUMINT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` INT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` BIGINT UNSIGNED NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
    ]
  
  end (* module Mig *)

float / double
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x FLOAT NOT NULL, y DOUBLE NOT NULL,
  >                                  z DOUBLE PRECISION NOT NULL);
  > ALTER TABLE t DROP COLUMN x;
  > ALTER TABLE t DROP COLUMN y;
  > ALTER TABLE t DROP COLUMN z;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` FLOAT NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN y") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `y` DOUBLE NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN z") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `z` DOUBLE NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

text size fidelity
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a TEXT, b TINYTEXT, c MEDIUMTEXT, d LONGTEXT);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` TEXT") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` TINYTEXT") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` MEDIUMTEXT") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` LONGTEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
    ]
  
  end (* module Mig *)

blob size fidelity
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a BLOB, b TINYBLOB, c MEDIUMBLOB, d LONGBLOB);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` BLOB") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` TINYBLOB") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` MEDIUMBLOB") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` LONGBLOB") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
    ]
  
  end (* module Mig *)

VARCHAR/CHAR/VARBINARY length fidelity (precision regression net)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a VARCHAR(1), b VARCHAR(255), c VARCHAR(65535),
  >   d CHAR(1), e CHAR(64), f VARBINARY(16), g VARBINARY(4096));
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN e;
  > ALTER TABLE t DROP COLUMN f;
  > ALTER TABLE t DROP COLUMN g;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` VARCHAR(1)") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` VARCHAR(255)") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` VARCHAR(65535)") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` CHAR(1)") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` CHAR(64)") T.no_params
  
    let apply_alter_t_6 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN f") T.no_params
  
    let revert_alter_t_6 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `f` VARBINARY(16)") T.no_params
  
    let apply_alter_t_7 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN g") T.no_params
  
    let revert_alter_t_7 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `g` VARBINARY(4096)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
      ("alter_t_6", [(apply_alter_t_6, revert_alter_t_6)]);
      ("alter_t_7", [(apply_alter_t_7, revert_alter_t_7)]);
    ]
  
  end (* module Mig *)

VARCHAR/CHAR/VARBINARY through CHANGE COLUMN inverse:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a VARCHAR(8) NOT NULL, b CHAR(2) NOT NULL, c VARBINARY(64) NOT NULL);
  > ALTER TABLE t CHANGE COLUMN a a VARCHAR(255) NOT NULL;
  > ALTER TABLE t CHANGE COLUMN b b CHAR(8) NOT NULL;
  > ALTER TABLE t CHANGE COLUMN c c VARBINARY(1024) NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN a a VARCHAR(255) NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `a` `a` VARCHAR(8) NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN b b CHAR(8) NOT NULL") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `b` `b` CHAR(2) NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN c c VARBINARY(1024) NOT NULL") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `c` `c` VARBINARY(64) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

oracle VARCHAR2 length fidelity:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, a VARCHAR2(4000) NOT NULL);
  > ALTER TABLE t DROP COLUMN a;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` VARCHAR2(4000) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

DECIMAL precision/scale fidelity
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a DECIMAL NOT NULL, b DECIMAL(5) NOT NULL,
  >   c DECIMAL(18,6) NOT NULL, d DECIMAL(38,12) NOT NULL,
  >   e NUMERIC NOT NULL, f NUMERIC(10,2) NOT NULL);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN e;
  > ALTER TABLE t DROP COLUMN f;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` DECIMAL NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` DECIMAL(5) NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` DECIMAL(18,6) NOT NULL") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` DECIMAL(38,12) NOT NULL") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` DECIMAL NOT NULL") T.no_params
  
    let apply_alter_t_6 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN f") T.no_params
  
    let revert_alter_t_6 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `f` DECIMAL(10,2) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
      ("alter_t_6", [(apply_alter_t_6, revert_alter_t_6)]);
    ]
  
  end (* module Mig *)

DATETIME / JSON / BOOLEAN
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   d DATETIME NOT NULL, j JSON NOT NULL, b BOOLEAN NOT NULL);
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN j;
  > ALTER TABLE t DROP COLUMN b;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` DATETIME NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN j") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `j` JSON NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` BOOLEAN NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

single ctor:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, e ENUM('only') NOT NULL);
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` ENUM('only') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

alphabetical reordering:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   e ENUM('zebra','apple','mango','banana') NOT NULL);
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` ENUM('apple', 'banana', 'mango', 'zebra') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

numeric-looking ctors:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, e ENUM('1','2','10','11') NOT NULL);
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` ENUM('1', '10', '11', '2') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

underscore ctors:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, e ENUM('a_b','a_a','b_a') NOT NULL);
  > ALTER TABLE t DROP COLUMN e;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` ENUM('a_a', 'a_b', 'b_a') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD COLUMN with inline constraints (apply preserved verbatim, revert is DROP)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t ADD COLUMN a INT;
  > ALTER TABLE t ADD COLUMN b INT NOT NULL;
  > ALTER TABLE t ADD COLUMN c INT NULL;
  > ALTER TABLE t ADD COLUMN d INT UNIQUE;
  > ALTER TABLE t ADD COLUMN e INT DEFAULT 1;
  > ALTER TABLE t ADD COLUMN f INT NOT NULL DEFAULT 7;
  > ALTER TABLE t ADD COLUMN g INT NOT NULL AUTO_INCREMENT PRIMARY KEY;
  > ALTER TABLE t ADD COLUMN h INT PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN a INT") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `a`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN b INT NOT NULL") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `b`") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN c INT NULL") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `c`") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN d INT UNIQUE") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `d`") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN e INT DEFAULT 1") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `e`") T.no_params
  
    let apply_alter_t_6 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN f INT NOT NULL DEFAULT 7") T.no_params
  
    let revert_alter_t_6 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `f`") T.no_params
  
    let apply_alter_t_7 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN g INT NOT NULL AUTO_INCREMENT PRIMARY KEY") T.no_params
  
    let revert_alter_t_7 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `g`") T.no_params
  
    let apply_alter_t_8 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN h INT PRIMARY KEY") T.no_params
  
    let revert_alter_t_8 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `h`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
      ("alter_t_6", [(apply_alter_t_6, revert_alter_t_6)]);
      ("alter_t_7", [(apply_alter_t_7, revert_alter_t_7)]);
      ("alter_t_8", [(apply_alter_t_8, revert_alter_t_8)]);
    ]
  
  end (* module Mig *)

DEFAULT — currently lost in revert:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a INT NOT NULL DEFAULT 7,
  >   b VARCHAR(8) NOT NULL DEFAULT 'x',
  >   c BOOLEAN NOT NULL DEFAULT TRUE);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` INT NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` VARCHAR(8) NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` BOOLEAN NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

UNIQUE / NOT NULL / NULL — preserved:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL,
  >   a INT UNIQUE,
  >   b INT NOT NULL,
  >   c INT NULL);
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` INT UNIQUE") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` INT NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` INT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

PRIMARY KEY + AUTO_INCREMENT inline — preserved:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL AUTO_INCREMENT PRIMARY KEY, x TEXT);
  > ALTER TABLE t DROP COLUMN id;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN id") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `id` INT PRIMARY KEY NOT NULL AUTO_INCREMENT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

column inside composite PRIMARY KEY (PK info NOT regenerated):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL, c TEXT, PRIMARY KEY (a, b));
  > ALTER TABLE t DROP COLUMN b;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

type widen, then revert restores narrow type:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n SMALLINT NOT NULL);
  > ALTER TABLE t CHANGE COLUMN n n BIGINT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN n n BIGINT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` SMALLINT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

nullability flip:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n INT NOT NULL);
  > ALTER TABLE t CHANGE COLUMN n n INT;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN n n INT") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n INT);
  > ALTER TABLE t CHANGE COLUMN n n INT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN n n INT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` INT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

rename via CHANGE:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t CHANGE COLUMN x y INT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN x y INT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `y` `x` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

MODIFY (no rename):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, val TEXT);
  > ALTER TABLE t MODIFY COLUMN val INT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t MODIFY COLUMN val INT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `val` `val` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

CHANGE preserving DEFAULT in source — DEFAULT lost:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n INT NOT NULL DEFAULT 7);
  > ALTER TABLE t CHANGE COLUMN n n BIGINT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN n n BIGINT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

CHANGE keeps source_kind precision:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n MEDIUMINT UNSIGNED NOT NULL);
  > ALTER TABLE t MODIFY COLUMN n BIGINT UNSIGNED NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t MODIFY COLUMN n BIGINT UNSIGNED NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` MEDIUMINT UNSIGNED NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD COLUMN positional — *POSITION LOST in revert (DROP doesn't carry pos)*
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD COLUMN a INT FIRST;
  > ALTER TABLE t ADD COLUMN b INT AFTER id;
  > ALTER TABLE t ADD COLUMN c INT;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN a INT FIRST") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `a`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN b INT AFTER id") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `b`") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN c INT") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `c`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

CHANGE COLUMN positional — position discarded in revert
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, n INT NOT NULL, m INT);
  > ALTER TABLE t CHANGE COLUMN m m INT NOT NULL FIRST;
  > ALTER TABLE t CHANGE COLUMN n n INT AFTER id;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN m m INT NOT NULL FIRST") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `m` `m` INT") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN n n INT AFTER id") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `n` `n` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

DROP PRIMARY KEY (single):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL PRIMARY KEY, x TEXT);
  > ALTER TABLE t DROP PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP PRIMARY KEY") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD PRIMARY KEY (`id`)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

DROP PRIMARY KEY (composite):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL, c TEXT, PRIMARY KEY (a, b));
  > ALTER TABLE t DROP PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP PRIMARY KEY") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD PRIMARY KEY (`a`, `b`)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD PRIMARY KEY (single + composite):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL);
  > ALTER TABLE t ADD PRIMARY KEY (a);
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL);
  > ALTER TABLE t ADD PRIMARY KEY (a, b);
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

DROP PRIMARY KEY when there is none — pk_cols is [] (degenerate):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL);
  > ALTER TABLE t DROP PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP PRIMARY KEY") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD PRIMARY KEY ()") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Multi-action ALTER (compound ordering preserved, then reverse)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b TEXT, c INT NOT NULL);
  > ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL,
  >                RENAME COLUMN a TO aa, ADD INDEX idx_d (d);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL,\n\
                 RENAME COLUMN a TO aa, ADD INDEX idx_d (d)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP INDEX `idx_d`, RENAME COLUMN `aa` TO `a`, DROP COLUMN `d`, ADD COLUMN `b` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ALTER chain — RENAME TABLE inside compound ALTER (effective_name fold)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t RENAME TO t1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME TO t1") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t1` RENAME TO `t`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t ADD COLUMN y INT, RENAME TO t2;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN y INT, RENAME TO t2") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t2` RENAME TO `t`, DROP COLUMN `y`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t RENAME TO t_mid, RENAME TO t_final;
  > EOF
  Failed : ALTER TABLE t RENAME TO t_mid, RENAME TO t_final
  Fatal error: exception Failure("no such table t")
  [2]

single rename:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE old_t (id INT NOT NULL);
  > RENAME TABLE old_t TO new_t;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_old_t_1 db  =
      T.execute db ("RENAME TABLE old_t TO new_t") T.no_params
  
    let revert_alter_old_t_1 db  =
      T.execute db ("RENAME TABLE `new_t` TO `old_t`") T.no_params
  
    let migrations = [
      ("alter_old_t_1", [(apply_alter_old_t_1, revert_alter_old_t_1)]);
    ]
  
  end (* module Mig *)

multi-pair rename:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE a (id INT NOT NULL);
  > CREATE TABLE b (id INT NOT NULL);
  > RENAME TABLE a TO a2, b TO b2;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_a_b_2 db  =
      T.execute db ("RENAME TABLE a TO a2, b TO b2") T.no_params
  
    let revert_alter_a_b_2 db  =
      T.execute db ("RENAME TABLE `a2` TO `a`, `b2` TO `b`") T.no_params
  
    let migrations = [
      ("alter_a_b_2", [(apply_alter_a_b_2, revert_alter_a_b_2)]);
    ]
  
  end (* module Mig *)

ALTER TABLE db.t ...:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE app.t (id INT NOT NULL, x TEXT);
  > ALTER TABLE app.t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_app_t_1 db  =
      T.execute db ("ALTER TABLE app.t DROP COLUMN x") T.no_params
  
    let revert_alter_app_t_1 db  =
      T.execute db ("ALTER TABLE `app`.`t` ADD COLUMN `x` TEXT") T.no_params
  
    let migrations = [
      ("alter_app_t_1", [(apply_alter_app_t_1, revert_alter_app_t_1)]);
    ]
  
  end (* module Mig *)

RENAME TABLE between two qualified names:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE a.t (id INT NOT NULL);
  > RENAME TABLE a.t TO b.t;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_a_t_1 db  =
      T.execute db ("RENAME TABLE a.t TO b.t") T.no_params
  
    let revert_alter_a_t_1 db  =
      T.execute db ("RENAME TABLE `b`.`t` TO `a`.`t`") T.no_params
  
    let migrations = [
      ("alter_a_t_1", [(apply_alter_a_t_1, revert_alter_a_t_1)]);
    ]
  
  end (* module Mig *)

ADD INDEX (named, single column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD INDEX idx_name (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_name (name)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP INDEX `idx_name`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD INDEX (named, multi-column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, a TEXT, b TEXT);
  > ALTER TABLE t ADD INDEX idx_ab (a, b);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_ab (a, b)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP INDEX `idx_ab`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

RENAME INDEX:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t RENAME INDEX old_idx TO new_idx;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME INDEX old_idx TO new_idx") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` RENAME INDEX `new_idx` TO `old_idx`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ALTER ... ADD INDEX (anonymous) — non invertible
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD INDEX (name);
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

ADD CONSTRAINT (named):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, email TEXT);
  > ALTER TABLE t ADD CONSTRAINT chk_email CHECK (email IS NOT NULL);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD CONSTRAINT chk_email CHECK (email IS NOT NULL)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP CONSTRAINT `chk_email`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD CONSTRAINT (anonymous) — non invertible:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, email TEXT);
  > ALTER TABLE t ADD CONSTRAINT CHECK (email IS NOT NULL);
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

DROP CONSTRAINT — non invertible:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t DROP CONSTRAINT chk_foo;
  > EOF
  ==> ALTER TABLE t DROP CONSTRAINT chk_foo
  Position 1:29 Tokens: CONSTRAINT chk_foo
  Error: Sqlgg.Sql_parser.MenhirBasics.Error
  Errors encountered, no code generated
  [1]

DROP FOREIGN KEY:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t DROP FOREIGN KEY fk_foo;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

DROP CHECK:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t DROP CHECK chk_foo;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

CREATE INDEX (single):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > CREATE INDEX idx_name ON t (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_create_index_idx_name db  =
      T.execute db ("CREATE INDEX idx_name ON t (name)") T.no_params
  
    let revert_create_index_idx_name db  =
      T.execute db ("DROP INDEX `idx_name` ON `t`") T.no_params
  
    let migrations = [
      ("create_index_idx_name", [(apply_create_index_idx_name, revert_create_index_idx_name)]);
    ]
  
  end (* module Mig *)

CREATE INDEX (multi-column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, a TEXT, b TEXT);
  > CREATE INDEX idx_ab ON t (a, b);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_create_index_idx_ab db  =
      T.execute db ("CREATE INDEX idx_ab ON t (a, b)") T.no_params
  
    let revert_create_index_idx_ab db  =
      T.execute db ("DROP INDEX `idx_ab` ON `t`") T.no_params
  
    let migrations = [
      ("create_index_idx_ab", [(apply_create_index_idx_ab, revert_create_index_idx_ab)]);
    ]
  
  end (* module Mig *)

CREATE UNIQUE INDEX:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, email TEXT);
  > CREATE UNIQUE INDEX uniq_email ON t (email);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_create_index_uniq_email db  =
      T.execute db ("CREATE UNIQUE INDEX uniq_email ON t (email)") T.no_params
  
    let revert_create_index_uniq_email db  =
      T.execute db ("DROP INDEX `uniq_email` ON `t`") T.no_params
  
    let migrations = [
      ("create_index_uniq_email", [(apply_create_index_uniq_email, revert_create_index_uniq_email)]);
    ]
  
  end (* module Mig *)

chain of charset changes (Inverse uses prior known charset):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t CONVERT TO CHARACTER SET utf8;
  > ALTER TABLE t CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
  > ALTER TABLE t CONVERT TO CHARACTER SET ascii;
  > EOF
  ==> ALTER TABLE t CONVERT TO CHARACTER SET ascii
  Position 1:44 Tokens: ascii
  Error: Sqlgg.Sql_parser.MenhirBasics.Error
  Errors encountered, no code generated
  [1]

single charset change (no prior charset known — placeholder comment):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t CONVERT TO CHARACTER SET utf8mb4;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CONVERT TO CHARACTER SET utf8mb4") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` (* unsupported: unknown previous charset *)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

TTL = col + INTERVAL N <unit>:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, ts DATETIME NOT NULL);
  > ALTER TABLE t TTL = ts + INTERVAL 7 DAY;
  > EOF
  Feature Ttl is not supported for dialect MySQL (supported by: TiDB) at TTL = ts + INTERVAL 7 DAY
  Errors encountered, no code generated
  [1]

TTL_ENABLE = 'true':
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t TTL_ENABLE = 'true';
  > EOF
  Feature Ttl is not supported for dialect MySQL (supported by: TiDB) at TTL_ENABLE = 'true'
  Errors encountered, no code generated
  [1]

REMOVE TTL — non invertible:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t REMOVE TTL;
  > EOF
  Feature Ttl is not supported for dialect MySQL (supported by: TiDB) at REMOVE TTL
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

INSERT without down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > INSERT INTO t (id, x) VALUES (1, 'hello');
  > EOF
  migrations mode: insert_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

INSERT + DELETE pair with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > INSERT INTO t (id, x) VALUES (1, 'hello');
  > DELETE FROM t WHERE id = 1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_insert_t_1 db  =
      T.execute db ("INSERT INTO t (id, x) VALUES (1, 'hello')") T.no_params
  
    let revert_insert_t_1 db  =
      T.execute db ("DELETE FROM t WHERE id = 1") T.no_params
  
    let migrations = [
      ("insert_t_1", [(apply_insert_t_1, revert_insert_t_1)]);
    ]
  
  end (* module Mig *)

UPDATE pair with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > UPDATE t SET x = 'new' WHERE id = 1;
  > UPDATE t SET x = 'old' WHERE id = 1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_update_t_1 db  =
      T.execute db ("UPDATE t SET x = 'new' WHERE id = 1") T.no_params
  
    let revert_update_t_1 db  =
      T.execute db ("UPDATE t SET x = 'old' WHERE id = 1") T.no_params
  
    let migrations = [
      ("update_t_1", [(apply_update_t_1, revert_update_t_1)]);
    ]
  
  end (* module Mig *)

down=explicit consumed by next statement
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > ALTER TABLE t DROP COLUMN x;
  > ALTER TABLE t ADD COLUMN x VARCHAR(99);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN x VARCHAR(99)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

down=explicit but no following statement — error:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  migrations mode: down=explicit but no following statement for alter_t_1
  Errors encountered, no code generated
  [1]

down=<custom SQL> string property (non-explicit)
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=ALTER TABLE t ADD COLUMN __marker INT
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN __marker INT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

@name with auto revert:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- @drop_x
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_drop_x db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_drop_x db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` TEXT") T.no_params
  
    let migrations = [
      ("drop_x", [(apply_drop_x, revert_drop_x)]);
    ]
  
  end (* module Mig *)

@name with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- @add_idx
  > -- [sqlgg] down=explicit
  > ALTER TABLE t ADD INDEX idx_x (x);
  > ALTER TABLE t DROP INDEX idx_x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_add_idx db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_x (x)") T.no_params
  
    let revert_add_idx db  =
      T.execute db ("ALTER TABLE t DROP INDEX idx_x") T.no_params
  
    let migrations = [
      ("add_idx", [(apply_add_idx, revert_add_idx)]);
    ]
  
  end (* module Mig *)

dialect=postgresql (drop column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect postgresql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

dialect=sqlite (add column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect sqlite -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t ADD COLUMN y TEXT;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN y TEXT") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `y`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

dialect=tidb (rename table):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect tidb -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t RENAME TO t2;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME TO t2") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t2` RENAME TO `t`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Multi-file: DDL in one file, migrations in another
  $ cat > _ddl.sql <<'EOF'
  > CREATE TABLE products (id INT NOT NULL, name TEXT, weight FLOAT);
  > EOF
  $ cat > _mig1.sql <<'EOF'
  > ALTER TABLE products ADD COLUMN color TEXT;
  > EOF
  $ cat > _mig2.sql <<'EOF'
  > ALTER TABLE products DROP COLUMN weight;
  > EOF
  $ sqlgg -no-header -dialect mysql -gen none _ddl.sql -gen caml -migrations -name mig _mig1.sql _mig2.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_products_0 db  =
      T.execute db ("ALTER TABLE products ADD COLUMN color TEXT") T.no_params
  
    let revert_alter_products_0 db  =
      T.execute db ("ALTER TABLE `products` DROP COLUMN `color`") T.no_params
  
    let apply_alter_products_0 db  =
      T.execute db ("ALTER TABLE products DROP COLUMN weight") T.no_params
  
    let revert_alter_products_0 db  =
      T.execute db ("ALTER TABLE `products` ADD COLUMN `weight` FLOAT") T.no_params
  
    let migrations = [
      ("alter_products_0", [(apply_alter_products_0, revert_alter_products_0)]);
      ("alter_products_0", [(apply_alter_products_0, revert_alter_products_0)]);
    ]
  
  end (* module Mig *)

schema state IS updated, so the later migrations file sees the post-ALTER schema.
  $ cat > _schema.sql <<'EOF'
  > CREATE TABLE t (id INT NOT NULL, x VARCHAR(8) NOT NULL);
  > ALTER TABLE t CHANGE COLUMN x x VARCHAR(64) NOT NULL;
  > EOF
  $ cat > _m.sql <<'EOF'
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  $ sqlgg -no-header -dialect mysql -gen none _schema.sql -gen caml -migrations -name mig _m.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_0 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_0 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` VARCHAR(64) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_0", [(apply_alter_t_0, revert_alter_t_0)]);
    ]
  
  end (* module Mig *)

ADD then DROP — DROP's revert should match ADD's column definition:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t ADD COLUMN x VARCHAR(64) NOT NULL DEFAULT 'd';
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN x VARCHAR(64) NOT NULL DEFAULT 'd'") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `x`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` VARCHAR(64) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

RENAME COLUMN then DROP — DROP's revert uses new name:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, old_name VARCHAR(8));
  > ALTER TABLE t RENAME COLUMN old_name TO new_name;
  > ALTER TABLE t DROP COLUMN new_name;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME COLUMN old_name TO new_name") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` RENAME COLUMN `new_name` TO `old_name`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN new_name") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `new_name` VARCHAR(8)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

CHANGE COLUMN then DROP — DROP's revert uses post-CHANGE definition:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x INT NOT NULL);
  > ALTER TABLE t CHANGE COLUMN x x BIGINT UNSIGNED NOT NULL;
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN x x BIGINT UNSIGNED NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `x` `x` INT NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` BIGINT UNSIGNED NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

RENAME TABLE then ALTER on new name:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t RENAME TO t2;
  > ALTER TABLE t2 DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME TO t2") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t2` RENAME TO `t`") T.no_params
  
    let apply_alter_t2_2 db  =
      T.execute db ("ALTER TABLE t2 DROP COLUMN x") T.no_params
  
    let revert_alter_t2_2 db  =
      T.execute db ("ALTER TABLE `t2` ADD COLUMN `x` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t2_2", [(apply_alter_t2_2, revert_alter_t2_2)]);
    ]
  
  end (* module Mig *)

XML basic ADD COLUMN:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t ADD COLUMN x VARCHAR(99) NOT NULL;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_t_1" apply="ALTER TABLE t ADD COLUMN x VARCHAR(99) NOT NULL" revert="ALTER TABLE `t` DROP COLUMN `x`"/>
  </migrations>

XML DROP COLUMN with ENUM (special chars escape probe):
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, status ENUM('pending','done') NOT NULL);
  > ALTER TABLE t DROP COLUMN status;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_t_1" apply="ALTER TABLE t DROP COLUMN status" revert="ALTER TABLE `t` ADD COLUMN `status` ENUM('done', 'pending') NOT NULL"/>
  </migrations>

XML compound ALTER:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b TEXT);
  > ALTER TABLE t DROP COLUMN b, ADD COLUMN c VARCHAR(8) NOT NULL,
  >                RENAME COLUMN a TO aa;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_t_1" apply="ALTER TABLE t DROP COLUMN b, ADD COLUMN c VARCHAR(8) NOT NULL,&#x0A;               RENAME COLUMN a TO aa" revert="ALTER TABLE `t` RENAME COLUMN `aa` TO `a`, DROP COLUMN `c`, ADD COLUMN `b` TEXT"/>
  </migrations>

XML CREATE INDEX:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > CREATE INDEX idx_name ON t (name);
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="create_index_idx_name" apply="CREATE INDEX idx_name ON t (name)" revert="DROP INDEX `idx_name` ON `t`"/>
  </migrations>

XML RENAME TABLE:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE old_t (id INT NOT NULL);
  > RENAME TABLE old_t TO new_t;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_old_t_1" apply="RENAME TABLE old_t TO new_t" revert="RENAME TABLE `new_t` TO `old_t`"/>
  </migrations>

Empty input:
  $ printf '' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let migrations = [
    ]
  
  end (* module Mig *)

Only CREATE TABLE — no migration emitted:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let migrations = [
    ]
  
  end (* module Mig *)

DROP TABLE without down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > DROP TABLE t;
  > EOF
  migrations mode: drop_t contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

DROP TABLE with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > -- [sqlgg] down=explicit
  > DROP TABLE t;
  > CREATE TABLE t (id INT NOT NULL);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_drop_t db  =
      T.execute db ("DROP TABLE t") T.no_params
  
    let revert_drop_t db  =
      T.execute db ("CREATE TABLE t (id INT NOT NULL)") T.no_params
  
    let migrations = [
      ("drop_t", [(apply_drop_t, revert_drop_t)]);
    ]
  
  end (* module Mig *)

SELECT in migrations — not a migration statement:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > SELECT * FROM t;
  > EOF
  migrations mode: unsupported statement type: SELECT * FROM t
  Errors encountered, no code generated
  [1]

SET — also Non_migration_stmt:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > SET @x = 1;
  > EOF
  ==> SET @x = 1
  Position 1:6 Tokens: @x = 1
  Error: Sqlgg.Sql_parser.MenhirBasics.Error
  Errors encountered, no code generated
  [1]

backtick-quoted names everywhere:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE `select` (`order` INT NOT NULL, `from` TEXT);
  > ALTER TABLE `select` DROP COLUMN `from`;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_select_1 db  =
      T.execute db ("ALTER TABLE `select` DROP COLUMN `from`") T.no_params
  
    let revert_alter_select_1 db  =
      T.execute db ("ALTER TABLE `select` ADD COLUMN `from` TEXT") T.no_params
  
    let migrations = [
      ("alter_select_1", [(apply_alter_select_1, revert_alter_select_1)]);
    ]
  
  end (* module Mig *)

column with reserved keyword name in CHANGE:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, `key` VARCHAR(8) NOT NULL);
  > ALTER TABLE t CHANGE COLUMN `key` `key` VARCHAR(64) NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN `key` `key` VARCHAR(64) NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `key` `key` VARCHAR(8) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

DROP COLUMN on VARCHAR(20) COLLATE utf8mb4_bin — COLLATE lost in revert:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, login VARCHAR(20) COLLATE utf8mb4_bin NOT NULL);
  > ALTER TABLE t DROP COLUMN login;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN login") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `login` VARCHAR(20) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

CHANGE COLUMN replacing collation — old COLLATE lost in revert:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, login VARCHAR(20) COLLATE utf8mb4_bin NOT NULL);
  > ALTER TABLE t CHANGE COLUMN login login VARCHAR(20) COLLATE utf8mb4_unicode_ci NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN login login VARCHAR(20) COLLATE utf8mb4_unicode_ci NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `login` `login` VARCHAR(20) NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

either name handling or VARCHAR(n) revert reconstruction is caught at once.
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, login VARCHAR(64) NOT NULL);
  > -- @shrink_login
  > ALTER TABLE t CHANGE COLUMN login login VARCHAR(16) NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_shrink_login db  =
      T.execute db ("ALTER TABLE t CHANGE COLUMN login login VARCHAR(16) NOT NULL") T.no_params
  
    let revert_shrink_login db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `login` `login` VARCHAR(64) NOT NULL") T.no_params
  
    let migrations = [
      ("shrink_login", [(apply_shrink_login, revert_shrink_login)]);
    ]
  
  end (* module Mig *)
