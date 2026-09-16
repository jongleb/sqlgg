Explicit ALTER TABLE ALGORITHM/LOCK clauses in target sources are preserved
on generated up migrations only.

MySQL preserves ALGORITHM and LOCK; down remains bare:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-explicit-mysql.sql | diff add-index-online-mysql.sql -

TiDB preserves ALGORITHM:

  $ sqlgg -no-header -dialect tidb -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-explicit-tidb.sql | diff add-index-online-tidb.sql -

TiDB rejects LOCK:

  $ sqlgg -no-header -dialect tidb -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-lock-tidb.sql 2>&1
  Feature AlterLock is not supported for dialect TiDB (supported by: MySQL) at LOCK=NONE
  [1]

PostgreSQL rejects ALGORITHM:

  $ sqlgg -no-header -dialect postgresql -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-algorithm-postgresql.sql 2>&1
  Feature AlterAlgorithm is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at ALGORITHM=INPLACE
  [1]

An explicit ALTER without clauses keeps the old output:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-explicit-plain.sql | diff add-index-plain.sql -

An unhinted column addition stays before a hinted index that depends on it:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base dependency-order-initial.sql -target dependency-order-target.sql | diff dependency-order.sql -

Hinted, plain, hinted ALTERs keep exact source order:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base two-indexes-initial.sql -target two-indexes-target.sql | diff two-indexes.sql -

Actions from one source ALTER stay grouped:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base two-indexes-initial.sql -target grouped-indexes-target.sql | diff grouped-indexes.sql -

A target expressed only as CREATE TABLE does not acquire clauses:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-target.sql | diff add-index-plain.sql -
