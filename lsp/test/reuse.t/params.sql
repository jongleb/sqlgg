CREATE TABLE t (a INT, b INT);

-- @byA | include: reuse
SELECT a FROM t WHERE a = @x;

-- @twice
WITH c1 AS &byA, c2 AS &byA
SELECT a FROM c1;

-- @cte
WITH used AS (SELECT a FROM t), unused AS (SELECT a FROM t)
SELECT a AS x FROM used;

-- @unnamed
SELECT a FROM t WHERE b = ? AND a = @named;
