CREATE TABLE users (id INT, name TEXT);

CREATE TABLE u (id INT, secret TEXT);

CREATE TABLE shadow (id INT, secret TEXT);

-- @alias_wins
SELECT u.name FROM users u;

-- @derived_wins
SELECT shadow.id FROM (SELECT id FROM users) shadow;

-- @cte_wins
WITH shadow AS (SELECT id FROM users)
SELECT shadow.id FROM shadow;
