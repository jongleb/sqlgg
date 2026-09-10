CREATE TABLE users (id INT, name TEXT);
CREATE TABLE posts (id INT, user_id INT, title TEXT);

-- @active | include: reuse
SELECT id, name FROM users WHERE id = @id;

-- @written | include: reuse
SELECT user_id, title FROM posts WHERE title = @title;

-- @gone | include: reuse
SELECT id FROM companies WHERE id = @id;

-- @feed
WITH u AS &active, p AS &written
SELECT u.name, p.title FROM u JOIN p ON p.user_id = u.id;
