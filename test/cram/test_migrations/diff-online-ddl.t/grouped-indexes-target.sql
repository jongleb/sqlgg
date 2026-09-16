CREATE TABLE users (id INT NOT NULL, email VARCHAR(255), name VARCHAR(255));
ALTER TABLE users
  ADD INDEX email_idx (email),
  ADD INDEX name_idx (name),
  ALGORITHM=INSTANT,
  LOCK=DEFAULT;
