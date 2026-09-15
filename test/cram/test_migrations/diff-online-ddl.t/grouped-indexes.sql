-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_index_email_idx_add_index_name_idx
ALTER TABLE `users` ADD INDEX `email_idx` (`email`), ADD INDEX `name_idx` (`name`), ALGORITHM=INSTANT, LOCK=DEFAULT;
ALTER TABLE `users` DROP INDEX `name_idx`, DROP INDEX `email_idx`;
