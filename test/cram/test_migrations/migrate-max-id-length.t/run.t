Migration ids are <timestamp>_<name>. Siblings of one run share the timestamp, so
the name is all that tells them apart, and it is stored in a finite name column.
`-max-migration-id-length` caps the id by building the name up to the cap instead
of letting the database chop it.

Uncapped - two tables with a shared prefix, four columns added to each:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -base initial.sql -target target.sql | grep -o 'id=.*'
  id=20260101120000_alter_blog_content_sync_jobs_full_add_col_redirects_synced_add_col_redirects_skipped_add_col_redirects_unsupported_add_col_redirects_errors
  id=20260101120000_alter_blog_content_sync_jobs_incremental_add_col_redirects_synced_add_col_redirects_skipped_add_col_redirects_unsupported_add_col_redirects_errors

Actions drop off the tail whole; one that no longer fits whole goes in as its bare
verb, and nothing follows it:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 100 -base initial.sql -target target.sql | grep -o 'id=.*'
  id=20260101120000_alter_blog_content_sync_jobs_full_add_col_redirects_synced_add_col_redirects_skipped
  id=20260101120000_alter_blog_content_sync_jobs_incremental_add_col_redirects_synced_add_col

Down to the verb of the first action:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 70 -base initial.sql -target target.sql | grep -o 'id=.*'
  id=20260101120000_alter_blog_content_sync_jobs_full_add_col
  id=20260101120000_alter_blog_content_sync_jobs_incremental_add_col

No action fits, so the table name gives up whole words - still distinct while the
word that differs survives:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 50 -base initial.sql -target target.sql | grep -o 'id=.*'
  id=20260101120000_alter_blog_content_sync_jobs_full
  id=20260101120000_alter_blog_content_sync_jobs

Cut past the word that differs, the two names collide. Nothing is appended to
force them apart, so the batch is refused rather than recorded:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 20 -base initial.sql -target target.sql
  two migrations get the same id "20260101120000_alter". increase -max-migration-id-length
  [1]

Below the timestamp width no name is left at all, caught by the same check:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 15 -base initial.sql -target target.sql
  two migrations get the same id "20260101120000". increase -max-migration-id-length
  [1]

Shortening is deterministic, so -migrate stays idempotent:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260101120000 -max-migration-id-length 100 -initial initial.sql -migrations-file migrations.sql -target target.sql > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20260101120000_alter_blog_content_sync_jobs_full_add_col_redirects_synced_add_col_redirects_skipped, 20260101120000_alter_blog_content_sync_jobs_incremental_add_col_redirects_synced_add_col)

  $ grep -o 'id=.*' migrations.sql
  id=20260101120000_alter_blog_content_sync_jobs_full_add_col_redirects_synced_add_col_redirects_skipped
  id=20260101120000_alter_blog_content_sync_jobs_incremental_add_col_redirects_synced_add_col

Generated code names follow the id:

  $ grep -o 'let apply_[a-z0-9_]*' migrations.ml
  let apply_20260101120000_alter_blog_content_sync_jobs_full_add_col_redirects_synced_add_col_redirects_skipped
  let apply_20260101120000_alter_blog_content_sync_jobs_incremental_add_col_redirects_synced_add_col

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260102120000 -max-migration-id-length 100 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)

Recorded ids are never rewritten, so changing the cap later leaves applied
migrations untouched:

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260102120000 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)
