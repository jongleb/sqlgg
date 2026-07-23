CREATE TABLE blog_content_sync_jobs_incremental (
  id INT NOT NULL PRIMARY KEY,
  redirects_synced INT NOT NULL,
  redirects_skipped INT NOT NULL,
  redirects_unsupported INT NOT NULL,
  redirects_errors INT NOT NULL
);

CREATE TABLE blog_content_sync_jobs_full (
  id INT NOT NULL PRIMARY KEY,
  redirects_synced INT NOT NULL,
  redirects_skipped INT NOT NULL,
  redirects_unsupported INT NOT NULL,
  redirects_errors INT NOT NULL
);
