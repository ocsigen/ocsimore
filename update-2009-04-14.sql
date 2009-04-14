BEGIN

ALTER TABLE wikiboxes ALTER COLUMN content DROP NOT NULL;
UPDATE wikiboxes SET CONTENT = NULL, content_type = 'wiki'
  WHERE content_type = 'deleted';

COMMIT