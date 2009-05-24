ALTER TABLE wikiboxes ALTER COLUMN content_type SET DEFAULT 'wikicreole';
UPDATE wikiboxes SET content_type = 'wikicreole' WHERE content_type = 'wiki';
ALTER TABLE wikis ADD COLUMN model text NOT NULL DEFAULT 'wikicreole';
