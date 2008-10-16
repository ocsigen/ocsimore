-- Isolated forum-related tables
ALTER TABLE messages RENAME TO forums_messages;
ALTER TABLE textdata RENAME TO forums_textdata;
ALTER TABLE threads RENAME TO forums_threads;

-- Missing consistency checks
ALTER TABLE wikiboxes ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wikipages ADD FOREIGN KEY (wiki) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wikiboxcreators ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxreaders ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxrightsgivers ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxwriters ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;

-- Limited form of redirections for wikipages : can now refer to a wikibox of another wiki
ALTER TABLE wikipages ADD COLUMN destwiki integer;
UPDATE wikipages SET destwiki = wiki;
ALTER TABLE wikipages RENAME wiki TO sourcewiki;
ALTER TABLE wikipages ALTER COLUMN destwiki SET NOT NULL;

-- Wikis now 'know' at what url they are located
ALTER TABLE wikis ALTER COLUMN pages DROP NOT NULL;
ALTER TABLE wikis ALTER COLUMN pages TYPE text USING NULL;
ALTER TABLE wikis ALTER COLUMN pages DROP DEFAULT;
COMMENT ON COLUMN wikis.pages IS
  'Root url for this wiki. Set to NULL if the wiki is not linked to an url';
-- For PPS only : populate the new column
UPDATE wikis SET pages = 'intranet' WHERE id = 1;
UPDATE wikis SET pages = '' WHERE id = 2;
UPDATE wikis SET pages = NULL WHERE id = 3;
UPDATE wikis SET pages = 'seminaire' WHERE id = 4;
UPDATE wikis SET pages = 'gdt-semantique' WHERE id = 5;
UPDATE wikis SET pages = 'gdt-concurrence' WHERE id = 6;
UPDATE wikis SET pages = 'gdt-prog' WHERE id = 7;
UPDATE wikis SET pages = 'seminaire-thesards' WHERE id = 8;

-- All wikis have a container page
ALTER TABLE wikis ALTER COLUMN container_id SET NOT NULL;


-- Names must be unique among wikis (since we use names essentially as primary keys)
ALTER TABLE wikis ADD UNIQUE (title);



-- TODO wikicss and css