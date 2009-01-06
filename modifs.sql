-- Isolated forum-related tables
ALTER TABLE messages RENAME TO forums_messages;
ALTER TABLE textdata RENAME TO forums_textdata;
ALTER TABLE threads RENAME TO forums_threads;


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

-- All wikis have a container page
ALTER TABLE wikis ALTER COLUMN container_id SET NOT NULL;


-- Names must be unique among wikis (since we use names essentially as primary keys)
ALTER TABLE wikis ADD UNIQUE (title);


-- Index of all wikiboxes plus missing consistency checks
ALTER TABLE wikipages ADD FOREIGN KEY (sourcewiki) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;

CREATE TABLE wikiboxindex
(
  wiki_id integer NOT NULL, -- Wiki to which the wikibox belongs
  id integer NOT NULL, -- Index of the wikibox in its wiki
  "comment" text,
  CONSTRAINT wikiboxesindex_pkey PRIMARY KEY (wiki_id, id)
)
WITH (OIDS=FALSE);
ALTER TABLE wikiboxindex OWNER TO ocsimore;
COMMENT ON COLUMN wikiboxindex.wiki_id IS 'Wiki to which the wikibox belongs';
COMMENT ON COLUMN wikiboxindex.id IS 'Index of the wikibox in its wiki';

INSERT INTO wikiboxindex(wiki_id, id)
  SELECT wiki_id, id FROM wikiboxes
  GROUP BY wiki_id, id
  ORDER BY wiki_id, id;

ALTER TABLE wikiboxindex ADD FOREIGN KEY (wiki_id) REFERENCES wikis(id)
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxes ADD FOREIGN KEY (wiki_id, id) REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wikiboxcreators ADD FOREIGN KEY (wiki_id, id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxreaders ADD FOREIGN KEY (wiki_id, id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxrightsgivers ADD FOREIGN KEY (wiki_id, id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxwriters ADD FOREIGN KEY (wiki_id, id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wikipages ADD FOREIGN KEY (destwiki, id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wikis ADD FOREIGN KEY (id, container_id)
      REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE
      DEFERRABLE INITIALLY DEFERRED;
-- Notice that the constraint above *must* be deferred. Otherwise we cannot create a wiki, as we need to create simultaneously the container wikibox

-- Consistency checks on users

ALTER TABLE users ADD UNIQUE (login);

ALTER TABLE wikiboxcreators ADD FOREIGN KEY (creator) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxreaders ADD FOREIGN KEY (reader) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxrightsgivers ADD FOREIGN KEY (wbadmin) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE wikiboxwriters ADD FOREIGN KEY (writer) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;


ALTER TABLE userrights DROP CONSTRAINT userrights_groupid_fkey;
ALTER TABLE userrights DROP CONSTRAINT userrights_id_fkey;
ALTER TABLE userrights ADD FOREIGN KEY (groupid) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE userrights ADD FOREIGN KEY (id) REFERENCES users
      ON DELETE CASCADE ON UPDATE CASCADE;

-- Content-type for wikiboxes
ALTER TABLE wikiboxes ADD COLUMN content_type text NOT NULL DEFAULT 'wiki';


-- TODO wikicss and css