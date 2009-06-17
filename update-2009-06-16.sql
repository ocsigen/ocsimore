CREATE OR REPLACE FUNCTION wbuid(integer,integer) RETURNS integer AS
$BODY$
  SELECT uid FROM wikiboxindex WHERE wiki_id=$1 AND id=$2;
$BODY$ LANGUAGE SQL;


ALTER TABLE wikiboxindex DROP CONSTRAINt wikiboxesindex_pkey CASCADE;
ALTER TABLE wikiboxindex ADD PRIMARY KEY (uid);


/* Update of CSS table. We change the content of the wikibox column */
UPDATE css SET wikibox = wbuid(wiki,wikibox);
ALTER TABLE css ADD FOREIGN KEY (wikibox) REFERENCES wikiboxindex(uid)
      ON UPDATE CASCADE ON DELETE CASCADE;


/* Update of the wikiboxes tables (which we rename to wikiboxescontent).
   We add a column wikibox, and remove the columns wiki_id and id */
ALTER TABLE wikiboxes ADD COLUMN wikibox integer;
UPDATE wikiboxes SET wikibox = wbuid(wiki_id,id);
ALTER TABLE wikiboxes ALTER COLUMN wikibox SET NOT NULL;
ALTER TABLE wikiboxes RENAME TO wikiboxescontent;
ALTER TABLE wikiboxescontent DROP COLUMN id;
ALTER TABLE wikiboxescontent DROP COLUMN wiki_id;
ALTER TABLE wikiboxescontent ADD PRIMARY KEY (wikibox, version);
ALTER TABLE wikiboxescontent ADD FOREIGN KEY (wikibox)
      REFERENCES wikiboxindex(uid)
      ON UPDATE CASCADE ON DELETE CASCADE;

/* Update of the wikipages table. We rename the column sourcewiki to
   wiki, rename the column id to wikibox, update its content,
   and remove the column destwiki */
ALTER TABLE wikipages RENAME COLUMN id TO wikibox;
UPDATE wikipages SET wikibox = wbuid(destwiki,wikibox);
ALTER TABLE wikipages DROP COLUMN destwiki;
ALTER TABLE wikipages RENAME COLUMN sourcewiki TO wiki;
ALTER TABLE wikipages ADD PRIMARY KEY (wiki,pagename);
ALTER TABLE wikipages ADD FOREIGN KEY (wikibox) REFERENCES wikiboxindex(uid)
      ON UPDATE CASCADE ON DELETE CASCADE;

/* Update of the wikis table. We update the column container_id */
ALTER TABLE wikis RENAME COLUMN container_id TO container;
UPDATE wikis SET container = wbuid(id,container);
ALTER TABLE wikis ADD FOREIGN KEY (container) REFERENCES wikiboxindex(uid)
      ON UPDATE CASCADE ON DELETE CASCADE;

/* We can now remove the column of wikiboxindex entirely */
/* XXX ALTER TABLE wikiboxindex DROP COLUMN id; */
ALTER TABLE wikiboxindex ALTER COLUMN id DROP NOT NULL;
ALTER TABLE wikiboxindex RENAME COLUMN wiki_id TO wiki;

/* Containers are not mandatory */
ALTER TABLE wikis ALTER COLUMN container DROP NOT NULL;