CREATE or REPLACE FUNCTION "update_db_css_wikipage_versioning"() RETURNS void AS
$BODY$
DECLARE
    rec record;
    boxid integer;
    adminuser integer;
  BEGIN
    adminuser := (SELECT id FROM users WHERE login = 'admin');
    FOR rec IN SELECT * FROM css  LOOP
      boxid := (SELECT max(id) FROM wikiboxes WHERE wiki_id = rec.wiki) + 1;
      UPDATE css SET wikibox = boxid WHERE wiki = rec.wiki AND page = rec.page;
--      RAISE NOTICE 'wiki % box %', rec.wiki, boxid;
      INSERT INTO wikiboxindex (wiki_id, id, comment)
        VALUES (rec.wiki, boxid, ('CSS for wikipage ' || rec.page || ', wiki ' ||
         rec.wiki));
      INSERT INTO wikiboxes (id, wiki_id, author, content, content_type)
        VALUES (boxid, rec.wiki, adminuser, rec.css, 'css');
    END LOOP;
  END;
$BODY$
LANGUAGE plpgsql VOLATILE;

BEGIN TRANSACTION;
ALTER TABLE css ADD PRIMARY KEY (wiki, page);
/*ALTER TABLE css DROP CONSTRAINT css_page_key;*/
ALTER TABLE css ADD COLUMN wikibox integer;

SELECT update_db_css_wikipage_versioning();
ALTER TABLE css ADD FOREIGN KEY (wiki, wikibox) REFERENCES wikiboxindex
      ON DELETE CASCADE ON UPDATE CASCADE ;
ALTER TABLE css ALTER COLUMN wikibox SET NOT NULL;
ALTER TABLE css DROP COLUMN css;

DROP FUNCTION "update_db_css_wikipage_versioning"() ;
END;
