CREATE or REPLACE FUNCTION "update_db_css_wiki_versioning"() RETURNS void AS
$BODY$
DECLARE
    rec record;
    boxid integer;
    adminuser integer;
  BEGIN
    adminuser := (SELECT id FROM users WHERE login = 'admin');
    FOR rec IN SELECT * FROM wikicss  LOOP
      boxid := (SELECT max(id) FROM wikiboxes WHERE wiki_id = rec.wiki) + 1;
--      RAISE NOTICE 'wiki % box %', rec.wiki, boxid;
      INSERT INTO wikiboxindex (wiki_id, id, comment)
        VALUES (rec.wiki, boxid, ('CSS for wiki ' || rec.wiki));
      INSERT INTO wikiboxes (id, wiki_id, author, content, content_type)
        VALUES (boxid, rec.wiki, adminuser, rec.css, 'css');
      INSERT INTO css (wiki, page, wikibox) VALUES (rec.wiki, NULL, boxid);
    END LOOP;
  END;
$BODY$
LANGUAGE plpgsql VOLATILE;

BEGIN TRANSACTION;
ALTER TABLE css DROP CONSTRAINT css_pkey;
ALTER TABLE css ALTER COLUMN PAGE DROP NOT NULL;
COMMENT ON COLUMN css.page IS 'this field is NULL if the CSS is for the wiki,
and PATH if it is for the wikipage PATH of the wiki';

SELECT update_db_css_wiki_versioning();

DROP TABLE wikicss;
DROP FUNCTION "update_db_css_wiki_versioning"() ;
END;
