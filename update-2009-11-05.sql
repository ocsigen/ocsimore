ALTER TABLE wikiboxindex DROP COLUMN id;

CREATE TABLE options (
     name text NOT NULL,
     value text NOT NULL,
)

INSERT INTO options VALUES ('dbversion', '1');
