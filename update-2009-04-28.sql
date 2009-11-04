BEGIN TRANSACTION;

/*
DROP TABLE wikiboxcreators;
DROP TABLE wikiboxreaders;
DROP TABLE wikiboxrightsgivers;
DROP TABLE wikiboxwriters;
*/

ALTER TABLE userrights DROP CONSTRAINT uni;
ALTER TABLE userrights ADD UNIQUE (id, idarg, groupid, groupidarg);
COMMIT;