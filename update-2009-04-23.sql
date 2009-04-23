BEGIN TRANSACTION;

ALTER TABLE wikiboxindex ADD COLUMN specialrights bool NOT NULL DEFAULT false;
ALTER TABLE css ADD COLUMN specialrights bool NOT NULL DEFAULT false;

ALTER TABLE userrights ADD COLUMN idarg int;
ALTER TABLE userrights ADD COLUMN groupidarg int;

ALTER TABLE wikiboxindex ADD COLUMN uid serial NOT NULL;
ALTER TABLE wikipages ADD COLUMN uid serial NOT NULL;
ALTER TABLE css ADD COLUMN uid serial NOT NULL;

COMMIT;