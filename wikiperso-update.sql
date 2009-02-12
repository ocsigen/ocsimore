-- Update the container pages for the wikipersos, by fetching the last
-- version of the template for the wikis

UPDATE wikiboxes
SET content=
-- We find the latest version of the template, which is contained
-- in the box wikiperso-template of the Admin wiki
  (SELECT content FROM wikiboxes
   WHERE version =
     (SELECT MAX(version) FROM wikiboxes
      WHERE (wiki_id, id) IN
        (SELECT destwiki, id FROM wikipages
         WHERE pagename='wikiperso-template'
         AND sourcewiki = (SELECT id FROM wikis WHERE title = 'Adminwiki'))))
-- And we update the initial container for all the wikipersos.
-- If the author has changed its container, the change will not be visible
WHERE version IN (
  SELECT MIN(version) FROM wikiboxes
  WHERE (wiki_id, id) IN
    (SELECT id, container_id FROM wikis
     WHERE substring(title, 0, 10)='wikiperso')
  GROUP BY id, wiki_id
  ORDER BY wiki_id
)