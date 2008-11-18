
- Paramètres pour Postgres: PGOPTS dans le Makefile
  (peut-être modifier aussi l'appel à PGOCaml.connect dans common_sql.ml)
- Créer la base de donnée
        make make-db
- Remplir la base de donnée
        make load
- Compiler
        make

=====

- Supprimer la base de donnée
        make drop-db
