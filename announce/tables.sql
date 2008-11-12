
CREATE SCHEMA announcement;

SET search_path TO announcement;

CREATE TABLE category (
  id SERIAL PRIMARY KEY CHECK (id <> 0),
  name TEXT NOT NULL,
  path TEXT UNIQUE NOT NULL,
  kind INTEGER NOT NULL
);

CREATE TABLE event (
  id SERIAL PRIMARY KEY,
  version INTEGER NOT NULL,
  last_updated TIMESTAMP NOT NULL, -- UTC
  start TIMESTAMP WITH TIME ZONE NOT NULL,
  finish TIMESTAMP WITH TIME ZONE NOT NULL,
  room TEXT NOT NULL,
  location TEXT NOT NULL,
  status INTEGER NOT NULL,
  category INTEGER NOT NULL REFERENCES category,
  title TEXT NOT NULL,
  description INTEGER NOT NULL,
  CHECK(start <= finish)
);

CREATE INDEX start_index ON event (start);
CREATE INDEX last_updated_index ON event (last_updated);

CREATE TABLE person (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  affiliation TEXT NOT NULL
);

CREATE TABLE event_person (
  event INTEGER NOT NULL REFERENCES event,
  person INTEGER NOT NULL REFERENCES person,
  PRIMARY KEY (event, person)
);