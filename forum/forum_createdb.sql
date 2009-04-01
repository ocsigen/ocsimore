CREATE TABLE forums (
    id serial NOT NULL primary key,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    arborescent boolean DEFAULT true NOT NULL,
    deleted boolean DEFAULT false NOT NULL,
    readonly boolean DEFAULT false NOT NULL
);
COMMENT ON COLUMN forums.title IS
  'Title of the forum. Sometimes used to find the forum by name';
COMMENT ON COLUMN forums.descr IS
  'Description of the forum. By default, used as the title of the forum pages';
COMMENT ON COLUMN forums.arborescent IS
  'In non arborescent forums, it is not possible to comment comments';

ALTER TABLE public.forums OWNER TO ocsimore;

CREATE TABLE forums_messages (
    id serial NOT NULL primary key,
    subject text,
    author_id integer NOT NULL REFERENCES users(id),
    datetime timestamp DEFAULT (now())::timestamp NOT NULL,
    parent_id integer REFERENCES forums_messages(id),
    root_id integer NOT NULL REFERENCES forums_messages(id),
    forum_id integer NOT NULL REFERENCES forums(id),
    text text DEFAULT ''::text NOT NULL,
    moderated boolean DEFAULT false NOT NULL,
    deleted boolean DEFAULT false NOT NULL,
    sticky boolean DEFAULT false NOT NULL,
    tree_min integer DEFAULT 1 NOT NULL,
    tree_max integer DEFAULT 2 NOT NULL
);
-- 20090319 WAS: "timestamp without time zone"
COMMENT ON COLUMN forums_messages.text IS
  'Text of the message';
COMMENT ON COLUMN forums_messages.parent_id IS
  'If NULL it is the root of a thread';
COMMENT ON COLUMN forums_messages.root_id IS
  'root of the thread';
COMMENT ON COLUMN forums_messages.moderated IS
  'true if the message has been moderated';
COMMENT ON COLUMN forums_messages.sticky IS
  'Sticky messages will be displayed first';

ALTER TABLE public.forums_messages OWNER TO ocsimore;
