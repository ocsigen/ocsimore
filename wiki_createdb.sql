CREATE TABLE wikis (
    id serial NOT NULL primary key,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    pages boolean NOT NULL,
    boxrights boolean NOT NULL,
    container_id integer,
    staticdir text
);
COMMENT ON COLUMN wikis.title IS
  'Title of the wiki. Sometimes used to find the wiki by name';
COMMENT ON COLUMN wikis.descr IS
  'Description of the wiki. By default, used as the title of the wiki pages';
COMMENT ON COLUMN wikis.pages IS
  'Set to True if this wiki is associated to an URL';
COMMENT ON COLUMN wikis.boxrights IS
  'True if each wikibox of the wiki has its own rights';


ALTER TABLE public.wikis OWNER TO ocsimore;


CREATE TABLE wikiboxes (
    id integer NOT NULL,
    wiki_id integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    version serial NOT NULL,
    comment text DEFAULT ''::text NOT NULL,
    author integer NOT NULL,
    content text NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    PRIMARY KEY (wiki_id, id, version)
);

ALTER TABLE public.wikiboxes OWNER TO ocsimore;


CREATE TABLE wikipages (
    wiki integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    id integer NOT NULL, --  REFERENCES wikiboxes(id)
    pagename text DEFAULT ''::text NOT NULL
);

ALTER TABLE public.wikipages OWNER TO ocsimore;


-- css for whole wiki:
CREATE TABLE wikicss (
    wiki integer NOT NULL, --  REFERENCES wikis(id)
    css text DEFAULT ''::text NOT NULL
);

ALTER TABLE public.wikicss OWNER TO ocsimore;


-- css for each page
CREATE TABLE css (
    wiki integer NOT NULL, --  REFERENCES wikis(id)
    page text NOT NULL UNIQUE,
    css text DEFAULT ''::text NOT NULL
);

ALTER TABLE public.css OWNER TO ocsimore;

--
-- Name: COLUMN wikiboxes.wik_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikiboxes.wiki_id IS 'wiki';

--
-- Name: wikis; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--



--
-- Data for Name: wikipages; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

-- COPY wikipages (id, suffix, wik_id, txt_id, author, datetime, subject) FROM stdin;
-- \.


--
-- Data for Name: wikis; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

-- COPY wikis (id, title, descr) FROM stdin;
-- \.

CREATE TABLE wikiboxreaders (
    wiki_id integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    id integer NOT NULL,
    reader integer NOT NULL,
    CONSTRAINT uni_r UNIQUE (wiki_id, id, reader)
);

CREATE TABLE wikiboxwriters (
    wiki_id integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    id integer NOT NULL,
    writer integer NOT NULL,
    CONSTRAINT uni_w UNIQUE (wiki_id, id, writer)
);

CREATE TABLE wikiboxrightsgivers (
    wiki_id integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    id integer NOT NULL,
    wbadmin integer NOT NULL,
    CONSTRAINT uni_a UNIQUE (wiki_id, id, wbadmin)
);

CREATE TABLE wikiboxcreators (
    wiki_id integer
      REFERENCES wikis(id)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
    id integer NOT NULL,
    creator integer NOT NULL,
    CONSTRAINT uni_c UNIQUE (wiki_id, id, creator)
);

