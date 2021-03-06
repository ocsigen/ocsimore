--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: announcement; Type: SCHEMA; Schema: -; Owner: ocsimore
--

CREATE SCHEMA announcement;


ALTER SCHEMA announcement OWNER TO ocsimore;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: postgres
--

CREATE OR REPLACE PROCEDURAL LANGUAGE plpgsql;


ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO postgres;

SET search_path = public, pg_catalog;

--
-- Name: wbuid(integer, integer); Type: FUNCTION; Schema: public; Owner: ocsimore
--

CREATE FUNCTION wbuid(integer, integer) RETURNS integer
    LANGUAGE sql
    AS $_$
  SELECT uid FROM wikiboxindex WHERE wiki_id=$1 AND id=$2;
$_$;


ALTER FUNCTION public.wbuid(integer, integer) OWNER TO ocsimore;

SET search_path = announcement, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: category; Type: TABLE; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE TABLE category (
    id integer NOT NULL,
    name text NOT NULL,
    path text NOT NULL,
    description integer NOT NULL,
    editable boolean NOT NULL,
    "time" time without time zone,
    duration integer NOT NULL,
    room text NOT NULL,
    location text NOT NULL
);


ALTER TABLE announcement.category OWNER TO ocsimore;

--
-- Name: category_id_seq; Type: SEQUENCE; Schema: announcement; Owner: ocsimore
--

CREATE SEQUENCE category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE announcement.category_id_seq OWNER TO ocsimore;

--
-- Name: category_id_seq; Type: SEQUENCE OWNED BY; Schema: announcement; Owner: ocsimore
--

ALTER SEQUENCE category_id_seq OWNED BY category.id;


--
-- Name: category_id_seq; Type: SEQUENCE SET; Schema: announcement; Owner: ocsimore
--

SELECT pg_catalog.setval('category_id_seq', 1, false);


--
-- Name: event; Type: TABLE; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE TABLE event (
    id integer NOT NULL,
    minor_version integer NOT NULL,
    major_version integer NOT NULL,
    last_updated timestamp without time zone NOT NULL,
    start timestamp with time zone NOT NULL,
    finish timestamp with time zone NOT NULL,
    room text NOT NULL,
    location text NOT NULL,
    status integer NOT NULL,
    category integer NOT NULL,
    title text NOT NULL,
    description integer NOT NULL,
    CONSTRAINT event_check CHECK ((start <= finish))
);


ALTER TABLE announcement.event OWNER TO ocsimore;

--
-- Name: event_id_seq; Type: SEQUENCE; Schema: announcement; Owner: ocsimore
--

CREATE SEQUENCE event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE announcement.event_id_seq OWNER TO ocsimore;

--
-- Name: event_id_seq; Type: SEQUENCE OWNED BY; Schema: announcement; Owner: ocsimore
--

ALTER SEQUENCE event_id_seq OWNED BY event.id;


--
-- Name: event_id_seq; Type: SEQUENCE SET; Schema: announcement; Owner: ocsimore
--

SELECT pg_catalog.setval('event_id_seq', 1, false);


--
-- Name: event_person; Type: TABLE; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE TABLE event_person (
    event integer NOT NULL,
    person integer NOT NULL
);


ALTER TABLE announcement.event_person OWNER TO ocsimore;

--
-- Name: person; Type: TABLE; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE TABLE person (
    id integer NOT NULL,
    name text NOT NULL,
    affiliation text NOT NULL
);


ALTER TABLE announcement.person OWNER TO ocsimore;

--
-- Name: person_id_seq; Type: SEQUENCE; Schema: announcement; Owner: ocsimore
--

CREATE SEQUENCE person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE announcement.person_id_seq OWNER TO ocsimore;

--
-- Name: person_id_seq; Type: SEQUENCE OWNED BY; Schema: announcement; Owner: ocsimore
--

ALTER SEQUENCE person_id_seq OWNED BY person.id;


--
-- Name: person_id_seq; Type: SEQUENCE SET; Schema: announcement; Owner: ocsimore
--

SELECT pg_catalog.setval('person_id_seq', 1, false);


SET search_path = public, pg_catalog;

--
-- Users settings
--

CREATE TABLE users_settings (
  id integer NOT NULL,
  basicusercreation boolean NOT NULL,
  registration_mail_from text NOT NULL,
  registration_mail_addr text NOT NULL,
  registration_mail_subject text NOT NULL,
  groups text NOT NULL,
  non_admin_can_create boolean NOT NULL
);

ALTER TABLE public.users_settings OWNER TO ocsimore;

--
-- Name: css; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE css (
    wiki integer NOT NULL,
    page text,
    wikibox integer NOT NULL,
    specialrights boolean DEFAULT false NOT NULL,
    uid integer NOT NULL,
    rank integer DEFAULT 1 NOT NULL,
    mediatype text DEFAULT 'all'::text NOT NULL
);


ALTER TABLE public.css OWNER TO ocsimore;

--
-- Name: COLUMN css.page; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN css.page IS 'this field is NULL if the CSS is for the wiki,
and PATH if it is for the wikipage PATH of the wiki';


--
-- Name: css_uid_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE css_uid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.css_uid_seq OWNER TO ocsimore;

--
-- Name: css_uid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE css_uid_seq OWNED BY css.uid;


--
-- Name: css_uid_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('css_uid_seq', 1, false);


--
-- Name: forums; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE forums (
    id integer NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    arborescent boolean DEFAULT true NOT NULL,
    deleted boolean DEFAULT false NOT NULL,
    title_syntax text NOT NULL,
    messages_wiki integer NOT NULL,
    comments_wiki integer NOT NULL
);


ALTER TABLE public.forums OWNER TO ocsimore;

--
-- Name: COLUMN forums.title; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.title IS 'Title of the forum. Sometimes used to find the forum by name';


--
-- Name: COLUMN forums.descr; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.descr IS 'Description of the forum. By default, used as the title of the forum pages';


--
-- Name: COLUMN forums.arborescent; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.arborescent IS 'In non arborescent forums, it is not possible to comment comments';


--
-- Name: COLUMN forums.title_syntax; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.title_syntax IS 'The syntax of message titles';


--
-- Name: COLUMN forums.messages_wiki; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.messages_wiki IS 'Wiki containing the messages';


--
-- Name: COLUMN forums.comments_wiki; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums.comments_wiki IS 'Wiki containing the comments';


--
-- Name: forums_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE forums_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.forums_id_seq OWNER TO ocsimore;

--
-- Name: forums_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE forums_id_seq OWNED BY forums.id;


--
-- Name: forums_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('forums_id_seq', 1, false);


--
-- Name: forums_messages; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE forums_messages (
    id integer NOT NULL,
    creator_id integer NOT NULL,
    datetime timestamp without time zone DEFAULT (now())::timestamp without time zone NOT NULL,
    parent_id integer,
    root_id integer NOT NULL,
    forum_id integer NOT NULL,
    subject integer,
    wikibox integer NOT NULL,
    moderated boolean DEFAULT false NOT NULL,
    special_rights boolean DEFAULT false NOT NULL,
    tree_min integer DEFAULT 1 NOT NULL,
    tree_max integer DEFAULT 2 NOT NULL
);


ALTER TABLE public.forums_messages OWNER TO ocsimore;

--
-- Name: COLUMN forums_messages.parent_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums_messages.parent_id IS 'If NULL it is the root of a thread';


--
-- Name: COLUMN forums_messages.root_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums_messages.root_id IS 'root of the thread';


--
-- Name: COLUMN forums_messages.wikibox; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums_messages.wikibox IS 'id of the wiki box in the wiki';


--
-- Name: COLUMN forums_messages.moderated; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums_messages.moderated IS 'true if the message has been moderated';


--
-- Name: COLUMN forums_messages.special_rights; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN forums_messages.special_rights IS 'means that it is the root of a thread with special rights. It has its own groups (will not look at forum groups)';


--
-- Name: forums_messages_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE forums_messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.forums_messages_id_seq OWNER TO ocsimore;

--
-- Name: forums_messages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE forums_messages_id_seq OWNED BY forums_messages.id;


--
-- Name: forums_messages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('forums_messages_id_seq', 1, false);


--
-- Name: options; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE options (
    name text NOT NULL,
    value text NOT NULL
);


ALTER TABLE public.options OWNER TO ocsimore;

--
-- Name: userrights; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE userrights (
    id integer NOT NULL,
    groupid integer NOT NULL,
    idarg integer,
    groupidarg integer
);


ALTER TABLE public.userrights OWNER TO ocsimore;

--
-- Name: users; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    login text NOT NULL,
    password text,
    fullname text NOT NULL,
    email text,
    dyn boolean NOT NULL,
    authtype character(1) NOT NULL
);


ALTER TABLE public.users OWNER TO ocsimore;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO ocsimore;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('users_id_seq', 1, false);


--
-- Name: wikiboxescontent; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE wikiboxescontent (
    version integer NOT NULL,
    comment text DEFAULT ''::text NOT NULL,
    author integer NOT NULL,
    content text,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    content_type text DEFAULT 'wikicreole'::text NOT NULL,
    wikibox integer NOT NULL,
    ip text
);


ALTER TABLE public.wikiboxescontent OWNER TO ocsimore;

--
-- Name: wikiboxes_version_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE wikiboxes_version_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wikiboxes_version_seq OWNER TO ocsimore;

--
-- Name: wikiboxes_version_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE wikiboxes_version_seq OWNED BY wikiboxescontent.version;


--
-- Name: wikiboxes_version_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('wikiboxes_version_seq', 1, false);


--
-- Name: wikiboxindex; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE wikiboxindex (
    wiki integer NOT NULL,
    comment text,
    specialrights boolean DEFAULT false NOT NULL,
    uid integer NOT NULL
);


ALTER TABLE public.wikiboxindex OWNER TO ocsimore;

--
-- Name: COLUMN wikiboxindex.wiki; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikiboxindex.wiki IS 'Wiki to which the wikibox belongs';


--
-- Name: wikiboxindex_uid_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE wikiboxindex_uid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wikiboxindex_uid_seq OWNER TO ocsimore;

--
-- Name: wikiboxindex_uid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE wikiboxindex_uid_seq OWNED BY wikiboxindex.uid;


--
-- Name: wikiboxindex_uid_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('wikiboxindex_uid_seq', 1, false);


--
-- Name: wikipages; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE wikipages (
    wiki integer NOT NULL,
    wikibox integer NOT NULL,
    pagename text DEFAULT ''::text NOT NULL,
    title text,
    uid integer NOT NULL
);


ALTER TABLE public.wikipages OWNER TO ocsimore;

--
-- Name: wikipages_uid_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE wikipages_uid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wikipages_uid_seq OWNER TO ocsimore;

--
-- Name: wikipages_uid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE wikipages_uid_seq OWNED BY wikipages.uid;


--
-- Name: wikipages_uid_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('wikipages_uid_seq', 1, false);


--
-- Name: wikis; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE wikis (
    id integer NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    pages text,
    boxrights boolean NOT NULL,
    container integer,
    staticdir text,
    model text DEFAULT 'wikicreole'::text NOT NULL,
    siteid text,
    deleted boolean NOT NULL DEFAULT(false)
);


ALTER TABLE public.wikis OWNER TO ocsimore;

--
-- Name: COLUMN wikis.title; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikis.title IS 'Title of the wiki. Sometimes used to find the wiki by name';


--
-- Name: COLUMN wikis.descr; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikis.descr IS 'Description of the wiki. By default, used as the title of the wiki pages';


--
-- Name: COLUMN wikis.pages; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikis.pages IS 'Root url for this wiki. Set to NULL if the wiki is not linked to an url';


--
-- Name: COLUMN wikis.boxrights; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN wikis.boxrights IS 'True if each wikibox of the wiki has its own rights';


--
-- Name: wikis_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsimore
--

CREATE SEQUENCE wikis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wikis_id_seq OWNER TO ocsimore;

--
-- Name: wikis_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsimore
--

ALTER SEQUENCE wikis_id_seq OWNED BY wikis.id;


--
-- Name: wikis_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsimore
--

SELECT pg_catalog.setval('wikis_id_seq', 1, false);


SET search_path = announcement, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE category ALTER COLUMN id SET DEFAULT nextval('category_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE event ALTER COLUMN id SET DEFAULT nextval('event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE person ALTER COLUMN id SET DEFAULT nextval('person_id_seq'::regclass);


SET search_path = public, pg_catalog;

--
-- Name: uid; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE css ALTER COLUMN uid SET DEFAULT nextval('css_uid_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE forums ALTER COLUMN id SET DEFAULT nextval('forums_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE forums_messages ALTER COLUMN id SET DEFAULT nextval('forums_messages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Name: version; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE wikiboxescontent ALTER COLUMN version SET DEFAULT nextval('wikiboxes_version_seq'::regclass);


--
-- Name: uid; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE wikiboxindex ALTER COLUMN uid SET DEFAULT nextval('wikiboxindex_uid_seq'::regclass);


--
-- Name: uid; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE wikipages ALTER COLUMN uid SET DEFAULT nextval('wikipages_uid_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsimore
--

ALTER TABLE wikis ALTER COLUMN id SET DEFAULT nextval('wikis_id_seq'::regclass);


SET search_path = announcement, pg_catalog;


--
-- Data for Name: category; Type: TABLE DATA; Schema: announcement; Owner: ocsimore
--

COPY category (id, name, path, description, editable, "time", duration, room, location) FROM stdin;
\.


--
-- Data for Name: event; Type: TABLE DATA; Schema: announcement; Owner: ocsimore
--

COPY event (id, minor_version, major_version, last_updated, start, finish, room, location, status, category, title, description) FROM stdin;
\.


--
-- Data for Name: event_person; Type: TABLE DATA; Schema: announcement; Owner: ocsimore
--

COPY event_person (event, person) FROM stdin;
\.


--
-- Data for Name: person; Type: TABLE DATA; Schema: announcement; Owner: ocsimore
--

COPY person (id, name, affiliation) FROM stdin;
\.


SET search_path = public, pg_catalog;

--
-- Data for Name: css; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY css (wiki, page, wikibox, specialrights, uid, rank, mediatype) FROM stdin;
\.


--
-- Data for Name: forums; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY forums (id, title, descr, arborescent, deleted, title_syntax, messages_wiki, comments_wiki) FROM stdin;
\.


--
-- Data for Name: forums_messages; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY forums_messages (id, creator_id, datetime, parent_id, root_id, forum_id, subject, wikibox, moderated, special_rights, tree_min, tree_max) FROM stdin;
\.


--
-- Data for Name: options; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY options (name, value) FROM stdin;
dbversion	7
\.


--
-- Data for Name: userrights; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY userrights (id, groupid, idarg, groupidarg) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY users (id, login, password, fullname, email, dyn, authtype) FROM stdin;
\.


--
-- Data for Name: wikiboxescontent; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY wikiboxescontent (version, comment, author, content, datetime, content_type, wikibox, ip) FROM stdin;
\.


--
-- Data for Name: wikiboxindex; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY wikiboxindex (wiki, comment, specialrights, uid) FROM stdin;
\.


--
-- Data for Name: wikipages; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY wikipages (wiki, wikibox, pagename, title, uid) FROM stdin;
\.


--
-- Data for Name: wikis; Type: TABLE DATA; Schema: public; Owner: ocsimore
--

COPY wikis (id, title, descr, pages, boxrights, container, staticdir, model, siteid) FROM stdin;
\.


SET search_path = announcement, pg_catalog;

--
-- Name: category_path_key; Type: CONSTRAINT; Schema: announcement; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY category
    ADD CONSTRAINT category_path_key UNIQUE (path);


--
-- Name: category_pkey; Type: CONSTRAINT; Schema: announcement; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);


--
-- Name: event_person_pkey; Type: CONSTRAINT; Schema: announcement; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY event_person
    ADD CONSTRAINT event_person_pkey PRIMARY KEY (event, person);


--
-- Name: event_pkey; Type: CONSTRAINT; Schema: announcement; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);


--
-- Name: person_pkey; Type: CONSTRAINT; Schema: announcement; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);


SET search_path = public, pg_catalog;

--
-- Name: css_page_key; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY css
    ADD CONSTRAINT css_page_key UNIQUE (wiki, page, rank);


--
-- Name: forums_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY forums_messages
    ADD CONSTRAINT forums_messages_pkey PRIMARY KEY (id);


--
-- Name: forums_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_pkey PRIMARY KEY (id);


--
-- Name: options_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY options
    ADD CONSTRAINT options_pkey PRIMARY KEY (name);


--
-- Name: userrights_id_key; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY userrights
    ADD CONSTRAINT userrights_id_key UNIQUE (id, idarg, groupid, groupidarg);


--
-- Name: users_login_key; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_login_key UNIQUE (login);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: wikiboxescontent_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY wikiboxescontent
    ADD CONSTRAINT wikiboxescontent_pkey PRIMARY KEY (wikibox, version);


--
-- Name: wikiboxindex_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY wikiboxindex
    ADD CONSTRAINT wikiboxindex_pkey PRIMARY KEY (uid);


--
-- Name: wikipages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_pkey PRIMARY KEY (wiki, pagename);


--
-- Name: wikis_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY wikis
    ADD CONSTRAINT wikis_pkey PRIMARY KEY (id);


--
-- Name: wikis_title_unique; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
--

ALTER TABLE ONLY wikis
    ADD CONSTRAINT wikis_title_unique UNIQUE (title, siteid);


SET search_path = announcement, pg_catalog;

--
-- Name: last_updated_index; Type: INDEX; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE INDEX last_updated_index ON event USING btree (last_updated);


--
-- Name: start_index; Type: INDEX; Schema: announcement; Owner: ocsimore; Tablespace: 
--

CREATE INDEX start_index ON event USING btree (start);


--
-- Name: event_category_fkey; Type: FK CONSTRAINT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE ONLY event
    ADD CONSTRAINT event_category_fkey FOREIGN KEY (category) REFERENCES category(id);


--
-- Name: event_person_event_fkey; Type: FK CONSTRAINT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE ONLY event_person
    ADD CONSTRAINT event_person_event_fkey FOREIGN KEY (event) REFERENCES event(id);


--
-- Name: event_person_person_fkey; Type: FK CONSTRAINT; Schema: announcement; Owner: ocsimore
--

ALTER TABLE ONLY event_person
    ADD CONSTRAINT event_person_person_fkey FOREIGN KEY (person) REFERENCES person(id);


SET search_path = public, pg_catalog;

--
-- Name: css_wikibox_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY css
    ADD CONSTRAINT css_wikibox_fkey FOREIGN KEY (wikibox) REFERENCES wikiboxindex(uid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: forums_messages_creator_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums_messages
    ADD CONSTRAINT forums_messages_creator_id_fkey FOREIGN KEY (creator_id) REFERENCES users(id);


--
-- Name: forums_messages_forum_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums_messages
    ADD CONSTRAINT forums_messages_forum_id_fkey FOREIGN KEY (forum_id) REFERENCES forums(id);


--
-- Name: forums_messages_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums_messages
    ADD CONSTRAINT forums_messages_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES forums_messages(id);


--
-- Name: forums_messages_root_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums_messages
    ADD CONSTRAINT forums_messages_root_id_fkey FOREIGN KEY (root_id) REFERENCES forums_messages(id);


--
-- Name: userrights_groupid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY userrights
    ADD CONSTRAINT userrights_groupid_fkey FOREIGN KEY (groupid) REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: userrights_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY userrights
    ADD CONSTRAINT userrights_id_fkey FOREIGN KEY (id) REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikiboxescontent_wikibox_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikiboxescontent
    ADD CONSTRAINT wikiboxescontent_wikibox_fkey FOREIGN KEY (wikibox) REFERENCES wikiboxindex(uid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikiboxindex_wiki_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikiboxindex
    ADD CONSTRAINT wikiboxindex_wiki_id_fkey FOREIGN KEY (wiki) REFERENCES wikis(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikipages_sourcewiki_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_sourcewiki_fkey FOREIGN KEY (wiki) REFERENCES wikis(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikipages_wiki_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wiki_fkey FOREIGN KEY (wiki) REFERENCES wikis(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikipages_wikibox_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wikibox_fkey FOREIGN KEY (wikibox) REFERENCES wikiboxindex(uid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wikis_container_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY wikis
    ADD CONSTRAINT wikis_container_fkey FOREIGN KEY (container) REFERENCES wikiboxindex(uid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

