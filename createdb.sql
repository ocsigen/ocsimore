--
-- PostgreSQL database dump
--

SET client_encoding = 'LATIN9';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: forums; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE forums (
    id integer NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    moderated boolean DEFAULT false NOT NULL,
    arborescent boolean NOT NULL,
    reader integer NOT NULL,
    writer integer NOT NULL,
    moderator integer NOT NULL
);


ALTER TABLE public.forums OWNER TO ocsigen;

--
-- Name: globalstore; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE globalstore (
    key text NOT NULL,
    value bytea
);


ALTER TABLE public.globalstore OWNER TO ocsigen;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE messages (
    id integer NOT NULL,
    datetime timestamp without time zone DEFAULT (now())::timestamp without time zone NOT NULL,
    thr_id integer NOT NULL,
    txt_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    parent_id integer DEFAULT 0 NOT NULL,
    sticky boolean DEFAULT false NOT NULL,
    tree_min integer DEFAULT 0 NOT NULL,
    tree_max integer DEFAULT 0 NOT NULL,
    author_id integer NOT NULL
);


ALTER TABLE public.messages OWNER TO ocsigen;

--
-- Name: COLUMN messages.thr_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.thr_id IS 'thread';


--
-- Name: COLUMN messages.txt_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.txt_id IS 'text';


--
-- Name: COLUMN messages.hidden; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.hidden IS 'for moderated forums';


--
-- Name: service_parameters; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE service_parameters (
    id integer NOT NULL,
    service_id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE public.service_parameters OWNER TO ocsigen;

--
-- Name: services; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE services (
    id integer NOT NULL,
    url text NOT NULL
);


ALTER TABLE public.services OWNER TO ocsigen;

--
-- Name: tablefunc_crosstab_2; Type: TYPE; Schema: public; Owner: ocsigen
--

CREATE TYPE tablefunc_crosstab_2 AS (
	row_name text,
	category_1 text,
	category_2 text
);


ALTER TYPE public.tablefunc_crosstab_2 OWNER TO ocsigen;

--
-- Name: tablefunc_crosstab_3; Type: TYPE; Schema: public; Owner: ocsigen
--

CREATE TYPE tablefunc_crosstab_3 AS (
	row_name text,
	category_1 text,
	category_2 text,
	category_3 text
);


ALTER TYPE public.tablefunc_crosstab_3 OWNER TO ocsigen;

--
-- Name: tablefunc_crosstab_4; Type: TYPE; Schema: public; Owner: ocsigen
--

CREATE TYPE tablefunc_crosstab_4 AS (
	row_name text,
	category_1 text,
	category_2 text,
	category_3 text,
	category_4 text
);


ALTER TYPE public.tablefunc_crosstab_4 OWNER TO ocsigen;

--
-- Name: textdata; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE textdata (
    id integer NOT NULL,
    txt text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.textdata OWNER TO ocsigen;

--
-- Name: threads; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE threads (
    id integer NOT NULL,
    subject text DEFAULT ''::text NOT NULL,
    frm_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    article_id integer,
    author_id integer NOT NULL
);


ALTER TABLE public.threads OWNER TO ocsigen;

--
-- Name: COLUMN threads.frm_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN threads.frm_id IS 'forum';


--
-- Name: users; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    login text NOT NULL,
    password text,
    fullname text NOT NULL,
    email text NOT NULL,
    permissions bytea
);


ALTER TABLE public.users OWNER TO ocsigen;

--
-- Name: wikipages; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE wikipages (
    id integer NOT NULL,
    suffix text DEFAULT ''::text NOT NULL,
    wik_id integer NOT NULL,
    txt_id integer NOT NULL,
    author text DEFAULT ''::text NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    subject text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.wikipages OWNER TO ocsigen;

--
-- Name: COLUMN wikipages.wik_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN wikipages.wik_id IS 'wiki';


--
-- Name: COLUMN wikipages.txt_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN wikipages.txt_id IS 'text';


--
-- Name: wikis; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE wikis (
    id integer NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.wikis OWNER TO ocsigen;

--
-- Name: forums_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE forums_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forums_id_seq OWNER TO ocsigen;

--
-- Name: forums_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE forums_id_seq OWNED BY forums.id;


--
-- Name: forums_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('forums_id_seq', 1, false);


--
-- Name: messages_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.messages_id_seq OWNER TO ocsigen;

--
-- Name: messages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE messages_id_seq OWNED BY messages.id;


--
-- Name: messages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('messages_id_seq', 1, false);


--
-- Name: service_parameters_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE service_parameters_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.service_parameters_id_seq OWNER TO ocsigen;

--
-- Name: service_parameters_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE service_parameters_id_seq OWNED BY service_parameters.id;


--
-- Name: service_parameters_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('service_parameters_id_seq', 1, false);


--
-- Name: services_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE services_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.services_id_seq OWNER TO ocsigen;

--
-- Name: services_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE services_id_seq OWNED BY services.id;


--
-- Name: services_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('services_id_seq', 1, false);


--
-- Name: textdata_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE textdata_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.textdata_id_seq OWNER TO ocsigen;

--
-- Name: textdata_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE textdata_id_seq OWNED BY textdata.id;


--
-- Name: textdata_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('textdata_id_seq', 1, false);


--
-- Name: threads_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE threads_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.threads_id_seq OWNER TO ocsigen;

--
-- Name: threads_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE threads_id_seq OWNED BY threads.id;


--
-- Name: threads_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('threads_id_seq', 1, false);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO ocsigen;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('users_id_seq', 1, false);


--
-- Name: wikipages_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE wikipages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.wikipages_id_seq OWNER TO ocsigen;

--
-- Name: wikipages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE wikipages_id_seq OWNED BY wikipages.id;


--
-- Name: wikipages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('wikipages_id_seq', 1, false);


--
-- Name: wikis_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE wikis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.wikis_id_seq OWNER TO ocsigen;

--
-- Name: wikis_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ocsigen
--

ALTER SEQUENCE wikis_id_seq OWNED BY wikis.id;


--
-- Name: wikis_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval('wikis_id_seq', 1, false);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE forums ALTER COLUMN id SET DEFAULT nextval('forums_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE messages ALTER COLUMN id SET DEFAULT nextval('messages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE service_parameters ALTER COLUMN id SET DEFAULT nextval('service_parameters_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE services ALTER COLUMN id SET DEFAULT nextval('services_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE textdata ALTER COLUMN id SET DEFAULT nextval('textdata_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE threads ALTER COLUMN id SET DEFAULT nextval('threads_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE wikipages ALTER COLUMN id SET DEFAULT nextval('wikipages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE wikis ALTER COLUMN id SET DEFAULT nextval('wikis_id_seq'::regclass);


--
-- Data for Name: forums; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY forums (id, title, descr, moderated, arborescent, reader, writer, moderator) FROM stdin;
\.


--
-- Data for Name: globalstore; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY globalstore (key, value) FROM stdin;
\.


--
-- Data for Name: messages; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY messages (id, datetime, thr_id, txt_id, hidden, parent_id, sticky, tree_min, tree_max, author_id) FROM stdin;
\.


--
-- Data for Name: service_parameters; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY service_parameters (id, service_id, name) FROM stdin;
\.


--
-- Data for Name: services; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY services (id, url) FROM stdin;
\.


--
-- Data for Name: textdata; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY textdata (id, txt) FROM stdin;
\.


--
-- Data for Name: threads; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY threads (id, subject, frm_id, hidden, datetime, article_id, author_id) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY users (id, login, password, fullname, email, permissions) FROM stdin;
0	anonymous	\N	Anonymous		\N
\.


--
-- Data for Name: wikipages; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY wikipages (id, suffix, wik_id, txt_id, author, datetime, subject) FROM stdin;
\.


--
-- Data for Name: wikis; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY wikis (id, title, descr) FROM stdin;
\.


--
-- Name: forums_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_pkey PRIMARY KEY (id);


--
-- Name: globalstore_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY globalstore
    ADD CONSTRAINT globalstore_pkey PRIMARY KEY (key);


--
-- Name: messages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


--
-- Name: service_parameters_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY service_parameters
    ADD CONSTRAINT service_parameters_pkey PRIMARY KEY (id);


--
-- Name: services_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY services
    ADD CONSTRAINT services_pkey PRIMARY KEY (id);


--
-- Name: textdata_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY textdata
    ADD CONSTRAINT textdata_pkey PRIMARY KEY (id);


--
-- Name: threads_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: wikipages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_pkey PRIMARY KEY (id);


--
-- Name: wikipages_wik_id_suffix_key; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wik_id_suffix_key UNIQUE (wik_id, suffix);


--
-- Name: wikis_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikis
    ADD CONSTRAINT wikis_pkey PRIMARY KEY (id);


--
-- Name: forums_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_moderator_fkey FOREIGN KEY (moderator) REFERENCES users(id);


--
-- Name: forums_reader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_reader_fkey FOREIGN KEY (reader) REFERENCES users(id);


--
-- Name: forums_writer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_writer_fkey FOREIGN KEY (writer) REFERENCES users(id);


--
-- Name: messages_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_author_id_fkey FOREIGN KEY (author_id) REFERENCES users(id);


--
-- Name: messages_thr_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_thr_id_fkey FOREIGN KEY (thr_id) REFERENCES threads(id);


--
-- Name: messages_txt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_txt_id_fkey FOREIGN KEY (txt_id) REFERENCES textdata(id);


--
-- Name: service_parameters_service_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY service_parameters
    ADD CONSTRAINT service_parameters_service_id_fkey FOREIGN KEY (service_id) REFERENCES services(id);


--
-- Name: threads_article_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_article_id_fkey FOREIGN KEY (article_id) REFERENCES textdata(id);


--
-- Name: threads_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_author_id_fkey FOREIGN KEY (author_id) REFERENCES users(id);


--
-- Name: threads_frm_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_frm_id_fkey FOREIGN KEY (frm_id) REFERENCES forums(id);


--
-- Name: wikipages_txt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_txt_id_fkey FOREIGN KEY (txt_id) REFERENCES textdata(id);


--
-- Name: wikipages_wik_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wik_id_fkey FOREIGN KEY (wik_id) REFERENCES wikis(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: pgsql
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM pgsql;
GRANT ALL ON SCHEMA public TO pgsql;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

