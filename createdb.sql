--
-- PostgreSQL database dump
--

SET client_encoding = 'LATIN9';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: pgsql
--

COMMENT ON SCHEMA public IS 'Standard public schema';


SET search_path = public, pg_catalog;

--
-- Name: tablefunc_crosstab_2; Type: TYPE; Schema: public; Owner: pgsql
--

CREATE TYPE tablefunc_crosstab_2 AS (
	row_name text,
	category_1 text,
	category_2 text
);


ALTER TYPE public.tablefunc_crosstab_2 OWNER TO pgsql;

--
-- Name: tablefunc_crosstab_3; Type: TYPE; Schema: public; Owner: pgsql
--

CREATE TYPE tablefunc_crosstab_3 AS (
	row_name text,
	category_1 text,
	category_2 text,
	category_3 text
);


ALTER TYPE public.tablefunc_crosstab_3 OWNER TO pgsql;

--
-- Name: tablefunc_crosstab_4; Type: TYPE; Schema: public; Owner: pgsql
--

CREATE TYPE tablefunc_crosstab_4 AS (
	row_name text,
	category_1 text,
	category_2 text,
	category_3 text,
	category_4 text
);


ALTER TYPE public.tablefunc_crosstab_4 OWNER TO pgsql;

--
-- Name: connectby(text, text, text, text, integer, text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION connectby(text, text, text, text, integer, text) RETURNS SETOF record
    AS '$libdir/tablefunc', 'connectby_text'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.connectby(text, text, text, text, integer, text) OWNER TO pgsql;

--
-- Name: connectby(text, text, text, text, integer); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION connectby(text, text, text, text, integer) RETURNS SETOF record
    AS '$libdir/tablefunc', 'connectby_text'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.connectby(text, text, text, text, integer) OWNER TO pgsql;

--
-- Name: connectby(text, text, text, text, text, integer, text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION connectby(text, text, text, text, text, integer, text) RETURNS SETOF record
    AS '$libdir/tablefunc', 'connectby_text_serial'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.connectby(text, text, text, text, text, integer, text) OWNER TO pgsql;

--
-- Name: connectby(text, text, text, text, text, integer); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION connectby(text, text, text, text, text, integer) RETURNS SETOF record
    AS '$libdir/tablefunc', 'connectby_text_serial'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.connectby(text, text, text, text, text, integer) OWNER TO pgsql;

--
-- Name: crosstab(text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab(text) RETURNS SETOF record
    AS '$libdir/tablefunc', 'crosstab'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab(text) OWNER TO pgsql;

--
-- Name: crosstab(text, integer); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab(text, integer) RETURNS SETOF record
    AS '$libdir/tablefunc', 'crosstab'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab(text, integer) OWNER TO pgsql;

--
-- Name: crosstab(text, text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab(text, text) RETURNS SETOF record
    AS '$libdir/tablefunc', 'crosstab_hash'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab(text, text) OWNER TO pgsql;

--
-- Name: crosstab2(text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab2(text) RETURNS SETOF tablefunc_crosstab_2
    AS '$libdir/tablefunc', 'crosstab'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab2(text) OWNER TO pgsql;

--
-- Name: crosstab3(text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab3(text) RETURNS SETOF tablefunc_crosstab_3
    AS '$libdir/tablefunc', 'crosstab'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab3(text) OWNER TO pgsql;

--
-- Name: crosstab4(text); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION crosstab4(text) RETURNS SETOF tablefunc_crosstab_4
    AS '$libdir/tablefunc', 'crosstab'
    LANGUAGE c STABLE STRICT;


ALTER FUNCTION public.crosstab4(text) OWNER TO pgsql;

--
-- Name: normal_rand(integer, double precision, double precision); Type: FUNCTION; Schema: public; Owner: pgsql
--

CREATE FUNCTION normal_rand(integer, double precision, double precision) RETURNS SETOF double precision
    AS '$libdir/tablefunc', 'normal_rand'
    LANGUAGE c STRICT;


ALTER FUNCTION public.normal_rand(integer, double precision, double precision) OWNER TO pgsql;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: forums; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE forums (
    id serial NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    moderated boolean DEFAULT false NOT NULL
);


ALTER TABLE public.forums OWNER TO ocsigen;

--
-- Name: forums_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE forums_id_seq
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
-- Name: globalstore; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE globalstore (
    "key" text NOT NULL,
    value bytea
);


ALTER TABLE public.globalstore OWNER TO ocsigen;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE messages (
    id serial NOT NULL,
    author text DEFAULT ''::text NOT NULL,
    datetime timestamp without time zone DEFAULT (now())::timestamp without time zone NOT NULL,
    thr_id integer NOT NULL,
    txt_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    parent_id integer DEFAULT 0 NOT NULL,
    sticky boolean DEFAULT false NOT NULL
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
-- Name: messages_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE messages_id_seq
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
-- Name: textdata; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE textdata (
    id serial NOT NULL,
    txt text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.textdata OWNER TO ocsigen;

--
-- Name: textdata_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE textdata_id_seq
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
-- Name: threads; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE threads (
    id serial NOT NULL,
    subject text DEFAULT ''::text NOT NULL,
    frm_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    author text DEFAULT ''::text NOT NULL,
    article_id integer
);


ALTER TABLE public.threads OWNER TO ocsigen;

--
-- Name: COLUMN threads.frm_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN threads.frm_id IS 'forum';


--
-- Name: threads_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE threads_id_seq
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
-- Name: wikipages; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE wikipages (
    id serial NOT NULL,
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
-- Name: wikis; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE wikis (
    id serial NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.wikis OWNER TO ocsigen;

--
-- Name: wikis_id_seq; Type: SEQUENCE; Schema: public; Owner: ocsigen
--

CREATE SEQUENCE wikis_id_seq
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

ALTER TABLE textdata ALTER COLUMN id SET DEFAULT nextval('textdata_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE threads ALTER COLUMN id SET DEFAULT nextval('threads_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE wikipages ALTER COLUMN id SET DEFAULT nextval('wikipages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ocsigen
--

ALTER TABLE wikis ALTER COLUMN id SET DEFAULT nextval('wikis_id_seq'::regclass);


--
-- Name: forums_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_pkey PRIMARY KEY (id);


--
-- Name: globalstore_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY globalstore
    ADD CONSTRAINT globalstore_pkey PRIMARY KEY ("key");


--
-- Name: messages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


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
-- Name: threads_article_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_article_id_fkey FOREIGN KEY (article_id) REFERENCES textdata(id);


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

