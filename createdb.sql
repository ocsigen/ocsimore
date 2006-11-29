--
-- PostgreSQL database dump
--

SET client_encoding = 'LATIN9';
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: ocsigen; Type: DATABASE; Schema: -; Owner: ocsigen
--

CREATE DATABASE ocsigen WITH TEMPLATE = template0 ENCODING = 'LATIN9';


ALTER DATABASE ocsigen OWNER TO ocsigen;

\connect ocsigen

SET client_encoding = 'LATIN9';
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


SET search_path = public, pg_catalog;

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
-- Name: forums_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('forums', 'id'), 16, true);


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
    hidden boolean DEFAULT false NOT NULL
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
-- Name: messages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('messages', 'id'), 12, true);


--
-- Name: textdata; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE textdata (
    id serial NOT NULL,
    txt text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.textdata OWNER TO ocsigen;

--
-- Name: textdata_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('textdata', 'id'), 25, true);


--
-- Name: threads; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE threads (
    id serial NOT NULL,
    subject text DEFAULT ''::text NOT NULL,
    frm_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    author text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.threads OWNER TO ocsigen;

--
-- Name: COLUMN threads.frm_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN threads.frm_id IS 'forum';


--
-- Name: threads_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('threads', 'id'), 3, true);


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
-- Name: wikipages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('wikipages', 'id'), 4, true);


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
-- Name: wikis_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ocsigen
--

SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('wikis', 'id'), 2, true);


--
-- Data for Name: forums; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY forums (id, title, descr, moderated) FROM stdin;
15	OCaml	Discussions about the OCaml language	t
16	Meta-Forum	The Forum module of this site	f
\.


--
-- Data for Name: globalstore; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY globalstore ("key", value) FROM stdin;
forum1_mod	\\204\\225\\246\\276\\000\\000\\000B\\000\\000\\000\\013\\000\\000\\000,\\000\\000\\000&\\240\\300*forum1_mod@9Moderators group - Forum1 \\300@\\240\\300 @.Anonymous user @@A
developers	\\204\\225\\246\\276\\000\\000\\0009\\000\\000\\000\\013\\000\\000\\000*\\000\\000\\000%\\240\\300*developers@0Developers group \\300@\\240\\300 @.Anonymous user @@A
FORUM1	\\204\\225\\246\\276\\000\\000\\000\\010\\000\\000\\000\\001\\000\\000\\000\\003\\000\\000\\000\\003\\022_i\\000\\000\\000\\000\\017
FORUM2	\\204\\225\\246\\276\\000\\000\\000\\010\\000\\000\\000\\001\\000\\000\\000\\003\\000\\000\\000\\003\\022_i\\000\\000\\000\\000\\020
global_users_container	\\204\\225\\246\\276\\000\\000\\0020\\000\\000\\000Q\\000\\000\\001Y\\000\\000\\001"\\240\\300$root\\220$root-Administrator;piero@localhost.localdomain\\300\\300@\\240\\300 @.Anonymous user @\\300@\\240\\300*developers@0Developers group \\300@\\004\\014@A@AB\\240\\300*forum1_mod@9Moderators group - Forum1 \\300@\\004\\022@A\\300\\300@\\240\\300%piero\\220%piero-Piero Furiesi;piero@localhost.localdomain\\300@\\004\\034@A@A\\240\\300&piero3\\220(oJAASEfV-Piero Furiesi/piero@localhost\\300@\\004$@A\\300\\300@\\240\\300&piero4\\220(fRqXofrw%piero;piero@localhost.localdomain\\300@\\004.@A\\300@\\240\\300&piero5\\220(9jbSS5wH%piero;piero@localhost.localdomain\\300@\\0047@A@AB\\240\\300&piero7\\220(DslXDhDK-Piero Furiesi;piero@localhost.localdomain\\300@\\004?@A\\300@\\240\\300'piero78\\220(x3SY63lv-Piero Furiesi;piero@localhost.localdomain\\300@\\004H@A@ACDE
\.


--
-- Data for Name: messages; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY messages (id, author, datetime, thr_id, txt_id, hidden) FROM stdin;
2	root	2006-11-25 18:34:37.794117	1	15	f
3	root	2006-11-25 18:37:13.362613	1	16	f
4	root	2006-11-25 18:37:23.509493	1	17	f
5	<anonymous>	2006-11-25 18:37:47.991547	2	18	f
6	<anonymous>	2006-11-26 10:31:38.049602	3	19	f
7	<anonymous>	2006-11-26 18:17:27.892339	3	20	f
10	<anonymous>	2006-11-26 18:58:52.347353	3	23	f
9	<anonymous>	2006-11-26 18:58:46.247199	3	22	f
8	<anonymous>	2006-11-26 18:58:37.994509	3	21	f
12	<anonymous>	2006-11-26 18:59:57.689928	3	25	f
11	<anonymous>	2006-11-26 18:59:50.290033	3	24	f
\.


--
-- Data for Name: textdata; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY textdata (id, txt) FROM stdin;
6	adsfadsf
7	fsadads
8	fdsasad
9	fdsfsad
10	fsadsdf
11	fdgshdfhd
12	hgjkghjhgjdg
13	jdjdghfhdfh
14	hsfhsdfhfhsrthrth
15	primo messaggio del nuovo thread
16	secondo messaggio
17	altro messaggio
18	nuovo messaggio
19	primo msg thread #2
20	secondo messaggio\r\n
21	gadsfgag qrgrecgfwqcef
22	vqwvfqewfvecfqcwfqxrqwzqwrzqzr\r\n
23	cqwrxwxrqewztxzqzegfzqtrxzqrqr
24	fxqwxfqwxfqwefxqwxfwerqxf
25	settmio
\.


--
-- Data for Name: threads; Type: TABLE DATA; Schema: public; Owner: ocsigen
--

COPY threads (id, subject, frm_id, hidden, datetime, author) FROM stdin;
1	nuovo thread	16	f	2006-11-25 18:34:37.794117	root
2	primo thread	15	f	2006-11-25 18:37:47.991547	<anonymous user>
3	thread #2	15	f	2006-11-26 10:31:38.049602	<anonymous user>
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
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

