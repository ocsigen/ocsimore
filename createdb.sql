--
-- PostgreSQL database dump
--

-- Started on 2006-11-29 19:13:52 CET

SET client_encoding = 'LATIN9';
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 1580 (class 0 OID 0)
-- Dependencies: 5
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1201 (class 1259 OID 25974)
-- Dependencies: 1535 1536 1537 5
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
-- TOC entry 1202 (class 1259 OID 25983)
-- Dependencies: 5
-- Name: globalstore; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE globalstore (
    "key" text NOT NULL,
    value bytea
);


ALTER TABLE public.globalstore OWNER TO ocsigen;

--
-- TOC entry 1204 (class 1259 OID 25990)
-- Dependencies: 1539 1540 1541 5
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
-- TOC entry 1582 (class 0 OID 0)
-- Dependencies: 1204
-- Name: COLUMN messages.thr_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.thr_id IS 'thread';


--
-- TOC entry 1583 (class 0 OID 0)
-- Dependencies: 1204
-- Name: COLUMN messages.txt_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.txt_id IS 'text';


--
-- TOC entry 1584 (class 0 OID 0)
-- Dependencies: 1204
-- Name: COLUMN messages.hidden; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN messages.hidden IS 'for moderated forums';


--
-- TOC entry 1206 (class 1259 OID 26001)
-- Dependencies: 1543 5
-- Name: textdata; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE textdata (
    id serial NOT NULL,
    txt text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.textdata OWNER TO ocsigen;

--
-- TOC entry 1208 (class 1259 OID 26010)
-- Dependencies: 1545 1546 1547 1548 5
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
-- TOC entry 1585 (class 0 OID 0)
-- Dependencies: 1208
-- Name: COLUMN threads.frm_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN threads.frm_id IS 'forum';


--
-- TOC entry 1210 (class 1259 OID 26022)
-- Dependencies: 1550 1551 1552 1553 5
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
-- TOC entry 1586 (class 0 OID 0)
-- Dependencies: 1210
-- Name: COLUMN wikipages.wik_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN wikipages.wik_id IS 'wiki';


--
-- TOC entry 1587 (class 0 OID 0)
-- Dependencies: 1210
-- Name: COLUMN wikipages.txt_id; Type: COMMENT; Schema: public; Owner: ocsigen
--

COMMENT ON COLUMN wikipages.txt_id IS 'text';


--
-- TOC entry 1212 (class 1259 OID 26034)
-- Dependencies: 1555 1556 5
-- Name: wikis; Type: TABLE; Schema: public; Owner: ocsigen; Tablespace: 
--

CREATE TABLE wikis (
    id serial NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.wikis OWNER TO ocsigen;

--
-- TOC entry 1558 (class 2606 OID 26043)
-- Dependencies: 1201 1201
-- Name: forums_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_pkey PRIMARY KEY (id);


--
-- TOC entry 1560 (class 2606 OID 26045)
-- Dependencies: 1202 1202
-- Name: globalstore_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY globalstore
    ADD CONSTRAINT globalstore_pkey PRIMARY KEY ("key");


--
-- TOC entry 1562 (class 2606 OID 26047)
-- Dependencies: 1204 1204
-- Name: messages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


--
-- TOC entry 1564 (class 2606 OID 26049)
-- Dependencies: 1206 1206
-- Name: textdata_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY textdata
    ADD CONSTRAINT textdata_pkey PRIMARY KEY (id);


--
-- TOC entry 1566 (class 2606 OID 26051)
-- Dependencies: 1208 1208
-- Name: threads_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_pkey PRIMARY KEY (id);


--
-- TOC entry 1568 (class 2606 OID 26053)
-- Dependencies: 1210 1210
-- Name: wikipages_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_pkey PRIMARY KEY (id);


--
-- TOC entry 1570 (class 2606 OID 26055)
-- Dependencies: 1210 1210 1210
-- Name: wikipages_wik_id_suffix_key; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wik_id_suffix_key UNIQUE (wik_id, suffix);


--
-- TOC entry 1572 (class 2606 OID 26057)
-- Dependencies: 1212 1212
-- Name: wikis_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsigen; Tablespace: 
--

ALTER TABLE ONLY wikis
    ADD CONSTRAINT wikis_pkey PRIMARY KEY (id);


--
-- TOC entry 1573 (class 2606 OID 26058)
-- Dependencies: 1204 1208 1565
-- Name: messages_thr_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_thr_id_fkey FOREIGN KEY (thr_id) REFERENCES threads(id);


--
-- TOC entry 1574 (class 2606 OID 26063)
-- Dependencies: 1204 1206 1563
-- Name: messages_txt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_txt_id_fkey FOREIGN KEY (txt_id) REFERENCES textdata(id);


--
-- TOC entry 1575 (class 2606 OID 26068)
-- Dependencies: 1208 1201 1557
-- Name: threads_frm_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_frm_id_fkey FOREIGN KEY (frm_id) REFERENCES forums(id);


--
-- TOC entry 1576 (class 2606 OID 26073)
-- Dependencies: 1210 1206 1563
-- Name: wikipages_txt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_txt_id_fkey FOREIGN KEY (txt_id) REFERENCES textdata(id);


--
-- TOC entry 1577 (class 2606 OID 26078)
-- Dependencies: 1210 1212 1571
-- Name: wikipages_wik_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsigen
--

ALTER TABLE ONLY wikipages
    ADD CONSTRAINT wikipages_wik_id_fkey FOREIGN KEY (wik_id) REFERENCES wikis(id);


--
-- TOC entry 1581 (class 0 OID 0)
-- Dependencies: 5
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2006-11-29 19:13:53 CET

--
-- PostgreSQL database dump complete
--

