
--
-- Name: forums; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE forums (
    id serial NOT NULL primary key,
    title text DEFAULT ''::text NOT NULL,
    descr text DEFAULT ''::text NOT NULL,
    moderated boolean DEFAULT false NOT NULL,
    arborescent boolean NOT NULL,
    reader integer NOT NULL REFERENCES users(id),
    writer integer NOT NULL REFERENCES users(id),
    moderator integer NOT NULL REFERENCES users(id)
);


ALTER TABLE public.forums OWNER TO ocsimore;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE messages (
    id serial NOT NULL primary key,
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


ALTER TABLE public.messages OWNER TO ocsimore;

--
-- Name: COLUMN messages.thr_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN messages.thr_id IS 'thread';


--
-- Name: COLUMN messages.txt_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN messages.txt_id IS 'text';


--
-- Name: COLUMN messages.hidden; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN messages.hidden IS 'for moderated forums';

--
-- Name: textdata; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE textdata (
    id serial NOT NULL primary key,
    txt text DEFAULT ''::text NOT NULL
);




ALTER TABLE public.textdata OWNER TO ocsimore;

--
-- Name: threads; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE threads (
    id serial NOT NULL primary key,
    subject text DEFAULT ''::text NOT NULL,
    frm_id integer NOT NULL,
    hidden boolean DEFAULT false NOT NULL,
    datetime timestamp without time zone DEFAULT now() NOT NULL,
    article_id integer,
    author_id integer NOT NULL
);


ALTER TABLE public.threads OWNER TO ocsimore;

--
-- Name: COLUMN threads.frm_id; Type: COMMENT; Schema: public; Owner: ocsimore
--

COMMENT ON COLUMN threads.frm_id IS 'forum';


-- --
-- -- Data for Name: forums; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY forums (id, title, descr, moderated, arborescent, reader, writer, moderator) FROM stdin;
-- \.


-- --
-- -- Data for Name: messages; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY messages (id, datetime, thr_id, txt_id, hidden, parent_id, sticky, tree_min, tree_max, author_id) FROM stdin;
-- \.

-- --
-- -- Data for Name: textdata; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY textdata (id, txt) FROM stdin;
-- \.


-- --
-- -- Data for Name: threads; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY threads (id, subject, frm_id, hidden, datetime, article_id, author_id) FROM stdin;
-- \.



--
-- Name: forums_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_moderator_fkey FOREIGN KEY (moderator) REFERENCES users(id);


--
-- Name: forums_reader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_reader_fkey FOREIGN KEY (reader) REFERENCES users(id);


--
-- Name: forums_writer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY forums
    ADD CONSTRAINT forums_writer_fkey FOREIGN KEY (writer) REFERENCES users(id);


--
-- Name: messages_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_author_id_fkey FOREIGN KEY (author_id) REFERENCES users(id);


--
-- Name: messages_thr_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_thr_id_fkey FOREIGN KEY (thr_id) REFERENCES threads(id);


--
-- Name: messages_txt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_txt_id_fkey FOREIGN KEY (txt_id) REFERENCES textdata(id);

--
-- Name: threads_article_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_article_id_fkey FOREIGN KEY (article_id) REFERENCES textdata(id);


--
-- Name: threads_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_author_id_fkey FOREIGN KEY (author_id) REFERENCES users(id);


--
-- Name: threads_frm_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
--

ALTER TABLE ONLY threads
    ADD CONSTRAINT threads_frm_id_fkey FOREIGN KEY (frm_id) REFERENCES forums(id);


ALTER TABLE messages RENAME TO forums_messages;
ALTER TABLE textdata RENAME TO forums_textdata;
ALTER TABLE threads RENAME TO forums_threads;
