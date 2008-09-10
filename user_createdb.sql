
--
-- Name: users; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--

CREATE TABLE users (
    id serial PRIMARY KEY,
    login text NOT NULL,
    password text, -- NULL when authtype='l' => no connection possible (this is a group)
    fullname text NOT NULL,
    email text,
    dyn boolean NOT NULL,
    authtype char NOT NULL -- 'p' for PAM, 'l' for local
);


ALTER TABLE public.users OWNER TO ocsimore;


CREATE TABLE userrights (
    id integer NOT NULL REFERENCES users(id),
    groupid integer NOT NULL REFERENCES users(id),
    CONSTRAINT uni UNIQUE (id, groupid)
);


--
-- Name: wikipages; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
--


-- --
-- -- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY users (id, login, password, fullname, email, permissions) FROM stdin;
-- 0	anonymous	\N	Anonymous		\N
-- \.

