
--
-- PostgreSQL database dump
--

SET client_encoding = 'UTF-8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


-- PAS FINI
-- --
-- -- Name: service_parameters; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
-- --

-- CREATE TABLE service_parameters (
--     id serial NOT NULL,
--     service_id integer NOT NULL,
--     name text NOT NULL
-- );


-- ALTER TABLE public.service_parameters OWNER TO ocsimore;

-- --
-- -- Name: services; Type: TABLE; Schema: public; Owner: ocsimore; Tablespace: 
-- --

-- CREATE TABLE services (
--     id serial NOT NULL,
--     url text NOT NULL
-- );


-- -- ALTER TABLE public.services OWNER TO ocsimore;

-- --
-- -- Name: tablefunc_crosstab_2; Type: TYPE; Schema: public; Owner: ocsimore
-- --

-- CREATE TYPE tablefunc_crosstab_2 AS (
-- 	row_name text,
-- 	category_1 text,
-- 	category_2 text
-- );


-- ALTER TYPE public.tablefunc_crosstab_2 OWNER TO ocsimore;

-- --
-- -- Name: tablefunc_crosstab_3; Type: TYPE; Schema: public; Owner: ocsimore
-- --

-- CREATE TYPE tablefunc_crosstab_3 AS (
-- 	row_name text,
-- 	category_1 text,
-- 	category_2 text,
-- 	category_3 text
-- );


-- ALTER TYPE public.tablefunc_crosstab_3 OWNER TO ocsimore;

-- --
-- -- Name: tablefunc_crosstab_4; Type: TYPE; Schema: public; Owner: ocsimore
-- --

-- CREATE TYPE tablefunc_crosstab_4 AS (
-- 	row_name text,
-- 	category_1 text,
-- 	category_2 text,
-- 	category_3 text,
-- 	category_4 text
-- );


-- ALTER TYPE public.tablefunc_crosstab_4 OWNER TO ocsimore;


-- --
-- -- Data for Name: service_parameters; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY service_parameters (id, service_id, name) FROM stdin;
-- \.


-- --
-- -- Data for Name: services; Type: TABLE DATA; Schema: public; Owner: ocsimore
-- --

-- COPY services (id, url) FROM stdin;
-- \.




-- --
-- -- Name: service_parameters_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
-- --

-- ALTER TABLE ONLY service_parameters
--     ADD CONSTRAINT service_parameters_pkey PRIMARY KEY (id);


-- --
-- -- Name: services_pkey; Type: CONSTRAINT; Schema: public; Owner: ocsimore; Tablespace: 
-- --

-- ALTER TABLE ONLY services
--     ADD CONSTRAINT services_pkey PRIMARY KEY (id);


-- --
-- -- Name: service_parameters_service_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ocsimore
-- --

-- ALTER TABLE ONLY service_parameters
--     ADD CONSTRAINT service_parameters_service_id_fkey FOREIGN KEY (service_id) REFERENCES services(id);



--
-- Name: public; Type: ACL; Schema: -; Owner: pgsql
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


