(** A little parser for entering valid XHTML in wikipages. *)

(** [parse (srv,sp) s] generates valid XHTML code from string [s].

    [s] may contain a small set of commands for pretty
    formatting. [(srv,sp)] argument is used for link creation
    (see the [%:] command below).

    All commands begin with a percent sign ['%']. There are {i "row commands"} and {i "inline commands"}.
    
    {b Row commands} are placed at the beginning of a row; they affect the whole row, or a set of ones:
    - [%###] {i [row]} formats the {i [row]} as a {b high-level title} {i (mnemonic: [#] has a high number of dashes)};
    - [%===] {i [row]} formats the {i [row]} as a {b medium-level title} {i (mnemonic: [=] has two dashes)};
    - [%---] {i [row]} formats the {i [row]} as a {b low-level title} {i (mnemonic: [-] has one dash only)};
    - [%pre\[]
    {L {i [...]}}
    {L {i [rows...]}}
    {L {i [...]}}
    [%\]]
    {L formats {i [rows...]} as {b preformatted code}; the text on the right of [%pre\[] and [%\]] is discarded} {i (mnemonic: [pre]format from here [\[] to there [\]])};
    - {i [(void)]} a row starting with no row commands is formatted as a simple {b paragraph}, whose text may be affected by inline commands.

    {b Inline commands} are used to change the appearance of (portions of) text in simple paragraphs. An inline command affects all the following text, until the first of: a) another inline command, or b) a [%] sign, or c) the end of the line is encountered:
    - [%/] {i [text]} formats the {i [text]} with {b emphasis} {i (mnemonic: [/text/] means "italic" in some mail composers)};
    - [%*] {i [text]} as above, with a {b strong emphasis} {i (mnemonic: [*text*] means "bold" in some mail composers)};
    - [%\[] {i [text]} treats the {i [text]} as a {b code} fragment {i (mnemonic: similar to the analogous row command)};
    - [%:] {i [text]} make a link to the service [srv], using server parameters [sp], and passing {i [text]} as a get parameter {i (mnemonic: [:] as in http[:])}.

    {b Escape:} to include a literal [%] in text affected by an inline command, use the escaped form [\%].
 *)

open Eliommod
open Eliomparameters
open Eliomservices

val parse :
    (string, unit, [< get_service_kind ], [< suff ], 'a,
     unit, [< registrable ]) service * server_params ->
    string -> {{ Xhtml1_strict.flows }}
