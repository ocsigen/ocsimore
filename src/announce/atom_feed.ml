
type xhtmlDiv =
  {{ <div xmlns="http://www.w3.org/1999/xhtml">(Xhtmltypes_duce.flows) }}

type atomNCName = {{ String }}

type atomMediaType = {{ [Char* "/" Char*] }}

type atomDateConstruct = {{ String }}

type atomUri = {{ String }}

type atomIcon =  {{ <icon> atomUri }}

type atomId =  {{ <id> atomUri }}

type atomLink =
  {{ <link ({href  =? atomUri
             rel   =?(atomNCName | atomUri)
             type  =? atomMediaType
             title =? String
             length =? String })> [] }}

type atomLogo = {{ <logo> atomUri }}

type atomTitle = {{ <title> String }}

type atomUpdated = {{ <updated> String }}

type atomInlineXHTMLContent =
  {{ <content type = "xhtml"> [xhtmlDiv] }}

type atomContent = atomInlineXHTMLContent

type atomEmailAddress = {{ String }}

type atomPersonConstruct =
  {{ [ <name>String
       (<uri>atomUri) ?
       (<email>atomEmailAddress)? ] }}

type atomAuthor = {{ <author> atomPersonConstruct }}

type atomPublished = {{ <published> atomDateConstruct }}

type atomEntry =
(*
            "An atom:entry must have at least one atom:link element "
            ~ "with a rel attribute of 'alternate' "
            ~ "or an atom:content."
            "An atom:entry must have an atom:author "
            ~ "if its feed does not."
*)
  {{ <entry> [
       atomAuthor*
       atomContent?
       atomId
       atomLink*
       atomPublished?
       atomTitle
       atomUpdated ] }}

type atomFeed =
  {{ <feed xmlns="http://www.w3.org/2005/Atom"> [
       atomAuthor*
       atomIcon?
       atomId
       atomLink*
       atomLogo?
       atomTitle
       atomUpdated
       atomEntry* ] }}

type simpleEntry =
  {{ <entry> [
       atomId
       atomLink
       atomUpdated
       atomTitle
       atomAuthor
       atomContent ] }}

type simpleFeed =
  {{ <feed xmlns="http://www.w3.org/2005/Atom"> [
       atomId
       atomLink
       atomLink
       atomUpdated
       atomTitle
       simpleEntry* ] }}

type identity =
  { id : string;
    link : string;
    updated : CalendarLib.Calendar.t;
    title : string }

type entry =
  { e_id : identity;
    author : string;
    content : Xhtmltypes_duce.flows }

let str = Ocamlduce.Utf8.make
let date d = str (CalendarLib.Printer.Calendar.sprint "%iT%TZ" d)

let simple_entry e : simpleEntry =
  {{ <entry>
       [ <id> {:str e.e_id.id:}
         <link href={:str e.e_id.link:}>[]
         <updated> {:date e.e_id.updated:}
         <title> {:str e.e_id.title:}
         <author>[<name>{:str e.author:}]
         <content type="xhtml">
           [ <div xmlns="http://www.w3.org/1999/xhtml"> {:e.content:}] ]
  }}

let simple_feed url id el : simpleFeed =
  {{ <feed xmlns="http://www.w3.org/2005/Atom">
       [ <id> {:str id.id:}
         <link rel="self" href={:str url:}>[]
         <link href={:str id.link:}>[]
         <updated> {:date id.updated:}
         <title> {:str id.title:}
         !{: List.map simple_entry el :} ] }}

let serialize x =
  let b = Buffer.create 1024 in
  Buffer.add_string b "<?xml version='1.0' encoding='UTF-8'?>";
  Ocamlduce.Print.print_xml (fun s -> Buffer.add_string b s) x;
  Buffer.contents b

let simple_feed url id el = serialize (simple_feed url id el)

(*
date-time       = full-date "T" full-time
full-date       = date-fullyear "-" date-month "-" date-mday
date-fullyear   = 4DIGIT
date-month      = 2DIGIT  ; 01-12
date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31
full-time       = partial-time time-offset
partial-time    = time-hour ":" time-minute ":" time-second
                  [time-secfrac]
time-hour       = 2DIGIT  ; 00-23
time-minute     = 2DIGIT  ; 00-59
time-second     = 2DIGIT  ; 00-58, 00-59, 00-60
time-secfrac    = "." 1*DIGIT
time-offset     = "Z" / time-numoffset


"%iT%TZ"

%Y-%m-%dT%H:%M:%S
*)
