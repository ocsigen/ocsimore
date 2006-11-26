val reset_input :
  ?a:[< `Accept
      | `Accesskey
      | `Alt
      | `Checked
      | `Class
      | `Disabled
      | `Id
      | `Input_Type
      | `Maxlength
      | `Name
      | `Readonly
      | `Size
      | `Src
      | `Tabindex
      | `Title
      | `Usemap
      | `Value
      | `XML_lang
      > `Input_Type `Value ]
     XHTML.M.attrib list ->
  XHTML.M.cdata -> [> `Input ] XHTML.M.elt
val select_option :
  ?a:[< `Class
      | `Disabled
      | `Id
      | `Multiple
      | `Name
      | `Size
      | `Tabindex
      | `Title
      | `XML_lang
      > `Name ]
     XHTML.M.attrib list ->
  ?default:'a ->
  ('a * string) list ->
  ('a -> XHTML.M.cdata) -> 'b Ocsigen.param_name -> [> `Select ] XHTML.M.elt
