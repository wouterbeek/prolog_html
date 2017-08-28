%! media_type_table(+Module:atom)// is det.

media_type_table(Module) -->
  {findall(MediaType, Module:media_type(MediaType), MediaTypes)},
  table(
    \table_header_row(["Format","Media Type"]),
    \html_maplist(media_type_data_row, MediaTypes)
  ).

media_type_data_row(Type/Subtype) -->
  {
    media_type_label(Type/Subtype, Label),
    format(string(MediaType), "~a/~a", [Type,Subtype])
  },
  table_data_row([Label,code(MediaType)]).

media_type_label(media(application/json,_), "JSON").
media_type_label(media(application/'ld+json',_), "JSON-LD 1.0").
media_type_label(media(application/'n-triples',_), "N-Triples 1.1").
media_type_label(media(application/'n-quads',_), "N-Quads 1.1").
media_type_label(media(application/'vnd.geo+json',_), "GeoJSON").
media_type_label(media(text/html,_), "HTML 5").
