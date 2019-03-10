% A simple JSON library for Mercury.

:- module json.
:- interface.

:- import_module assoc_list, bool, float, int, io, list, parsing_utils, string.

:- type json.

% Output format of a json object.

:- type format
	---> format(
		object_spec::format_spec,
		array_spec::format_spec
	).

% Format specifier.

:- type format_spec
	---> compact
	;	 multiline(
		indent::int,
		newline::newline
	).

:- type newline
	---> crlf
	;	 cr
	;	 lf.

% Create a json.

:- pred from_int(int::in, json::out) is det.

:- pred from_float(float::in, json::out) is det.

:- pred from_string(string::in, json::out) is det.

:- pred from_bool(bool::in, json::out) is det.

:- pred from_list(list(json)::in, json::out) is det.

:- pred from_assoc_list(assoc_list(string, json)::in, json::out) is det.

:- pred from_nothing(json::out) is det.

:- func from_int(int::in) = (json::out) is det.

:- func from_float(float::in) = (json::out) is det.

:- func from_string(string::in) = (json::out) is det.

:- func from_bool(bool::in) = (json::out) is det.

:- func from_list(list(json)::in) = (json::out) is det.

:- func from_assoc_list(assoc_list(string, json)::in) = (json::out) is det.

:- func from_nothing = (json::out) is det.

% Check the type of the content in a json.

:- pred is_integer(json::in) is semidet.

:- pred is_float(json::in) is semidet.

:- pred is_string(json::in) is semidet.

:- pred is_boolean(json::in) is semidet.

:- pred is_array(json::in) is semidet.

:- pred is_object(json::in) is semidet.

:- pred is_null(json::in) is semidet.

% Get the content of a json.

:- func as_int(json::in) = (int::out) is semidet.
/*
:- func det_as_int(json) = int.
*/
:- func as_float(json::in) = (float::out) is semidet.
/*
:- func det_as_float(json) = float.
*/
:- func as_string(json::in) = (string::out) is semidet.
/*
:- func det_as_string(json) = string.
*/
:- func as_bool(json::in) = (bool::out) is semidet.
/*
:- func det_as_bool(json) = bool.
*/
:- func as_assoc_list(json::in) = (assoc_list(string, json)::out) is semidet.
/*
:- func det_as_assoc_list(json) = assoc_list(string, json).
*/
:- func as_list(json::in) = (list(json)::out) is semidet.
/*
:- func det_as_list(json) = list(json).
*/
% null has no content therefore no need for a predicate
% to extract its content.

% Conversion to string with a specified format.
:- func to_string(json, format) = string.

% Write a json to output with a specified format.
:- pred write_json(json::in, format::in, io::di, io::uo) is det.
:- pred write_json(io.text_output_stream::in, json::in, format::in, io::di, io::uo) is det.

% Same but with the default format.
/*
:- func to_string(json) = string.

:- pred write_json(json::in, io::di, io::uo) is det.
:- pred write_json(io.text_output_stream::in, json::in, io::di, io::uo) is det.
*/

% A JSON parser.
:- pred json_parser(src::in, json::out, ps::in, ps::out) is semidet.

% Parse a JSON string.
:- pred parse_json_string(string::in, parse_result(json)::out) is cc_multi.

:- implementation.

:- import_module pair, stream, stream.string_writer, string.builder.

:- type json
	--->	integer(int)
	;		float(float)
	;		string(string)
	;		boolean(bool)
	;		array(list(json))
	;		object(assoc_list(string, json))
	;		null.

from_int(Int, integer(Int)).

from_float(Float, float(Float)).

from_string(String, string(String)).

from_bool(Bool, boolean(Bool)).

from_list(List, array(List)).

from_assoc_list(AssocList, object(AssocList)).

from_nothing(null).

from_int(Int) = integer(Int).

from_float(Float) = float(Float).

from_string(String) = string(String).

from_bool(Bool) = boolean(Bool).

from_list(List) = array(List).

from_assoc_list(AssocList) = object(AssocList).

from_nothing = null.

is_integer(integer(_)).

is_float(float(_)).

is_string(string(_)).

is_boolean(boolean(_)).

is_array(array(_)).

is_object(object(_)).

is_null(null).

as_int(integer(Int)) = Int.

% Implement det_integer/1!!!

as_float(float(Float)) = Float.

% Implement det_float/1!!!

as_string(string(String)) = String.

% Implement det_string/1!!!

as_bool(boolean(Bool)) = Bool.

% Implement det_boolean/1!!!

as_list(array(List)) = List.

% Implement det_list/1!!!

as_assoc_list(object(Object)) = Object.

% Implement det_object/1!!!
/*
to_string(JSON, Format) = to_string(JSON, Format, 1).

:- func to_string(json, format, int) = string.

to_string(integer(Int), _, _) = int_to_string(Int).
to_string(float(Float), _, _) = float_to_string(Float).
to_string(string(String), _, _) = """" ++ String ++ """".
to_string(boolean(no), _, _) = "false".
to_string(boolean(yes), _, _) = "true".
to_string(array(List), Format @ format(_, compact), Layer) =
	"[" ++ foldl((func(JSON, Before) =
		(	if Before = ""
			then to_string(JSON, Format, Layer)
			else Before ++ ", " ++ to_string(JSON, Format, Layer)
		)
	), List, "") ++ "]".
to_string(array(List), Format @ format(_, multiline(Indent, Newline)), Layer) =
	"[" ++ newline_string(Newline) ++ foldl((func(JSON, Before) =
		(	if Before = ""
			then from_char_list(duplicate(Indent * Layer, ' ')) ++ to_string(JSON, Format, Layer + 1)
			else Before ++ "," ++ newline_string(Newline) ++ from_char_list(duplicate(Indent * Layer, ' ')) ++ to_string(JSON, Format, Layer + 1)
		)
	), List, "") ++ newline_string(Newline) ++ from_char_list(duplicate(Indent * (Layer - 1), ' ')) ++ "]".
to_string(object(AssocList), Format @ format(compact, _), Layer) =
	"{" ++ foldl((func((Key - JSON), Before) =
		(	if Before = ""
			then """" ++ Key ++ """: " ++ to_string(JSON, Format, Layer)
			else Before ++ ", """ ++ Key ++ """: " ++ to_string(JSON, Format, Layer)
		)
	), AssocList, "") ++ "}".
to_string(object(AssocList), Format @ format(multiline(Indent, Newline), _), Layer) =
	"{" ++ newline_string(Newline) ++ foldl((func((Key - JSON), Before) =
		(	if Before = ""
			then from_char_list(duplicate(Indent * Layer, ' ')) ++ """" ++ Key ++ """: " ++ to_string(JSON, Format, Layer + 1)
			else Before ++ "," ++ newline_string(Newline) ++ from_char_list(duplicate(Indent * Layer, ' ')) ++ """" ++ Key ++ """: " ++ to_string(JSON, Format, Layer + 1)
		)
	), AssocList, "") ++ newline_string(Newline) ++ from_char_list(duplicate(Indent * (Layer - 1), ' ')) ++ "}".
to_string(null, _, _) = "null".
*/

to_string(JSON, Format) = to_string(Output) :-
	put_json(JSON, Format, 1, init, Output).

:- pred put_json(json::in, format::in, int::in, string.builder.state::di, string.builder.state::uo).

put_json(integer(Int), _, _, !SB) :- put_int(handle, Int, !SB).
put_json(float(Float), _, _, !SB) :- put_float(handle, Float, !SB).
put_json(string(String), _, _, !SB) :-
	put(handle, '"', !SB),
	put(handle, String, !SB),
	put(handle, '"', !SB).
put_json(boolean(no), _, _, !SB) :- put(handle, "false", !SB).
put_json(boolean(yes), _, _, !SB) :- put(handle, "true", !SB).
put_json(array(List), Format @ format(_, compact), Layer, !SB) :-
	put(handle, '[', !SB),
	foldl2((pred(JSON::in, Comma::in, yes::out, !.SB0::di, !:SB0::uo) is det :-
		(	Comma = no,
			put_json(JSON, Format, Layer, !SB0)
		;	Comma = yes,
			put(handle, ", ", !SB0),
			put_json(JSON, Format, Layer, !SB0)
		)
	), List, no, _, !SB),
	put(handle, ']', !SB).
put_json(array(List), Format @ format(_, multiline(Indent, Newline)), Layer, !SB) :-
	put(handle, '[', !SB),
	put(handle, newline_string(Newline), !SB),
	foldl2((pred(JSON::in, Comma::in, yes::out, !.SB0::di, !:SB0::uo) is det :-
		(	Comma = no,
			put(handle, from_char_list(duplicate(Indent * Layer, ' ')), !SB0),
			put_json(JSON, Format, Layer + 1, !SB0)
		;	Comma = yes,
			put(handle, ',', !SB0),
			put(handle, newline_string(Newline), !SB0),
			put(handle, from_char_list(duplicate(Indent * Layer, ' ')), !SB0),
			put_json(JSON, Format, Layer + 1, !SB0)
		)
	), List, no, _, !SB),
	put(handle, newline_string(Newline), !SB),
	put(handle, from_char_list(duplicate(Indent * (Layer - 1), ' ')), !SB),
	put(handle, ']', !SB).
put_json(object(AssocList), Format @ format(compact, _), Layer, !SB) :-
	put(handle, '{', !SB),
	foldl2((pred((Key - JSON)::in, Comma::in, yes::out, !.SB0::di, !:SB0::uo) is det :-
		(	Comma = no,
			put(handle, '"', !SB0),
			put(handle, Key, !SB0),
			put(handle, """: ", !SB0),
			put_json(JSON, Format, Layer, !SB0)
		;	Comma = yes,
			put(handle, ", """, !SB0),
			put(handle, Key, !SB0),
			put(handle, """: ", !SB0),
			put_json(JSON, Format, Layer, !SB0)
		)
	), AssocList, no, _, !SB),
	put(handle, '}', !SB).
put_json(object(AssocList), Format @ format(multiline(Indent, Newline), _), Layer, !SB) :-
	put(handle, '{', !SB),
	put(handle, newline_string(Newline), !SB),
	foldl2((pred((Key - JSON)::in, Comma::in, yes::out, !.SB0::di, !:SB0::uo) is det :-
		(	Comma = no,
			put(handle, from_char_list(duplicate(Indent * Layer, ' ')), !SB0),
			put(handle, '"', !SB0),
			put(handle, Key, !SB0),
			put(handle, """: ", !SB0),
			put_json(JSON, Format, Layer + 1, !SB0)
		;	Comma = yes,
			put(handle, ',', !SB0),
			put(handle, newline_string(Newline), !SB0),
			put(handle, from_char_list(duplicate(Indent * Layer, ' ')), !SB0),
			put(handle, '"', !SB0),
			put(handle, Key, !SB0),
			put(handle, """: ", !SB0),
			put_json(JSON, Format, Layer + 1, !SB0)
		)
	), AssocList, no, _, !SB),
	put(handle, newline_string(Newline), !SB),
	put(handle, from_char_list(duplicate(Indent * (Layer - 1), ' ')), !SB),
	put(handle, '}', !SB).
put_json(null, _, _, !SB) :- put(handle, "null", !SB).

:- func newline_string(newline) = string.

newline_string(crlf) = "\r\n".
newline_string(cr) = "\r".
newline_string(lf) = "\n".

write_json(JSON, Format) --> write_string(to_string(JSON, Format)).

write_json(Stream, JSON, Format) --> write_string(Stream, to_string(JSON, Format)).

json_parser(Src, JSON, !PS) :-
	(	int_literal(Src, Int, !PS)
	->	JSON = integer(Int)
	;	float_literal(Src, Float, !PS)
	->	JSON = float(Float)
	;	string_literal('"', Src, String, !PS)
	->	JSON = string(String)
	;	keyword("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "true", Src, _, !PS)
	->	JSON = boolean(yes)
	;	keyword("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "false", Src, _, !PS)
	->	JSON = boolean(no)
	;	brackets("[", "]", comma_separated_list(json_parser), Src, List, !PS)
	->	JSON = array(List)
	;	brackets("{", "}", comma_separated_list(object_entry_parser), Src, AssocList, !PS)
	->	JSON = object(AssocList)
	;	keyword("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "null", Src, _, !PS)
	->	JSON = null
	;	fail
	).

:- pred object_entry_parser(src::in, pair(string, json)::out, ps::in, ps::out) is semidet.

object_entry_parser(Src, String - JSON) --> string_literal('"', Src, String), punct(":", Src, _), json_parser(Src, JSON).

parse_json_string(String, ParseResult) :- parse(String, json_parser, ParseResult).