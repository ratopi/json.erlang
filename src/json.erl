%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 14. Okt 2018 17:58
%%%-------------------------------------------------------------------
-module(json).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([parse/1]).

-compile(export_all).


parse(Binary) ->
	parse_element(skip_whitespaces(Binary)).



parse_element(<<>>) ->
	eof;

parse_element(<<"true", Rest/binary>>) ->
	{ok, true, Rest};

parse_element(<<"false", Rest/binary>>) ->
	{ok, false, Rest};

parse_element(<<$-, Rest/binary>>) ->
	parse_number(Rest, {integer, "-"});

parse_element(Binary = <<Digit, _/binary>>) when Digit >= $0, Digit =< $9 ->
	parse_number(Binary, {integer, ""});

parse_element(<<$", Rest/binary>>) ->
	parse_string(Rest, "");

parse_element(<<$[, Rest/binary>>) ->
	parse_array(Rest, [], element);

parse_element(<<${, Rest/binary>>) ->
	parse_object(Rest, [], key);

parse_element(<<Rest/binary>>) ->
	{error, syntax_error, {"Cannot parse", Rest}}.



parse_string(<<$\\, $", Rest/binary>>, String) ->
	parse_string(Rest, String ++ [$"]);

parse_string(<<$\\, $\\, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [$\\]);

parse_string(<<$\\, $/, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [$/]);

parse_string(<<$\\, $b, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [8]);

parse_string(<<$\\, $f, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [12]);

parse_string(<<$\\, $n, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [10]);

parse_string(<<$\\, $r, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [13]);

parse_string(<<$\\, $t, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [9]);

parse_string(<<$\\, Letter, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [Letter]);

parse_string(<<$", Rest/binary>>, String) ->
	{ok, String, Rest};

parse_string(<<Letter, Rest/binary>>, String) ->
	parse_string(Rest, String ++ [Letter]).


parse_number(<<Digit, Rest/binary>>, {Type, Text}) when Digit >= $0, Digit =< $9 ->
	parse_number(Rest, {Type, [Digit | Text]});

parse_number(<<$., Rest/binary>>, {integer, Text}) ->
	parse_number(Rest, {float, [$. | Text]});

parse_number(<<$e, $-, Rest/binary>>, {integer, Text}) ->
	parse_number(Rest, {float, [$-, $e, $0, $. | Text]});

parse_number(<<$e, Rest/binary>>, {integer, Text}) ->
	parse_number(Rest, {float, [$e, $0, $. | Text]});

parse_number(<<$e, $-, Rest/binary>>, {float, Text}) ->
	parse_number(Rest, {float, [$-, $e | Text]});

parse_number(<<$e, Rest/binary>>, {float, Text}) ->
	parse_number(Rest, {float, [$e | Text]});

parse_number(<<Rest/binary>>, {integer, Text}) ->
	{ok, list_to_integer(lists:reverse(Text)), Rest};

parse_number(<<Rest/binary>>, {float, Text}) ->
	{ok, list_to_float(lists:reverse(Text)), Rest}.


parse_array(<<$], Rest/binary>>, Array, _) ->
	{ok, Array, Rest};

parse_array(<<32, Rest/binary>>, Array, Phase) ->
	parse_array(Rest, Array, Phase);

parse_array(<<$,, Rest/binary>>, Array, separator) ->
	parse_array(Rest, Array, element);

parse_array(<<Bin/binary>>, Array, element) ->
	{ok, Element, Rest} = parse(Bin),
	parse_array(Rest, Array ++ [Element], separator);

parse_array(_, _, _) ->
	{error, syntax_error, "Syntax error in parsing array"}.



parse_object(<<32, Rest/binary>>, Map, Phase) ->
	parse_object(Rest, Map, Phase);

parse_object(<<$}, Rest/binary>>, Map, key) ->
	{ok, Map, Rest};

parse_object(<<Binary/binary>>, Map, key) ->
	{ok, Key, Rest} = parse(Binary),
	parse_object(Rest, Map, {separator, Key});

parse_object(<<$:, Rest/binary>>, Map, {separator, Key}) ->
	parse_object(Rest, Map, {value, Key});

parse_object(<<Binary/binary>>, Map, {value, Key}) ->
	{ok, Value, Rest} = parse(Binary),
	parse_object(Rest, Map ++ [{Key, Value}], key);

parse_object(_, _, _) ->
	{error, syntax_error, "Syntax error in parsing object"}.



skip_whitespaces(<<" ", Rest/binary>>) ->
	skip_whitespaces(Rest);
skip_whitespaces(<<"\t", Rest/binary>>) ->
	skip_whitespaces(Rest);
skip_whitespaces(<<"\n", Rest/binary>>) ->
	skip_whitespaces(Rest);
skip_whitespaces(<<"\r", Rest/binary>>) ->
	skip_whitespaces(Rest);
skip_whitespaces(<<Rest/binary>>) ->
	Rest.
