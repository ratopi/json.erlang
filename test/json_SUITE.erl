%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 19. Okt 2018 00:16
%%%-------------------------------------------------------------------
-module(json_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([boolean/1, boolean_whitespaces/1, string/1, integer/1, float/1, array/1, object/1]).

all() -> [
	boolean,
	boolean_whitespaces,
	string,
	integer,
	float,
	array,
	object
].

% ---

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.

end_per_testcase(_, _Config) ->
	ok.

% ---

boolean(_Config) ->
	{ok, true, <<>>} = json:parse(<<"true">>),
	{ok, false, <<>>} = json:parse(<<"false">>).

boolean_whitespaces(_Config) ->
	{ok, true, <<>>} = json:parse(<<"  true">>),
	{ok, true, <<"  ">>} = json:parse(<<"  true  ">>),
	{ok, true, <<"  ">>} = json:parse(<<"true  ">>),
	{ok, false, <<>>} = json:parse(<<"  false">>),
	{ok, false, <<"  ">>} = json:parse(<<"  false  ">>),
	{ok, false, <<"  ">>} = json:parse(<<"false  ">>).

string(_Config) ->
	{ok, "Hello world", <<>>} = json:parse(<<"\"Hello world\"">>),
	{ok, "", <<>>} = json:parse(<<"\"\"">>).

integer(_Config) ->
	{ok, 3, <<>>} = json:parse(<<"3">>),
	{ok, 0, <<>>} = json:parse(<<"0">>),
	{ok, -5, <<>>} = json:parse(<<"-5">>),
	{ok, 12, <<>>} = json:parse(<<"12">>).

float(_Config) ->
	{ok, 3.2, <<>>} = json:parse(<<"3.2">>),
	{ok, -6.5, <<>>} = json:parse(<<"-6.5">>),
	{ok, 3.0e10, <<>>} = json:parse(<<"3e10">>),
	{ok, -12.0e10, <<>>} = json:parse(<<"-12e10">>),
	{ok, 32.0e-10, <<>>} = json:parse(<<"32e-10">>),
	{ok, -34.0e-10, <<>>} = json:parse(<<"-34e-10">>).

array(_Config) ->
	{ok, [], <<>>} = json:parse(<<"[]">>),
	{ok, [true, "Hello", 3], <<>>} = json:parse(<<"[true,\"Hello\",3]">>).

object(_Config) ->
	{ok, #{}, <<>>} = json:parse(<<"{}">>),
	{ok, #{"a" => false}, <<>>} = json:parse(<<"{\"a\": true}">>),
	{ok, #{"a" => false, "b" => 23}, <<>>} = json:parse(<<"{\"a\": true, \"b\": 23}">>).
