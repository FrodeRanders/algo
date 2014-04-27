%%%  Copyright (C) 2009-2014 Frode Randers
%%%
%%%  This file is part of Algo.
%%% 
%%%  Algo is free software: you can redistribute it and/or modify it 
%%%  under the terms of the GNU Lesser General Public License as 
%%%  published by the Free Software Foundation, either version 3 of
%%%  the License, or (at your option) any later version.
%%%
%%%  Algo is distributed in the hope that it will be useful, but 
%%%  WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%%  GNU Lesser General Public License for more details.
%%%
%%%  You should have received a copy of the GNU Lesser General 
%%%  Public License along with Algo. If not, see 
%%%    <http://www.gnu.org/licenses/>.
%%%
-module(algo_test).
-author('Frode Randers').

-include("algo.hrl").

-export([start/0, test/1]).

-spec start() -> ['ok'] | {algo_has_no_versions, coreRef() }.
start() ->
    mnesia:start(),
    test(1000).

% debug(D) ->
%    io:format("Debug: ~p~n", [D]).
    
%%-------------------------------------------------------------------
%% Test of version functionality.
%%
%% Observe that this implementation is very CPU bound and memory exhaustive,
%% mainly because of the list handling.
%%
%% One good effect is that the transaction is efficient, but the effect of
%% distributing several transactions on many nodes can easily outperform the
%% current scheme. Also, a transaction should be kept as short as possible
%% and this is another key point that we ignore here - certainly to a cost.
%%
%% But we _do_ test the functions :)
%%-------------------------------------------------------------------
-spec test( integer() ) -> ['ok'] | { 'algo_has_no_versions', coreRef() }.
test(Num) ->
    % Test setup
    User = algo:establish_principal("cn=frode,ou=users,dc=test", 0),
    
    % Attributes; http://purl.org/dc/elements/1.1/creator
    {ok, _Attribute} = algo:define_attribute("dc:creator", string),

    % Type core
    Type = algo:new_type(#coreRef{instanceId=1, algoId=1}, "base type", User),
    
    % Type version(s) being PA1, A, PB1, B, PC1, PC2 where B is effective
    {latest, Version1} = algo:get_latest_version(Type),
    Version_PA1 = Version1#version{versionName="PA1", name="base type", modifier=User#principal.userId},
    {Type1, _ReplacedVersion} = algo:replace_version(Type, Version_PA1),

    {next, Version2} = algo:get_next_version(Type1),
    Type2 = algo:add_version(Type1, Version2#version{versionState=released, versionName="A"}),
    
    {next, Version3} = algo:get_next_version(Type2),
    Type3 = algo:add_version(Type2, Version3#version{versionState=preliminary, versionName="PB1"}),

    {next, Version4} = algo:get_next_version(Type3),
    Type4 = algo:add_version(Type3, Version4#version{versionState=released, versionName="B"}),
    
    {next, Version5} = algo:get_next_version(Type4),
    Type5 = algo:add_version(Type4, Version5#version{versionState=preliminary, versionName="PC1"}),
    
    {next, Version6} = algo:get_next_version(Type5),
    Type6 = algo:add_version(Type5, Version6#version{versionState=preliminary, versionName="PC2"}),
    
    ok = algo_version:assert_version_structure(Type6#algo.versions),

    % Save type
    algo_core:save(Type6), % may actually fail
    
    % Create 'Num' documents based on type 'Type'
    L = test(User, Type6, lists:seq(1, Num), []),
    
    algo:save_multiple(L).

%%-------------------------------------------------------------------
%% Internal function 
%%-------------------------------------------------------------------
-spec test( user(), coreRef(), [ integer() ], [ algo() ] ) -> 'ok' | { 'algo_has_no_versions', algo() }.
test(User, Type, [H|I], L) ->
    % Define a new reference, will currently ignore contents of database
    % and generate existing primary keys.
    NewRef = #coreRef{instanceId = 1, algoId = H + 1},
    
    % Create a new document with primary key 'NewRef'
    Document = algo:new_algo(NewRef, document, Type, User),
    
    % Add a version with an invented name (the count is the name)
    {latest, Version} = algo:get_latest_version(Document),
    Version1 = Version#version{versionId = 1, name = "doc-" ++ integer_to_list(H), modifier = User#principal.userId},
    
    % Associate version with algo and continue - save is
    % performed on all created documents later on.
    {Document1, _ReplacedVersion} = algo:replace_version(Document, Version1),
    
    %
    ok = algo_version:assert_version_structure(Document1#algo.versions),
    algo:save(Document1),
    
    test(User, Type, I, [Document1|L]);

test(_User, _Type, [], L) -> L.


