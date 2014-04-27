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
-module(algo_core).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% Public interface
-export([find_by_ref/2,
         save/1, save_multiple/1,
         remove/2]).

%%% Inter module interface
-export([new/2]).

-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
%%-define(PRINT(S, A), true).

%% --------------------------------------------------------------------
%% new/2
%%
%% Creates a new algo (without any versions still)
%% --------------------------------------------------------------------
-spec new( integer(), atom() ) -> algo().
new(InstanceId, generic) -> #algo{ref = ref(InstanceId), class = generic};
new(InstanceId, tree) -> #algo{ref = ref(InstanceId), class = tree};
new(InstanceId, container) -> #algo{ref = ref(InstanceId), class = container};
new(InstanceId, type) -> #algo{ref = ref(InstanceId), class = type};
new(InstanceId, document) -> #algo{ref = ref(InstanceId), class = document}.

%% --------------------------------------------------------------------
%% uuid/0
%%
%% Generates a unique id for an algo
%% --------------------------------------------------------------------
-spec uuid() -> string(). % somewhat
uuid() ->    
    P1 = random:uniform(trunc(math:pow(2, 48))) - 1,    
    P2 = random:uniform(trunc(math:pow(2, 12))) - 1,     
    P3 = random:uniform(trunc(math:pow(2, 32))) - 1,     
    P4 = random:uniform(trunc(math:pow(2, 30))) - 1,    
    <<P1:48, 4:4, P2:12, 2:2, P3:32, P4:30>>.    

-type uuid() :: string().
-spec uuid_to_string( uuid() ) -> string().
uuid_to_string(<<S1:32, S2:16, S3:16, S4:8, S5:8, S6:48>>) ->    
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", [S1, S2, S3, S4, S5, S6])).    

-spec ref( integer() ) -> coreRef().
ref(InstanceId) ->    
    #coreRef{instanceId = InstanceId, algoId = uuid_to_string(uuid())}.    

%% --------------------------------------------------------------------
%% save/1
%%
%% Saves an algo to database
%% --------------------------------------------------------------------
-spec save( algo() ) -> atom().
save(Algo) when is_record(Algo, algo) ->
    ?PRINT("Saving algo: ~p~n", [Algo]),
    F = fun() ->
              mnesia:write(Algo)
        end,
    mnesia:transaction(F).

%% --------------------------------------------------------------------
%% save_multiple/1
%%
%% Saves a list of algos to database within same transaction
%% --------------------------------------------------------------------
-spec save_multiple( [ algo() ] ) -> atom().
save_multiple([]) -> 
    ok;
save_multiple(L) ->
    F = fun() ->
              lists:foreach(fun mnesia:write/1, L)
        end,
    mnesia:transaction(F).


%% --------------------------------------------------------------------
%% remove/2
%%
%% Removes an algo from database
%% --------------------------------------------------------------------
-spec remove( algo(), integer() ) -> atom().
remove(Algo, AuthorizationLevel) when is_record(Algo, algo) ->
    remove(Algo#algo.ref, AuthorizationLevel);
remove(AlgoRef, AuthorizationLevel) ->
    F = fun() -> 
            case find_by_ref(AlgoRef, AuthorizationLevel) of
            [E | _L] ->
                {algo, AlgoRef} = E,
                mnesia:delete({algo, AlgoRef});
            [] -> algo_not_found
            end
        end,
    mnesia:transaction(F).

%% --------------------------------------------------------------------
%% find_by_ref/2
%%
%% Finds an algo, given its reference.
%% --------------------------------------------------------------------
-spec find_by_ref( coreRef(), integer() ) -> algo().
find_by_ref(CoreRef, AuthorizationLevel) when is_record(CoreRef, coreRef)->
    ?PRINT("Looking for algo: ~p~n", [CoreRef]), 
    F = fun() ->
               Q = qlc:q([Algo || Algo <- mnesia:table(algo),
                                  Algo#algo.ref =:= CoreRef,
                                  AuthorizationLevel >= Algo#algo.securityLevel]),
               qlc:e(Q)
            end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Why} = Failure ->
            throw({failed_to_find_algo_by_reference, Failure})
    end;
find_by_ref(VersionRef, AuthorizationLevel) when is_record(VersionRef, versionRef) ->
    InstanceId = VersionRef#versionRef.instanceId,
    AlgoId = VersionRef#versionRef.algoId,
    find_by_ref(#coreRef{instanceId = InstanceId, algoId = AlgoId}, AuthorizationLevel).
