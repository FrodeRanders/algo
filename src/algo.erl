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
-module(algo).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").

%%% Public interface
-export([establish_principal/2, establish_instance/2, register_instances/1, 
         
         define_namespace/2, expand_namespace/1, namespace_alias/1,
         define_attribute/2, define_attribute/3,
          
         new_algo/4, new_algo/5, get_algo/2,
         
         new_type/3, extend_type/4, extend_type/5,
         
         get_effective_version/1, get_effective_version/2,
         get_latest_version/1, get_latest_version/2, 
         get_next_version/1, get_next_version/2,
         get_all_versions/1, get_all_versions/2,
          
         add_version/2, replace_version/2,
         save/1, save_multiple_in_transaction/1, save_multiple/1,
         
         add_child/2, get_children/1]).

% debug(D) ->
%    io:format("Algo: ~p~n", [D]).

%%-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
-define(PRINT(S, A), true).

%% --------------------------------------------------------------------
%% establish_principal/2
%%
%% Establishes a principal within the system, for use in 
%% subsequent calls.
%% --------------------------------------------------------------------
-spec establish_principal( string(), integer() ) -> user().
establish_principal(UserId, AuthorizationLevel) ->
    #principal{userId=UserId, authorizationLevel=AuthorizationLevel}.


%% --------------------------------------------------------------------
%% establish_instance/2
%%
%% Establishes an instance with roles and general access
%% information for a specific instance, for use in subsequent
%% calls.
%% --------------------------------------------------------------------
-spec establish_instance( integer(), roleList() ) -> instance().
establish_instance(InstanceId, Roles) ->
    #instance{instanceId=InstanceId, roles=Roles}.


%% --------------------------------------------------------------------
%% register_instances/1
%%
%% Registers a set of established instances for use in
%% subsequent calls.
%% --------------------------------------------------------------------
-spec register_instances( [ instance() ] ) -> 'ok'.
register_instances(Instances) ->
    put(instances, Instances),
    ok.


%% --------------------------------------------------------------------
%% define_namespace/2
%%
%% Defines a namespace.
%%
%% Example:
%%    define_namespace("http://purl.org/dc/elements/1.1/", "dc")
%% --------------------------------------------------------------------
-spec define_namespace( string(), string() ) -> 'ok'.
define_namespace(QualifiedName, AliasName) ->
    algo_namespace:define(QualifiedName, AliasName).


%% --------------------------------------------------------------------
%% expand_namespace/1
%%
%% Expands a namespace, e.g.  
%% "dc" returns "http://purl.org/dc/elements/1.1/".
%% --------------------------------------------------------------------
-spec expand_namespace( string() ) -> 'nil' | string().
expand_namespace(AliasName) ->
    algo_namespace:expand(AliasName).


%% --------------------------------------------------------------------
%% namespace_alias/1
%%
%% Determines namespace alias, e.g.
%% "http://purl.org/dc/elements/1.1/" returns "dc".
%% --------------------------------------------------------------------
-spec namespace_alias( string() ) -> 'nil' | string().
namespace_alias(QualifiedName) ->
    algo_namespace:alias(QualifiedName).


%% --------------------------------------------------------------------
%% define_attribute/3
%%
%% Defines an attribute
%% Returns: Attribute
%% --------------------------------------------------------------------
-spec define_attribute( string(), string(), atom() ) -> {unknown_attribute_type, atom()} | {unknown_namespace, string()} | attribute().
define_attribute(Namespace, Name, Type) ->
    case algo_namespace:find_by_alias(Namespace) of
        [] ->
            {unknown_namespace, Namespace};
        [{namespace, ShortName, _QualifiedName}] ->
            % Define attribute within the specified namespace
            case algo_attributes:new(ShortName, Name, Type) of
                {ok, Attribute} = Result ->
                    algo_attributes:save(Attribute),
                    Result;
                E ->
                    E
            end
    end.


%% --------------------------------------------------------------------
%% define_attribute/2
%%
%% Defines an attribute by compound name, e.g. "dc:creator"
%% --------------------------------------------------------------------
-spec define_attribute( string(), atom() ) -> {unknown_attribute_type, atom()} | {unknown_namespace, string()} | {invalid_compound_name, string()} | attribute().
define_attribute(CompoundName, Type) ->
    ?PRINT("Defining attribute ~p of type ~p~n", [CompoundName, Type]),
    case string:tokens(CompoundName, ":") of
        [Namespace, Name] ->
            define_attribute(Namespace, Name, Type);
        _ ->
            {invalid_compound_name, CompoundName}
    end.


%% --------------------------------------------------------------------
%% replicate_version/1
%%
%% Replicates effective version from Type (referred to as a template)
%% --------------------------------------------------------------------
-spec replicate_version( algo() ) -> version().
replicate_version(Type) when is_record(Type, algo) ->
    % Copy attributes from the type effective version and save reference back to type
    {effective, Version} = algo_version:get_effective_version(Type),
    #version{typeRef = Type#algo.ref, attributes=Version#version.attributes}.


%% --------------------------------------------------------------------
%% new_algo/4
%%
%% Create a new algo of specified type (referred to as a template)
%% --------------------------------------------------------------------
-spec new_algo( integer(), atom(), algo(), user() ) -> algo().
new_algo(InstanceId, Class, Type, Owner) when is_record(Type, algo) ->
    ?PRINT("Creating new algo of class ~p from type ~p~n", [Class, Type#algo.ref]), 
    % Create a new "something"
    Algo = algo_core:new(InstanceId, Class),
    Algo1 = Algo#algo{owner = Owner#principal.userId, versionControlled = Type#algo.versionControlled},
    % Associate an initial version with algo, taken from the type.
    % We expect that no corresponding version already exists in Algo1.
    {Algo2, ReplacedVersion} = algo_version:replace_version(Algo1, replicate_version(Type)),
    synchronous_liquidate(ReplacedVersion),
    Algo2.


%% --------------------------------------------------------------------
%% new_algo/5
%%
%% Creates a new algo from reference to a type (referred to as a template).
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec new_algo( integer(), atom(), coreRef(), string(), user() ) -> {type_not_found, coreRef()} | {permission_denied, coreRef()} | algo().
new_algo(InstanceId, Class, TypeRef, Owner, User) when is_record(TypeRef, coreRef) ->
    ?PRINT("Creating new algo of class ~p from type ~p~n", [Class, TypeRef]), 
    % Look up type from TypeRef (which is why we need the User)
    case algo_core:find_by_ref(TypeRef, User#principal.authorizationLevel) of
        % We demand exactly one match on a type class         
        [Type] when Type#algo.class =:= type ->
            % Check access to base type
            case algo_access:may_access(Type, User) of
                false ->
                    {permission_denied, TypeRef};
                true ->
                    new_algo(InstanceId, Class, Type, Owner)
            end;
        _ ->
            {type_not_found, TypeRef}
    end.


%% --------------------------------------------------------------------
%% new_type/3
%%
%% Creates a new type (from scratch)
%% --------------------------------------------------------------------
-spec new_type( integer(), string(), string() ) -> algo().
new_type(InstanceId, Name, Owner) ->
    ?PRINT("Creating new type ~p~n", [Name]), 
    NewType = algo_core:new(InstanceId, type),
    NewType1 = NewType#algo{owner = Owner#principal.userId},
    % Set name of first associated version of type
    {initial, FirstVersion} = algo_version:get_initial_version(NewType1),
    % We expect that no corresponding version already exists in NewType1.
    {NewType2, ReplacedVersion} = algo_version:replace_version(NewType1, FirstVersion#version{name = Name}),
    synchronous_liquidate(ReplacedVersion),
    NewType2.

    
%% --------------------------------------------------------------------
%% extend_type/4
%%
%% Creates a new type by extending existing type.
%% --------------------------------------------------------------------
-spec extend_type( integer(), string(), algo(), string() ) -> algo().
extend_type(InstanceId, Name, Type, Owner) ->
    NewType = new_algo(InstanceId, type, Type, Owner),
    % Set name of first associated version of type
    {latest, FirstVersion} = algo_version:get_latest_version(NewType),
    {NewType1, ReplacedVersion} = algo_version:replace_version(NewType, FirstVersion#version{name = Name}),
    synchronous_liquidate(ReplacedVersion),
    NewType1.

    
%% --------------------------------------------------------------------
%% extend_type/5
%%
%% Creates a new type by extending existing type, identified
%% by reference to type.
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec extend_type( integer(), string(), coreRef(), string(), user() ) -> {type_not_found, coreRef()} | {permission_denied, coreRef()} | algo().
extend_type(InstanceId, Name, TypeRef, Owner, User) ->
    case algo_core:find_by_ref(TypeRef) of
        [Type] ->
            % Check access to base type
            case algo_access:may_access(Type, User) of
                false ->
                    {permission_denied, TypeRef};
                true ->
                    extend_type(InstanceId, Name, Type, Owner)
            end;
        _ ->
            {type_not_found, TypeRef}
    end.


%% --------------------------------------------------------------------
%% get_algo/2
%%
%% Gets algo, identified by reference, containing only the
%% effective version (i.e. a snapshot representation of algo).
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec get_algo( coreRef(), user() ) -> {permission_denied, coreRef()} | {algo_not_found, coreRef()} | snapshot().
get_algo(AlgoRef, User) when is_record(AlgoRef, coreRef) ->
    ?PRINT("Getting algo ~p~n", [AlgoRef]), 
    case algo_core:find_by_ref(AlgoRef, User#principal.authorizationLevel) of
        [Algo] ->
            % Check access to algo
            case algo_access:may_access(Algo, User) of
                false ->
                    {permission_denied, AlgoRef};
                true ->
                    {effective, Version} = get_effective_version(Algo),
                    list_to_tuple(simplify(tuple_to_list(Algo), [], Version))
            end;
        _ ->
            {algo_not_found, AlgoRef}
    end.


%% --------------------------------------------------------------------
%% simplify/3
%%
%% Recodes internal algo tuple to public snapshot tuple.
%% Will recode list of terms of an algo tuple ({algo, ref, ..., versionS})
%% to a list of terms of a snapshot tuple ({snapshot, ref, ..., version}).
%% --------------------------------------------------------------------
-spec simplify( [term()], [], version() ) -> [term()].
simplify([], [_H|T], Version) -> lists:reverse([Version|T]);
simplify([_H|T], [], Version) -> simplify(T, [snapshot], Version);
simplify([H|T], E, Version) -> simplify(T, [H|E], Version).


%% --------------------------------------------------------------------
%% get_effective_version/1
%%
%% Gets effective version of specified algo. This will be
%% the latest _released_ version of the algo -or- the latest
%% version if none has been marked as released.
%% --------------------------------------------------------------------
-spec get_effective_version( algo() ) -> {'effective', version()}.
get_effective_version(Algo) when is_record(Algo, algo) ->
    ?PRINT("Getting effective version of algo ~p~n", [Algo#algo.ref]), 
    algo_version:get_effective_version(Algo).


%% --------------------------------------------------------------------
%% get_effective_version/2
%%
%% Gets effective version of an algo from reference to algo.
%% This will be the latest _released_ version of the algo -or- 
%% the latest version if none has been marked as released.
%% This implies a database lookup and subsequent access verification!
%% --------------------------------------------------------------------
-spec get_effective_version( coreRef(), user() ) -> {'algo_not_found', coreRef()} | {'permission_denied', coreRef()} | {'effective', version()}.
get_effective_version(AlgoRef, User) when is_record(AlgoRef, coreRef) ->
    ?PRINT("Getting effective version of algo ~p~n", [AlgoRef]), 
    case algo_core:find_by_ref(AlgoRef, User#principal.authorizationLevel) of
        [Algo] ->
            % Check access to algo
            case algo_access:may_access(Algo, User) of
                false ->
                    {permission_denied, AlgoRef};
                true ->
                    get_effective_version(Algo)
            end;
        _ ->
            {algo_not_found, AlgoRef}
    end.


%% --------------------------------------------------------------------
%% get_latest_version/1
%%
%% Gets latest version (not necessarily the effective version!) of specified algo.
%% --------------------------------------------------------------------
-spec get_latest_version( algo() ) -> {'latest', version()}.
get_latest_version(Algo) when is_record(Algo, algo) ->
    ?PRINT("Getting latest version of algo ~p~n", [Algo#algo.ref]), 
    algo_version:get_latest_version(Algo).


%% --------------------------------------------------------------------
%% get_latest_version/2
%%
%% Gets latest version (not necessarily the effective version!)  of algo,
%% identified by reference.
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec get_latest_version( coreRef(), user() ) -> {'algo_not_found', coreRef()} | {'permission_denied', coreRef()} | {'latest', version()}.
get_latest_version(AlgoRef, User) when is_record(AlgoRef, coreRef) ->
    ?PRINT("Getting latest version of algo ~p~n", [AlgoRef]), 
    case algo_core:find_by_ref(AlgoRef, User#principal.authorizationLevel) of
        [Algo] ->
            % Check access to algo
            case algo_access:may_access(Algo, User) of
                false ->
                    {permission_denied, AlgoRef};
                true ->
                    get_latest_version(Algo)
            end;
        _ ->
            {algo_not_found, AlgoRef}
    end.


%% --------------------------------------------------------------------
%% get_next_version/1
%%
%% Creates a new version by copying the latest version
%% of specified algo.
%% --------------------------------------------------------------------
-spec get_next_version( algo() ) -> {'next', version()}.
get_next_version(Algo) when is_record(Algo, algo) ->
    ?PRINT("Getting next version of algo ~p~n", [Algo#algo.ref]), 
    algo_version:get_next_version(Algo).


%% --------------------------------------------------------------------
%% get_next_version/2
%%
%% Creates a new version by copying the lastest version
%% of specified algo, identified by reference.
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec get_next_version( coreRef(), user() ) -> {'algo_not_found', coreRef()} | {'permission_denied', coreRef()} | {'version_not_found', algo()} | {'next', version()}.
get_next_version(AlgoRef, User) when is_record(AlgoRef, coreRef) ->
    ?PRINT("Getting next version of algo ~p~n", [AlgoRef]), 
    case algo_core:find_by_ref(AlgoRef, User#principal.authorizationLevel) of
        [Algo] ->
            % Check access to algo
            case algo_access:may_access(Algo, User) of
                false ->
                    {permission_denied, AlgoRef};
                true ->
                    get_next_version(Algo)
            end;
        _ ->
            {algo_not_found, AlgoRef}
    end.


%% --------------------------------------------------------------------
%% get_all_versions/1
%%
%% Gets all versions of specified algo.
%% --------------------------------------------------------------------
-spec get_all_versions( algo() ) -> [version()].
get_all_versions(Algo) when is_record(Algo, algo) ->
    ?PRINT("Getting all versions of algo ~p~n", [Algo#algo.ref]), 
    Algo#algo.versions.


%% --------------------------------------------------------------------
%% get_all_versions/2
%%
%% Gets all versions of specified algo, identified by reference.
%% This implies a database lookup and subsequent access
%% verification!
%% --------------------------------------------------------------------
-spec get_all_versions( coreRef(), user() ) -> {'algo_not_found', coreRef()} | {'permission_denied', coreRef()} | [version()].
get_all_versions(AlgoRef, User) when is_record(AlgoRef, coreRef) ->
    ?PRINT("Getting all versions of algo ~p~n", [AlgoRef]), 
    case algo_core:find_by_ref(AlgoRef, User#principal.authorizationLevel) of
        [Algo] ->
            % Check access to algo
            case algo_access:may_access(Algo, User) of
                false ->
                    {permission_denied, AlgoRef};
                true ->
                    get_all_versions(Algo)
            end;
        _ ->
            {algo_not_found, AlgoRef}
    end.


%% --------------------------------------------------------------------
%% add_version/2
%%
%% Adds a new version to an algo.
%% --------------------------------------------------------------------
-spec add_version( algo(), version() ) -> algo().
add_version(Algo, Version) ->
    ?PRINT("Adding version to algo ~p~n", [Algo#algo.ref]), 
    algo_version:add_version(Algo, Version).


%% --------------------------------------------------------------------
%% replace_version/2
%%
%% Replaces the current version of an algo. Returns
%% modified algo and the version that has been replaced
%% (for liquidation if needed)
%% --------------------------------------------------------------------
-spec replace_version( algo(), version() ) -> { algo(), version() }.
replace_version(Algo, Version) ->
    ?PRINT("Replacing version of algo ~p~n", [Algo#algo.ref]), 
    algo_version:replace_version(Algo, Version).


%% --------------------------------------------------------------------
%% save/1
%%
%% Saves an algo
%% --------------------------------------------------------------------
-spec save( algo() ) -> 'ok' | {'algo_has_no_versions', algo()}.
save(Algo) when is_record(Algo, algo) ->
    ?PRINT("Saving algo ~p~n", [Algo#algo.ref]), 
    % TODO Just for now, skip control of versions in algo(s)
    % TODO We have to handle both snapshot and algo!
    case Algo#algo.versions of
        [] ->
            algo_has_no_versions;
        _ -> 
            algo_core:save(Algo)
    end.


%% --------------------------------------------------------------------
%% save_multiple_in_transaction/1
%%
%% Saves multiple algos in same transaction.
%% --------------------------------------------------------------------
-spec save_multiple_in_transaction( [algo()] ) -> 'ok' | {'algo_has_no_versons', algo()}.
save_multiple_in_transaction(AlgoList) when is_list(AlgoList) ->
    ?PRINT("Saving ~p algo(s) in same transaction~n", [lists:flatlength(AlgoList)]),
    % TODO Just for now, skip control of versions in algo(s)
    % TODO We have to handle both snapshot and algo!
    algo_core:save_multiple(AlgoList).


%% --------------------------------------------------------------------
%% save_multiple/1
%%
%% Saves multiple algos in sequence, i.e. in separate transactions.
%% --------------------------------------------------------------------
-spec save_multiple( [algo()] ) -> 'ok' | {'algo_has_no_versions', algo()}.
save_multiple(AlgoList) when is_list(AlgoList) ->
    ?PRINT("Saving ~p algo(s)~n", [lists:flatlength(AlgoList)]), 
    % TODO Just for now, skip control of versions in algo(s)
    % TODO We have to handle both snapshot and algo!
    lists:map(fun save/1, AlgoList).


%% --------------------------------------------------------------------
%% add_child/2
%%
%% Adds child to folder, making use of the 'parent-child' relationship.
%% --------------------------------------------------------------------
-spec add_child( algo(), algo() ) -> 'ok'.
add_child(ParentAlgo, ChildAlgo) ->
    algo_structure:relate(ParentAlgo, 'parent-child', ChildAlgo).


%% --------------------------------------------------------------------
%% get_children/1
%%
%% Gets children in folder, making use of the 'parent-child'
%% relationship.
%% --------------------------------------------------------------------
-spec get_children( algo() ) -> [ algo() ].
get_children(ParentAlgo) ->
    % TODO Should we simplify algos?
    algo_structure:relations_from(ParentAlgo, 'parent-child').


%% --------------------------------------------------------------------
%% synchronous_liquidate/1
%%
%% Liquidates a version or schedules a version for liquidation.
%% --------------------------------------------------------------------
-spec synchronous_liquidate( version ) -> 'ok'.
synchronous_liquidate(Version) when is_record(Version, version) ->
    ?PRINT("Liquidating version ~p~n", [Version]), 
    ok;
synchronous_liquidate(nil) ->
    ?PRINT("Ignoring liquidation~n", []), 
    ok;
synchronous_liquidate(_Version) ->
    ?PRINT("Can not liquidate unknown ~p~n", [_Version]), 
    ok.
