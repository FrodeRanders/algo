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
-module(algo_namespace).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% Public interface
-export([define/2, delete/1, expand/1, alias/1, find_by_alias/1, find_by_qualified_name/1]).

%%% Inter module interface


%% --------------------------------------------------------------------
%% define/2
%%
%% Defines a namespace
%% --------------------------------------------------------------------
-spec define( string(), string() ) -> { 'namespace_bound_to_other_alias', string() } | {'namespace_alias_already_exists', string() } | { 'atomic', 'ok' }.
define(QualifiedName, AliasName) -> insert(QualifiedName, AliasName).


%% --------------------------------------------------------------------
%% insert/2
%%
%% Inserts a namespace into the namespace database
%% --------------------------------------------------------------------
-spec insert( string(), string() ) -> { 'namespace_bound_to_other_alias', string() } | {'namespace_alias_already_exists', string() } | { 'atomic', 'ok' }.
insert(QualifiedName, AliasName) ->
    case find_by_alias(AliasName) of
        [] ->
            % No namespace defined for this alias yet
            case find_by_qualified_name(QualifiedName) of
                [] ->
                    % No qualified namespace defined yet
                    Namespace = #namespace{aliasName = AliasName, qualifiedName = QualifiedName},
                    F = fun() ->
                        mnesia:write(Namespace)
                    end,
                    mnesia:transaction(F);
                [Namespace] ->
                    {namespace_bound_to_other_alias, Namespace}
            end;
        [Namespace] ->
            {namespace_alias_already_exists, Namespace}
    end.


%% --------------------------------------------------------------------
%% delete/1
%%
%% Deletes a namespace (through it's alias) from the database
%% --------------------------------------------------------------------
-spec delete( string() ) -> atom().
delete(AliasName) ->
    % Implicitly using nested transactions     
    F = fun() -> 
            case find_by_alias(AliasName) of
            [] -> unknown_namespace;
            [Ns | _L] ->
                {namespace, AliasName, _QualifiedName} = Ns,
                mnesia:delete({namespace, AliasName})
            end
        end,
    mnesia:transaction(F).

    
%% --------------------------------------------------------------------
%% expand/1
%%
%% Expands an alias into a qualified name.
%% --------------------------------------------------------------------
-spec expand( string() ) -> string() | 'nil'.
expand(AliasName) ->
    case find_by_alias(AliasName) of
    [Ns | _L] ->
        {namespace, AliasName, QualifiedName} = Ns,
        QualifiedName;
    [] -> []
    end.


%% --------------------------------------------------------------------
%% alias/1
%%
%% Contracts a qualified namespace name to it's alias.
%% --------------------------------------------------------------------
-spec alias( string() ) -> string() | 'nil'.
alias(QualifiedName) ->
    case find_by_qualified_name(QualifiedName) of
    [Ns | _L] ->
        {namespace, AliasName, QualifiedName} = Ns,
        AliasName;
    [] -> []
    end.

    
%% --------------------------------------------------------------------
%% find_by_alias/1
%%
%% Searches for a namespace given it's alias name.
%% --------------------------------------------------------------------
-spec find_by_alias( string() ) -> string().
find_by_alias(AliasName) ->
    F = fun() ->
               Q = qlc:q([Ns || Ns <- mnesia:table(namespace),
                                Ns#namespace.aliasName =:= AliasName]),
               qlc:e(Q)
        end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Why} = Failure ->
            throw({failed_to_find_namespace_by_alias, Failure})
    end.


%% --------------------------------------------------------------------
%% find_by_qualified_name/1
%%
%% Searches for a namespace given it's qualified name.
%% --------------------------------------------------------------------
-spec find_by_qualified_name( string() ) -> string().
find_by_qualified_name(QualifiedName) ->
    F = fun() ->
               Q = qlc:q([Ns || Ns <- mnesia:table(namespace),
                                Ns#namespace.qualifiedName =:= QualifiedName]),
               qlc:e(Q)
        end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Why} = Failure ->
            throw({failed_to_find_namespace_by_qualified_name, Failure})
    end.

