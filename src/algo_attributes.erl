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
-module(algo_attributes).
-author('Frode Randers').

-include("algo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% Public interface
-export([new/3, get_all/0, find_by_name/2, save/1]).

%%% Inter module interface

-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
%%-define(PRINT(S, A), true).

%% --------------------------------------------------------------------
%% Creates typed attributes.
%% --------------------------------------------------------------------
-spec new(atom()) -> { atom(), attribute() }.
new(integer) -> {ok, #attribute{type = integer}};
new(string) -> {ok, #attribute{type = string}};
new(date) -> {ok, #attribute{type = date}};
new(boolean) -> {ok, #attribute{type = boolean}};
new(reference) -> {ok, #attribute{type = reference}};
new(_Type) -> unknown_attribute_type.

%% --------------------------------------------------------------------
%% new/3
%%
%% Creates a named attribute of specified type.
%% --------------------------------------------------------------------
-spec new( string(), string(), atom() ) -> { atom(), atom() }.
new(Namespace, Name, Type) ->
    case find_by_name(Namespace, Name) of
        [] ->
            % Attribute does not exist already, go ahead
            case new(Type) of
                {ok, Attribute} -> 
                    {ok , Attribute#attribute{namespace=Namespace, name=Name, type=Type}};
                E -> {E, Type}
            end;
        [Attribute] when Attribute#attribute.type =:= Type ->
            % Attribute exists with identical type, treat as successful creation
            {ok, Attribute};
        E ->
            % Attribute exists but with other type 
            {attribute_exists_with_diffrent_type, E}
    end.


%% --------------------------------------------------------------------
%% save/1
%%
%% Saves an attribute among available attributes. 
%% We demand uniqueness on the combination of namespace and name.
%% --------------------------------------------------------------------
-spec save( attribute() ) -> atom().
save(Attribute) ->
    ?PRINT("Saving attribute: ~p~n", [Attribute]), 
    F = fun() ->
              mnesia:write(Attribute)
        end,
    mnesia:transaction(F).


%% --------------------------------------------------------------------
%% get_all/0
%%
%% Gets a list of all registered attributes
%% --------------------------------------------------------------------
-spec get_all() -> [ attribute() ].
get_all() ->
    F = fun() ->
               Q = qlc:q([Attribute || Attribute <- mnesia:table(attribute)]),
               qlc:e(Q)
            end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Why} = Failure ->
            throw({failed_to_retrieve_all_attributes, Failure})
    end.


%% --------------------------------------------------------------------
%% find_by_name/2
%%
%% Find an attribute by (qualified) name.
%% --------------------------------------------------------------------
-spec find_by_name( string(), string() ) -> [ attribute() ].
find_by_name(Namespace, Name) ->
    ?PRINT("Looking for attribute matching ~p~n", [{Namespace, Name}]), 
    F = fun() ->
               Q = qlc:q([Attribute || Attribute <- mnesia:table(attribute),
                                  Attribute#attribute.namespace =:= Namespace,
                                  Attribute#attribute.name =:= Name]),
               qlc:e(Q)
            end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Why} = Failure ->
            throw({failed_to_find_attribute_by_name, Failure})
    end.
