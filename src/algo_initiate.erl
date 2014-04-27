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
-module(algo_initiate).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% Maintenace interface
-export([init/0, init/1]).

%% --------------------------------------------------------------------
%% init/0
%%
%% Initiates the current Algo node
%% --------------------------------------------------------------------
-spec init() -> { 'atomic', 'ok'} | 'nil'.
init() ->
    init([node()]).

%% --------------------------------------------------------------------
%% init/1
%%
%% Initiates the specified set of Algo nodes
%% --------------------------------------------------------------------
-spec init( [node()] ) -> { 'atomic', 'ok'} | 'nil'.
init(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),

    % Create namespace table
    mnesia:create_table(namespace, 
                        [{type, set}, {disc_copies, Nodes}, 
                         {attributes, record_info(fields, namespace)}]),
                         
    % Populate with some common namespaces
    algo_namespace:define("http://klopp.se/ns/default/", ""),  % default namespace (none)
    algo_namespace:define("http://purl.org/dc/elements/1.1/", "dc"),

    % Create attribute table
    mnesia:create_table(attribute, 
                        [{type, set}, {disc_copies, Nodes}, 
                         {attributes, record_info(fields, attribute)}]),
                         
    % Create algo table
    mnesia:create_table(algo, 
                        [{type, set}, {disc_copies, Nodes}, 
                         {attributes, record_info(fields, algo)}]),

    % Create left->right and right->left relation tables
    mnesia:create_table('right-relation',
                        [{type, set}, {disc_copies, Nodes},
                         {attributes, record_info(fields, 'right-relation')}]),

    mnesia:create_table('left-relation',
                        [{type, set}, {disc_copies, Nodes},
                         {attributes, record_info(fields, 'left-relation')}]),

    % Create left->right and right->left association tables
    mnesia:create_table('right-association',
                        [{type, set}, {disc_copies, Nodes},
                         {attributes, record_info(fields, 'right-association')}]),

    mnesia:create_table('left-association',
                        [{type, set}, {disc_copies, Nodes},
                         {attributes, record_info(fields, 'left-association')}]).

