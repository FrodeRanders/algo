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
-module(algo_structure).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% Public interface
-export([relate/3, relations_from/2, relations_to/2, 
       associate/3, associations_from/2, associations_to/2]).

%%% -------------------------------------------------------------------
%%% Description of the structuring system.
%%%
%%% This system provides two types of connections between on one hand
%%% algos and on the other (external) entities and algos. A relation
%%% connects algos and an association connects (external) entities with
%%% algos.
%%%
%%% -------------------------------------------------------------------
%%% Definition of a relation.
%%%
%%% A relation defines a connection between pairs of algos. Since this
%%% connection is considered to be bidirectional, we will store pairs of 
%%% relations (denoted 'right-relation' and 'left-relation'). 
%%% Thus we will be able to follow the relation both ways. 
%%%
%%% The 'right-relation' of a specific type is one-to-many and 
%%% the 'left-relation' of a specific type is one-to-one. 
%%%
%%% A relation is neither version controlled nor access controlled,
%%% but the two individual algos addressed by the relation are both
%%% version controlled and access controlled.
%%%
%%% Common relations:
%%%   'parent-child' relation between a folder and a document in this folder.
%%%
%%% An algo may participate in multiple relations of the same type,
%%% with different algos. Thus a child algo may participate in multiple
%%% 'parent-child' relations, effectively meaning that any single algo may
%%% at the same time be located in multiple folders (just to pick an example).
%%%
%%% -------------------------------------------------------------------
%%% Definition of an association.
%%%
%%% An association defines a connection between any (external) entity and an
%%% algo. Since this connection is considered to be bidirectional, we will store
%%% pairs of associations (denoted 'right-association' and 'left-association'). 
%%% Thus we will be able to follow the association both ways. 
%%%
%%% The 'right-association' of a specific type is one-to-many and 
%%% the 'left-association' of a specific type is one-to-one. 
%%%
%%% An association is neither version controlled nor access controlled, 
%%% but the algo addressed by the assocation is both version controlled
%%% and access controlled. The (external) entity itself (which is not maintained
%%% by this system) will neither be version controlled nor access controlled
%%% by this system. It may well be version controlled or access controlled
%%% by means of an external system.
%%%
%%% The entity of an association may be anything that is identifyable through
%%% a string. Typically this could be a transaction id in a ledger, a
%%% person id (social insurance number) or in short anything that does not
%%% have to be modelled as an algo.
%%%
%%% -------------------------------------------------------------------
%%% Summary:
%%% Relations and associations are not version controlled!
%%% Relations and associations are not access controlled!
%%% ...but the individual algos are both version as well as access controlled.
%%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% relate/3
%%
%% Relates an algo with another algo.
%% Several types of relations exist, e.g. the 'parent-child'
%% relation that tracks documents stored in a folder.
%% --------------------------------------------------------------------
-spec relate( coreRef(), atom(), coreRef() ) -> {'atomic', 'ok'}.
relate(LeftRef, Type, RightRef) when is_record(LeftRef, coreRef), is_record(RightRef, coreRef) ->
	% We need to do all this in same transaction
	F = fun() ->
		case find_right_relations_by_type(LeftRef, Type) of
			[Relation] ->
				RightRefs = [RightRef | Relation#'right-relation'.rightRefs];
			[] ->
				RightRefs = [RightRef]
		end,
		RightRelation = #'right-relation'{leftRef = LeftRef, type = Type, rightRefs = RightRefs},
		mnesia:write(RightRelation),
		LeftRelation = #'left-relation'{rightRef = RightRef, type = Type, leftRef = LeftRef},
		mnesia:write(LeftRelation)
	end,
	mnesia:transaction(F);
relate(LeftAlgo, Type, RightAlgo) when is_record(LeftAlgo, algo), is_record(RightAlgo, algo) ->
	relate(LeftAlgo#algo.ref, Type, RightAlgo#algo.ref).


%% --------------------------------------------------------------------
%% relations_from/2
%%
%% Searches for relations of the specified type from an algo.
%% --------------------------------------------------------------------
-spec relations_from( coreRef(), atom() ) -> [ coreRef() ].
relations_from(AlgoRef, Type) when is_record(AlgoRef, coreRef) ->
	find_right_relations_by_type(AlgoRef, Type);
relations_from(Algo, Type) when is_record(Algo, algo) ->
	find_right_relations_by_type(Algo#algo.ref, Type).


%% --------------------------------------------------------------------
%% relations_to/2
%%
%% Searches for relations of the specified type to an algo.
%% --------------------------------------------------------------------
-spec relations_to( coreRef(), atom() ) -> [ coreRef() ].
relations_to(AlgoRef, Type) when is_record(AlgoRef, coreRef) ->
	find_left_relation_by_type(AlgoRef, Type);
relations_to(Algo, Type) when is_record(Algo, algo) ->
	find_left_relation_by_type(Algo#algo.ref, Type).


%% --------------------------------------------------------------------
%% associate/3
%%
%% Associates an entity with an algo.
%% Several types of associations may exist.
%% --------------------------------------------------------------------
-spec associate( string(), atom(), coreRef() ) -> { 'atomic', 'ok' }.
associate(Entity, Type, AlgoRef) when is_record(AlgoRef, coreRef) ->
	% We need to do all this in same transaction
	F = fun() ->
		case find_right_associations_by_type(Entity, Type) of
			[Association] ->
				Refs = [AlgoRef | Association#'right-association'.refs];
			[] ->
				Refs = [AlgoRef]
		end,
		RightAssociation = #'right-association'{entity = Entity, type = Type, refs = Refs},
		mnesia:write(RightAssociation),
		LeftAssociation = #'left-association'{ref = AlgoRef, type = Type, entity = Entity},
		mnesia:write(LeftAssociation)
	end,
	mnesia:transaction(F);
associate(Entity, Type, Algo) when is_record(Algo, algo) ->
	associate(Entity, Type, Algo#algo.ref).


%% --------------------------------------------------------------------
%% associations_from/2
%%
%% Searches for associations of the specified type from an entity.
%% --------------------------------------------------------------------
-spec associations_from( string(), atom() ) -> [ coreRef() ].
associations_from(Entity, Type) ->
	find_right_associations_by_type(Entity, Type).


%% --------------------------------------------------------------------
%% associations_to/2
%%
%% Searches for associations of the specified type to an algo.
%% --------------------------------------------------------------------
-spec associations_to( coreRef, atom() ) -> [ string() ].
associations_to(AlgoRef, Type) when is_record(AlgoRef, coreRef) ->
	find_left_association_by_type(AlgoRef, Type);
associations_to(Algo, Type) when is_record(Algo, algo) ->
	find_left_association_by_type(Algo#algo.ref, Type).


%% --------------------------------------------------------------------
%% find_right_relations_by_type/2
%%
%% Searches for left-to-right relations of specified type from an algo.
%% --------------------------------------------------------------------
-spec find_right_relations_by_type( coreRef(), atom() ) -> [ coreRef() ].
find_right_relations_by_type(AlgoRef, Type) ->
	F = fun() ->
              Q = qlc:q([Relation || Relation <- mnesia:table('right-relation'),
                       Relation#'right-relation'.leftRef =:= AlgoRef,
                    Relation#'right-relation'.type =:= Type]),
            qlc:e(Q)
       end,
	case mnesia:transaction(F) of
		{atomic, L} ->
			L;
		{aborted, _Why} = Failure ->
			throw({failed_to_find_right_relations_by_type, Failure})
	end.
	

%% --------------------------------------------------------------------
%% find_left_relation_by_type/2
%%
%% Searches for left-to-right relation of specified type from an algo.
%% --------------------------------------------------------------------
-spec find_left_relation_by_type( coreRef(), atom() ) -> [ coreRef() ].
find_left_relation_by_type(AlgoRef, Type) ->
	F = fun() ->
              Q = qlc:q([Relation || Relation <- mnesia:table('left-relation'),
                    Relation#'left-relation'.rightRef =:= AlgoRef,
                    Relation#'left-relation'.type =:= Type]),
            qlc:e(Q)
       end,
	case mnesia:transaction(F) of
		{atomic, L} ->
			L;
		{aborted, _Why} = Failure ->
			throw({failed_to_find_left_relation_by_type, Failure})
	end.


%% --------------------------------------------------------------------
%% find_right_associations_by_type/2
%%
%% Searches for left-to-right associations of specified type from an entity.
%% --------------------------------------------------------------------
-spec find_right_associations_by_type( string(), atom() ) -> [ coreRef() ].
find_right_associations_by_type(Entity, Type) ->
	F = fun() ->
              Q = qlc:q([Assoc || Assoc <- mnesia:table('right-association'),
                             Assoc#'right-association'.entity =:= Entity,
 								Assoc#'right-association'.type =:= Type]),
            qlc:e(Q)
       end,
	case mnesia:transaction(F) of
		{atomic, L} ->
			L;
		{aborted, _Why} = Failure ->
			throw({failed_to_find_right_associations_by_type, Failure})
	end.


%% --------------------------------------------------------------------
%% find_left_association_by_type/2
%%
%% Searches for left-to-right relation of specified type from an algo.
%% --------------------------------------------------------------------
-spec find_left_association_by_type( coreRef(), atom() ) -> [ string() ].
find_left_association_by_type(AlgoRef, Type) ->
	F = fun() ->
              Q = qlc:q([Assoc || Assoc <- mnesia:table('left-association'),
                             Assoc#'left-association'.ref =:= AlgoRef,
 								Assoc#'left-association'.type =:= Type]),
            qlc:e(Q)
        end,
	case mnesia:transaction(F) of
		{atomic, L} ->
			L;
		{aborted, _Why} = Failure ->
			throw({failed_to_find_left_association_by_type, Failure})
	end.
