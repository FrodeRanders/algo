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
-author('Frode Randers').

-ifndef(_ALGO_HRL).
-define(_ALGO_HRL, true).


%%-------------------------------------------------------------------
%% Various concepts
%%-------------------------------------------------------------------

%%
%% A timestamp with granularity in seconds
%%
-type timestamp() :: { erlang:date(), erlang:time() }.
-record(timestamp, {
            date = erlang:date(),
            time = erlang:time()
        }).


%%-------------------------------------------------------------------
%% Reference types.
%%
%% When refering to algo objects, we either use the version specific form
%% "<instanceId>.<algoId>v<versionId>" or the implicit effective version
%% form "<instanceId>.<algoId>".
%%-------------------------------------------------------------------

%%
%% A version-independent algo reference, refering to the effective version.
%%
-type coreRef() :: { integer(), string() }.
-record(coreRef, {
            instanceId,
            algoId
        }).
        
%%
%% A version-dependent algo reference, refering to a specific version.
%%
-type versionRef() :: { integer(), string(), integer() }.
-record(versionRef, {
            instanceId,
            algoId,
            versionId
        }).


%%-------------------------------------------------------------------
%% Access and security control features
%%-------------------------------------------------------------------

%%
%% Rights
%%
-define(         NO_ACCESS, 2#0000 ). % no access whatsoever
-define(       READ_ACCESS, 2#0001 ). % may access resource
-define(      WRITE_ACCESS, 2#0010 ). % may modify resource
-define( MANAGEMENT_ACCESS, 2#0100 ). % may modify access properties of resource

%%
%% Access level patterns, used to check rights.
%%
-define(       READ_ACCESS_CHECK_PATTERN, 2#0001 ). 
-define(      WRITE_ACCESS_CHECK_PATTERN, 2#0011 ). % includes read access
-define( MANAGEMENT_ACCESS_CHECK_PATTERN, 2#0111 ). % includes read and write access

%%
%% Security level
%%
-define( UNRESTRICTED, 0 ). % no security restriction

%% 
%% Access control entry.
%%
%% - A user may belong to a group. 
%% - A user may participate in a role. 
%% - A group may participate in a role.
%%
-type ace() :: { string(), atom(), string() }.
-record(ace, {
            entity, 
            type,  %% user, role, group, ...
            right
        }).


%%-------------------------------------------------------------------
%% Attributes.
%%
%% Attributes are defined in namespaces.
%%-------------------------------------------------------------------

%%
%% Currently namespaces are defined globally to the installation.
%%
-type namespace() :: { string(), string() }.
-record(namespace, {
            aliasName,     %% "dc"
            qualifiedName  %% "http://purl.org/dc/elements/1.1/"
        }).

%%
%% An attribute of any type, with any name in any namespace,
%% which may hold multiple values
%%
-type stringList() :: [ string() ].
-type attribute() :: { string(), string(), atom(), stringList() }.
-record(attribute, {
            namespace,
            name,
            type = any,
            values = [] % first value first in list
        }).


%%-------------------------------------------------------------------
%% An algo is a generic entity which is used to model
%% documents, messages, containers, types and the like.
%% An algo is (optionally) version controlled, but always
%% access and security controlled. 
%%-------------------------------------------------------------------

%%
%% This defines the version specific data of an algo.
%%
-type attributeList() :: [ string() ]. % an approximation
-type version() :: { integer(), atom(), string(), atom(), string(), timestamp(), string(), attributeList() }.
-record(version, {
            versionId = 1,
            versionState = preliminary, % preliminary, released, retained
            versionName,
            typeRef = typeless,
            name = "", % does not need to have a name
            modified = #timestamp{},
            modifier,
            attributes = [] % first attribute first in list
        }).

%%
%% This defines the internal storage format for an algo
%% and constitutes version-independent data.
%%
%% E.g. ownership is independent of version.
%% 
-type acl() :: [ ace() ].
-type versionList() :: [ version() ].
-type algo() :: { coreRef(), integer(), atom(), string(), atom(), timestamp(), boolean(), string(), acl(), versionList() }.
-record(algo, {
            ref = #coreRef{},  % primary key
            securityLevel = ?UNRESTRICTED,
            visibility = public,
            class,
            lifeCycleStatus = created,
            created = #timestamp{},
            versionControlled = true,
            owner,
            acl = [], % last ACE first in list
            versions = [] % last version first in list
        }).

%%
%% A public and portable format for an algo, containing version-independent
%% data together with version-dependent data _for_a_single_version_. 
%%
-type snapshot() :: { coreRef(), integer(), atom(), string(), atom(), timestamp(), boolean(), string(), acl(), version() }.
-record(snapshot, {
            ref = #coreRef{},  % primary key
            securityLevel = ?UNRESTRICTED,
            visibility = public,
            class,
            lifeCycleStatus = created,
            created = #timestamp{},
            versionControlled = true,
            owner,
            acl = [], % last ACE first in list
            version = #version{} % any version
        }).


%%-------------------------------------------------------------------
%% Users, roles and groups
%%-------------------------------------------------------------------

%%
%% Models a using entity, typically a user
%%
-type roleList() :: [ string() ]. % an approximation
-type groupList() :: [ string() ]. % an approximation
-type principal() :: { string(), integer(), roleList(), groupList() }.
-record(principal, {
            userId,
            authorizationLevel = ?UNRESTRICTED,
            roleParticipation = [],
            groupMembership = []
        }).

%%
%% Models roles for an instance together with general access information
%%
-type instance() :: { integer(), roleList() }.
-record(instance, {
            instanceId,
            roles = []
        }).


%%
%% A user
%%
-type user() :: { string() }.
-record(user, {
            userId
        }).

%%-------------------------------------------------------------------
%% Relations ('parent-child', ...) among algos and associations
%% between external entities and algos.
%%
%% Relations are stored in "pairs"; a right-to-left relation implies
%% (possibly) multiple left-to-right relations. The system will take
%% care of the reflective left-to-right relations, so you only have
%% to define the right-to-left relation yourself.
%%
%% The same goes for associations.
%%
%%-------------------------------------------------------------------

%%
%% Models a right-to-left relation, which is a one-to-many relation.
%%
-type coreRefList() :: [ coreRef() ].
-type 'right-relation'() :: { coreRef(), atom(), coreRefList() }.
-record('right-relation', {
            leftRef = #coreRef{},  % primary key
            type = 'parent-child',
            rightRefs = []
        }).

%%
%% Models a left-to-right relation, which is a one-to-one relation.
%%
-type 'left-relation'() :: { coreRef(), atom(), coreRef() }.
-record('left-relation', {
            rightRef = #coreRef{},  % primary key
            type = 'parent-child',
            leftRef = #coreRef{}
        }).

%%
%% Models a right-to-left association, which is a one-to-many association.
%%
-type 'right-association'() :: { string(), atom(), stringList() }.
-record('right-association', {
            entity = "",  % primary key
            type,
            refs = []
        }).

%%
%% Models a left-to-right association, which is a one-to-one association.
%%
-type 'left-association'() :: { coreRef(), atom(), string() }.
-record('left-association', {
            ref = #coreRef{},  % primary key
            type,
            entity = ""
        }).


%%-------------------------------------------------------------------
%% Resources
%%-------------------------------------------------------------------

%%
%% A single resource, being a file, a blob, a URL, ...
%%
-type resource() :: { string(), atom(), string(), integer() }.
-record(resource, {
            resourceId,
            type = file,
            mimeType = "application/octet-stream",
            size = 0
        }).


-endif.
