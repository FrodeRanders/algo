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
-module(algo_access).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").

%%% Public interface
-export([may_access/2, may_modify/2, may_manage/2]).

%% --------------------------------------------------------------------
%% may_access/2
%%
%% Performs access control for a user towards an algo.
%% --------------------------------------------------------------------
-spec may_access( algo(), user() ) -> boolean().
may_access(Algo, User) ->
    if 
        Algo#algo.owner =:= User#principal.userId ->
            % Owner of Algo has every access possible
            true;
        true ->
            % All else must satisfy access rules based on
            % specific access (through ACL) or on general 
            % access (through roles).
            Ref = Algo#algo.ref,
            case Algo#algo.acl of
                [] ->
                    % Nothing specified in ACL, fall back on general access behaviour
                    has_general_read_access(Ref#coreRef.instanceId, User);
                Acl ->
                    % Study specific access rules in access control list
                    case check_acl(?READ_ACCESS_CHECK_PATTERN, Acl, User) of
                        no_access ->
                            % User explicitly does not have access to resource
                            false;
                        true -> 
                            true;
                        false ->
                            % We could not decide access based upon the ACL, so
                            % we fall back on the general access behaviour
                            has_general_read_access(Ref#coreRef.instanceId, User)
                    end
            end
    end.


%% --------------------------------------------------------------------
%% may_modify/2
%%
%% Performs modification control for a user towards an algo.
%% --------------------------------------------------------------------
-spec may_modify( algo(), user() ) -> boolean().
may_modify(Algo, User) ->
    if 
        Algo#algo.owner =:= User#principal.userId ->
            % Owner of Algo has every access possible
            true;
        true ->
            % All else must satisfy access rules based on
            % specific access (through ACL) or on general 
            % access (through roles).
            Ref = Algo#algo.ref,
            case Algo#algo.acl of
                [] ->
                    % Nothing specified in ACL, fall back on general access behaviour
                    has_general_write_access(Ref#coreRef.instanceId, User);
                Acl ->
                    % Study specific access rules in access control list
                    case check_acl(?WRITE_ACCESS_CHECK_PATTERN, Acl, User) of
                        no_access ->
                            % User explicitly does not have access to resource
                            false;
                        true -> 
                            true;
                        false ->
                            % We could not decide access based upon the ACL, so
                            % we fall back on the general access behaviour
                            has_general_write_access(Ref#coreRef.instanceId, User)
                    end
            end
    end.


%% --------------------------------------------------------------------
%% may_manage/2
%%
%% Performs management control for a user towards an algo.
%% --------------------------------------------------------------------
-spec may_manage( algo(), user() ) -> boolean().
may_manage(Algo, User) ->
    if 
        Algo#algo.owner =:= User#principal.userId ->
            % Owner of Algo has every access possible
            true;
        true ->
            % All else must satisfy access rules based on
            % specific access (through ACL) or on general 
            % access (through roles).
            Ref = Algo#algo.ref,
            case Algo#algo.acl of
                [] ->
                    % Nothing specified in ACL, fall back on general access behaviour
                    has_general_management_access(Ref#coreRef.instanceId, User);
                Acl ->
                    % Study specific access rules in access control list
                    case check_acl(?MANAGEMENT_ACCESS_CHECK_PATTERN, Acl, User) of
                        no_access ->
                            % User explicitly does not have access to resource
                            false;
                        true -> 
                            true;
                        false ->
                            % We could not decide access based upon the ACL, so
                            % we fall back on the general access behaviour
                            has_general_management_access(Ref#coreRef.instanceId, User)
                    end
            end
    end.


%% --------------------------------------------------------------------
%% has_general_read_access/2
%%
%% Checks if user has general read access, based upon
%% the set of roles the user participates in for a specified
%% instance.
%% --------------------------------------------------------------------
-spec has_general_read_access( integer(), user() ) -> boolean().
has_general_read_access(_InstanceId, _User) ->
    true. % TODO Just for now, assume everybody has read access to all resources


%% --------------------------------------------------------------------
%% has_general_write_access/2
%%
%% Checks if user has general write access, based upon
%% the set of roles the user participates in for a specified
%% instance.
%% --------------------------------------------------------------------
-spec has_general_write_access( integer(), user() ) -> boolean().
has_general_write_access(_InstanceId, _User) ->
    false. % TODO Just for now, assume only owner has write access to resources


%% --------------------------------------------------------------------
%% has_general_management_access/2
%%
%% Checks if user has general management access, based upon
%% the set of roles the user participates in for a specified
%% instance.
%% --------------------------------------------------------------------
-spec has_general_management_access(integer(), user()) -> boolean().
has_general_management_access(_InstanceId, _User) ->
    false. % TODO Just for now, assume only owner has management access to resources


%% --------------------------------------------------------------------
%% check_acl/3
%%
%% Checks if user has specified access, based upon
%% an access control list.
%% --------------------------------------------------------------------
-spec check_acl( integer(), acl(), user() ) -> 'no_access' | boolean().
check_acl(AccessRequest, [Ace | L], User) ->
    case check_ace(AccessRequest, Ace, User) of
        no_access -> no_access;
        true -> true;
        false -> check_acl(AccessRequest, L, User)
    end;
check_acl(_AccessRequest, [], _User) -> 
    false.


%% --------------------------------------------------------------------
%% check_ace/3
%%
%% Checks if user has specific access to resource, 
%% based upon a specific access control entry. Individual
%% entries in the access control list may apply to an
%% individual user, to a role that the user may participate
%% in or to a group that the user has membership of.
%% --------------------------------------------------------------------
-spec check_ace( integer(), ace(), user() ) -> 'no_access' | boolean().
check_ace(AccessRequest, {ace, Entity, Type, Right}, User) ->
    % Match access on user, role participation or group membership.
    % An explicit matching ?NO_ACCESS shall block access for this
    % user!
    case Type of
        user when Entity =:= User#principal.userId ->
            if
                Right =:= ?NO_ACCESS ->
                    no_access;
                true ->
                    Right band AccessRequest
            end;
        role ->
            case lists:member(Entity, User#principal.roleParticipation) of
                false ->
                    % ACE does not apply to this user's roles
                    false;
                true ->
                    if
                        Right =:= ?NO_ACCESS ->
                            no_access;
                        true ->
                            Right band AccessRequest
                    end
            end;
        group ->
            case lists:member(Entity, User#principal.groupMembership) of
                false ->
                    % ACE does not apply to this user's groups
                    false;
                true ->
                    if
                        Right =:= ?NO_ACCESS ->
                            no_access;
                        true ->
                            Right band AccessRequest
                    end
            end;
        _ ->
            false
    end.
