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
-module(algo_version).
-author('Frode Randers').

-compile([strict_record_tests]).
-include("algo.hrl").

%%% Public interface
-export([get_initial_version/1, get_next_version/1,
         get_effective_version/1, get_latest_version/1, 
         add_version/2, replace_version/2, consolidate/1]).

%%% Internal interface
-export([assert_version_structure/1]).

-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
%%-define(PRINT(S, A), true).

%%% -------------------------------------------------------------------
%%% Description of the versioning system.
%%%
%%% The version list is kept such that the 'effective version' always
%%% comes first in the list and the arrangements of maintaining the
%%% effective version first is done when adding a new version - never
%%% when retrieving the version.
%%%
%%% By 'effective version' is meant, by example:
%%%
%%% a. Suppose you have two preliminary versions and no released
%%%    version yet. In this case PA2, the latest preliminary version,
%%%    is the effective version
%%%
%%%       PA2 <- PA1        
%%%
%%% b. Suppose you have a released version, A, with two preceeding
%%%    preliminary versions PA1 to PA2. In this case A is the effective
%%%    version.
%%%
%%%       A <- PA2 <- PA1
%%%
%%% c. Suppose you have a released version A, with two preceeding
%%%    preliminary versions PA1 to PA2 and with two subsequent
%%%    preliminiary version PB1 to PB2. In this case A is still
%%%    the effective version.
%%%
%%%       A <- PB2 <- PB1 <- PA2 <- PA1
%%%
%%% d. Suppose you have a released version B, with two preceeding
%%%    preliminary versions PB1 to PB2 and with a preceeding
%%%    released version A. In this case B is the effective version.
%%%
%%%       B <- PB2 <- PB1 <- A <- PA2 <- PA1
%%%
%%%
%%% Thus;
%%% 1. The version most probably retrieved is always stored first.
%%%
%%% 2. All versions must have unique and increasing versionId's. They
%%%    do not have to be in a strict incremental sequence.
%%%
%%% 3. Generally the latest version (highest versionId) is stored first
%%%    in the list and the list is sorted on decreasing versionId.
%%%
%%% 4. If there are no elements with versionState == released in the
%%%    list, the whole list shall be adequately sorted in decreasing
%%%    order on versionId.
%%%
%%% 5. The first element does not have to be the latest version if it
%%%    has versionState == released, in which case no other released
%%%    versions with higher versionId must appear anywhere in the list.
%%%
%%% The function assert_version_structure/1 verifies these rules and
%%% could be applied to the version list during (later) development to 
%%% obtain built-in regression tests.
%%% -------------------------------------------------------------------


%% --------------------------------------------------------------------
%% get_effective_version/1
%%
%% Gets effective version of an algo. This will be the
%% latest _released_ version of the algo -or- the latest
%% version if none has been marked as released.
%% --------------------------------------------------------------------
-spec get_effective_version( algo() ) -> { 'effective', version() }.
get_effective_version(Algo) when is_record(Algo, algo) ->
    {effective, hd(Algo#algo.versions)}.


%% --------------------------------------------------------------------
%% get_latest_version/1
%%
%% Gets latest version of an algo.
%% --------------------------------------------------------------------
-spec get_latest_version( algo() ) -> { 'latest', version() }.
get_latest_version(Algo) when is_record(Algo, algo) ->
    % We need to operate on a normalized version list
    case Algo#algo.versions of
        [Version1, Version2 | _Tail] when Version1#version.versionState =:= released,
                                          Version2#version.versionId > Version1#version.versionId ->
            % In this case we know that the list [Version2 | Tail] is
            % normalized (see add_unconditionally/2), so Version2 is
            % the latest version
            {latest, Version2};
        [Version1, _Version2 | _Tail] when Version1#version.versionState =/= released ->
            % In this case we know that the whole list is
            % normalized (see add_conditionally/2), so Version1 is
            % the latest version
            {latest, Version1};
        [Version1 | _Tail] ->
            {latest, Version1};
        [] ->
            {latest, []}
    end.


%% --------------------------------------------------------------------
%% get_next_version/1
%%
%% Creates a new version by copying the latest version
%% --------------------------------------------------------------------
-spec get_next_version( algo() ) -> { 'version_not_found', algo() } | { 'next', version() }.
get_next_version(Algo) when is_record(Algo, algo) ->
    case get_latest_version(Algo) of
        {latest, Version} ->
            {next, incr_version(Version)};
        _ ->
            {version_not_found, Algo}
    end.

%% --------------------------------------------------------------------
%% get_initial_version/1
%%
%% Creates the initial version (from scratch so to say).
%% --------------------------------------------------------------------
-spec get_initial_version( algo() ) -> { 'versions_already_exists', algo() } | { 'initial', version() }.
get_initial_version(Algo) when is_record(Algo, algo) ->
    case get_latest_version(Algo) of
        {latest, []} ->
            {initial, #version{versionId = 1}}; 
        _ ->
            {versions_already_exists, Algo}
    end.


%% --------------------------------------------------------------------
%% replace_version/2
%%
%% Replaces a version in an algo.
%% --------------------------------------------------------------------
-spec replace_version( algo(), version() ) -> { algo(), 'nil' } | { algo(), version() }.
replace_version(Algo, Version) ->
    ?PRINT("Replacing version ~p among versions ~p~n", [Version, Algo#algo.versions]),
    case Algo#algo.versions of
        [] ->
            {Algo#algo{versions = [Version]}, nil};
        Versions ->
            case lists:keysearch(Version#version.versionId, 2, Versions) of
                {value, OldVersion} ->
                    {Algo#algo{versions = lists:keyreplace(Version#version.versionId, 2, Versions, Version)}, OldVersion};
                false ->
                    {add_version(Algo, Version), nil}
            end
    end.

%% --------------------------------------------------------------------
%% consolidate/1
%%
%% Consolidates an algo, i.e. keep only the last effective
%% version returning all the other versions (or attributes)  
%% (to be subsequently dissolved).
%% --------------------------------------------------------------------
-spec consolidate( algo() ) -> [_].
consolidate(_Algo) ->
    % Not yet implemented
    [].
    
%% --------------------------------------------------------------------
%% incr_version/1
%%
%% Creates a new version with increased versionId
%% --------------------------------------------------------------------
-spec incr_version( version() ) -> version().
incr_version(Version) -> 
    CurrentVersion = Version#version.versionId,
    Version#version{versionId = CurrentVersion + 1}.
    

%% --------------------------------------------------------------------
%% add_version/2
%%
%% Adds a new version to an algo, honouring the version
%% status; work-in-progress, released, ...
%% --------------------------------------------------------------------
-spec add_version( algo(), version() ) -> algo().
add_version(Algo, Version) ->
    ?PRINT("Adding version ~p to algo ~p~n", [Version#version.versionId, Algo#algo.ref]),
    case Version#version.versionState of
        released ->
            L = add_unconditionally(Version, Algo#algo.versions),
            Algo#algo{versions = L};
        _ ->
            L = add_conditionally(Version, Algo#algo.versions),
            Algo#algo{versions = L}
    end.


%% --------------------------------------------------------------------
%% compare_versions/2
%%
%% Compares versions in such a way that later versions
%% are kept first in list, sorted on descending versionId.
%% --------------------------------------------------------------------
-spec compare_versions( version(), version() ) -> boolean().
compare_versions(V1, V2) ->
    V1#version.versionId >= V2#version.versionId.


%% --------------------------------------------------------------------
%% add_unconditionally/2
%%
%% Unconditionally adds version to front of versions list.
%% The current list of versions may not be normalized, i.e.
%% may not contain the latest version first, so we will
%% normalize it before adding the new version.
%% The latest released version is always stored first in
%% the versions list.
%% --------------------------------------------------------------------
-spec add_unconditionally( version(), versionList() ) -> versionList().
add_unconditionally(Version, VersionList) ->
    ?PRINT("Unconditionally adding version ~p to ~p versions~n", [Version#version.versionId, lists:flatlength(VersionList)]), 
    NormalizedVersions = lists:sort(fun compare_versions/2, VersionList),
    [Version | NormalizedVersions].
    

%% --------------------------------------------------------------------
%% add_conditionally/2
%%
%% Conditionally adds version to versions list. Since the
%% version we are adding is not 'released' (merely 
%% 'work_in_progress' or the like) we must handle an
%% occasional 'released' version - keeping it first in
%% the list and adding the new version behind it.
%% --------------------------------------------------------------------
-spec add_conditionally( version(), versionList() ) -> versionList().
add_conditionally(Version, [H|Tail] = VersionList) ->
    ?PRINT("Conditionally adding version ~p to ~p versions~n", [Version#version.versionId, lists:flatlength(VersionList)]), 
    case H#version.versionState of
        released -> 
            [H, Version | Tail];
        _ -> 
            NormalizedVersions = lists:sort(fun compare_versions/2, VersionList),
            [Version | NormalizedVersions]
    end;
add_conditionally(Version, []) -> [Version].


%% --------------------------------------------------------------------
%% assert_version_structure/1
%%
%% Asserts version list order.
%% --------------------------------------------------------------------
-spec assert_version_structure( versionList() ) -> 'ok'.
assert_version_structure([Head|Tail] = _VersionList) ->
    case Head#version.versionState of
        released ->
            assert_later_version(Tail, Head#version.versionId, nil);
        _ ->
            assert_later_version(Tail, nil, Head#version.versionId)
    end;
assert_version_structure([]) -> ok.

-spec assert_later_version( versionList(), integer(), integer() ) -> 'ok'.
assert_later_version([Head|Tail] = _VersionList, nil, PreviousVersionId) ->
    if 
        % Assert that the version of Head is less than that of the previous
        % element
        Head#version.versionId > PreviousVersionId ->
            throw({latest_preliminary_version_not_effective});

        Head#version.versionId =:= PreviousVersionId ->
            throw({list_contains_duplicate_versions});
        
        % Assert that we do not find any released version among the
        % later elements in the list
        Head#version.versionState =:= released -> 
            throw({released_version_not_effective});

        % Normal case
        Head#version.versionId < PreviousVersionId -> 
            assert_later_version(Tail, nil, Head#version.versionId)
    end;
assert_later_version([Head|Tail] = _VersionList, ReleasedVersionId, nil) ->
    % Assert against the released version only since the previous
    % element may have a smaller versionId than that of Head.
    case Head#version.versionState of
        released ->
            if
                Head#version.versionId > ReleasedVersionId ->
                    throw({latest_released_version_not_effective});
                
                Head#version.versionId =:= ReleasedVersionId ->
                    throw({list_contains_duplicate_versions});
                
                % Normal case
                Head#version.versionId < ReleasedVersionId ->
                    assert_later_version(Tail, ReleasedVersionId, Head#version.versionId)
            end;
        _ ->
            assert_later_version(Tail, ReleasedVersionId, Head#version.versionId)
    end;
assert_later_version([Head|Tail] = _VersionList, ReleasedVersionId, PreviousVersionId) ->
    case Head#version.versionState of
        released ->
            if
                Head#version.versionId >= ReleasedVersionId ->
                    throw({latest_released_version_not_effective});

                Head#version.versionId =:= ReleasedVersionId ->
                    throw({list_contains_duplicate_versions});
                
                % Normal case
                Head#version.versionId < ReleasedVersionId ->
                    assert_later_version(Tail, ReleasedVersionId, Head#version.versionId)
            end;
        _ ->
            if
                Head#version.versionId > PreviousVersionId ->
                    throw({list_is_not_well_ordered});
                
                Head#version.versionId =:= PreviousVersionId ->
                    throw({list_contains_duplicate_versions});
                
                % Normal case
                Head#version.versionId < PreviousVersionId ->
                    assert_later_version(Tail, ReleasedVersionId, Head#version.versionId)
            end
    end;
assert_later_version([], _, _) ->
    ok.
