-module(kazoo_tts).

-export([create/1
        ,create/2
        ,create/3
        ,create/4
        ,create/5
        ]).

-include("kazoo_speech.hrl").
-include_lib("kazoo_translator/include/kazoo_translator.hrl").

-spec create(ne_binary()) -> create_resp().
create(Text) ->
    create(Text, <<"female/en-us">>).

-spec create(ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

-spec create(ne_binary(), ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice, Format) ->
    create(Text, Voice, Format, []).

-spec create(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    Provider = ?DEFAULT_TTS_ENGINE,
    create(Provider, Text, Voice, Format, Options).

-spec create(api_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"flite">>, _Text, _Voice, _Format, _Options) ->
    {'error', 'tts_provider_failure', <<"flite is not available to create TTS media">>};
create(Engine, Text, Voice, Format, Options) ->
    (kz_term:to_atom(<<"kazoo_tts_", Engine/binary>>, 'true')):create(Text, Voice, Format, Options).
