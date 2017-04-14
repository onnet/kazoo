-module(kazoo_tts).

-export([create/1
        ,create/2
        ,create/3
        ,create/4
        ,create/5
        ]).

-export([cache_time_ms/0
        ,default_engine/0, default_engine/1
        ,default_language/0
        ,default_voice/0
        ]).

-include("kazoo_speech.hrl").

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
    create(default_engine(), Text, Voice, Format, Options).

-spec create(api_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"flite">>, _Text, _Voice, _Format, _Options) ->
    {'error', 'tts_provider_failure', <<"flite is not available to create TTS media">>};
create(Engine, Text, Voice, Format, Options) ->
    (kz_term:to_atom(<<"kazoo_tts_", Engine/binary>>, 'true')):create(Text, Voice, Format, Options).

-spec cache_time_ms() -> pos_integer().
cache_time_ms() ->
    kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_cache">>, ?MILLISECONDS_IN_HOUR).

-spec default_engine() -> ne_binary().
-spec default_engine(kapps_call:call()) -> ne_binary().
default_engine() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>).

default_engine(Call) ->
    Default = default_engine(),
    kapps_account_config:get_global(Call, ?MOD_CONFIG_CAT, <<"tts_provider">>, Default).

-spec default_language() -> ne_binary().
default_language() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_language">>, <<"en-us">>).

-spec default_voice() -> ne_binary().
default_voice() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_voice">>, <<"male">>).
