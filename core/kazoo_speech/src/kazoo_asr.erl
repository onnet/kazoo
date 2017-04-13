-module(kazoo_asr).

-export([freeform/1, freeform/2, freeform/3, freeform/4
        ,commands/2, commands/3, commands/4, commands/5
        ]).

-include("kazoo_speech.hrl").

-define(ASR_PROVIDER, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>)).

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec freeform(binary()) -> asr_resp().
-spec freeform(binary(), ne_binary()) -> asr_resp().
-spec freeform(binary(), ne_binary(), ne_binary()) -> asr_resp().
-spec freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
freeform(Content) ->
    freeform(Content, <<"application/wav">>).
freeform(Content, ContentType) ->
    freeform(Content, ContentType, <<"en-US">>).
freeform(Content, ContentType, Locale) ->
    freeform(Content, ContentType, Locale, []).
freeform(Content, ContentType, Locale, Options) ->
    case kz_term:is_empty(?ASR_PROVIDER) of
        'true' -> {'error', 'no_asr_provider'};
        'false' -> maybe_convert_content(Content, ContentType, Locale, Options)
    end.

-spec maybe_convert_content(binary(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
maybe_convert_content(Content, ContentType, Locale, Options) ->
    ContentTypes = kapps_config:get(?MOD_CONFIG_CAT
                                   ,<<"asr_content_types">>
                                   ,[<<"application/mpeg">>
                                    ,<<"application/wav">>
                                    ]),
    case lists:member(ContentType, ContentTypes) of
        'true' -> attempt_freeform(Content, ContentType, Locale, Options);
        'false' ->
            ConvertTo = kapps_config:get_binary(?MOD_CONFIG_CAT
                                               ,<<"asr_prefered_content_type">>
                                               ,<<"application/mpeg">>
                                               ),
            case convert_content(Content, ContentType, ConvertTo) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted ->
                    attempt_freeform(Converted, ConvertTo, Locale, Options)
            end
    end.

-spec attempt_freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
attempt_freeform(Content, ContentType, Locale, Options) ->
    case attempt_freeform(?ASR_PROVIDER, Content, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'http_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p"
                       ,[ReqID, props:get_value('receiver', Options)]),
            {'ok', ReqID};
        {'ok', 200, _Headers, Content2} ->
            lager:debug("asr of media succeeded: ~s", [Content2]),
            {'ok', kz_json:decode(Content2)};
        {'ok', _Code, _Hdrs, Content2} ->
            lager:debug("asr of media failed with code ~p", [_Code]),
            lager:debug("resp: ~s", [Content2]),
            {'error', 'asr_provider_failure', kz_json:decode(Content2)}
    end.

-spec attempt_freeform(api_binary(), binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                              provider_return().
attempt_freeform(_, <<>>, _, _, _) -> {'error', 'no_content'};
attempt_freeform(<<"ispeech">>, Bin, ContentType, Locale, Opts) ->
    BaseUrl = kapps_config:get_string(?MOD_CONFIG_CAT
                                     ,<<"asr_freeform_url">>
                                     ,<<"http://api.ispeech.org/api/rest">>
                                     ),
    lager:debug("sending request to ~s", [BaseUrl]),
    Props = [{<<"apikey">>, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
            ,{<<"action">>, <<"recognize">>}
            ,{<<"freeform">>, <<"1">>}
            ,{<<"content-type">>, ContentType}
            ,{<<"output">>, <<"json">>}
            ,{<<"locale">>, Locale}
            ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = kz_http_util:props_to_querystring(Props),
    lager:debug("req body: ~s", [Body]),
    case props:get_value('receiver', Opts) of
        Pid when is_pid(Pid) ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions);
        _ ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:post(BaseUrl, Headers, Body, HTTPOptions)
    end;
attempt_freeform(_Engine, _, _, _, _) ->
    lager:debug("unknown or misconfigured provider: ~p", [_Engine]),
    {'error', 'unknown_provider'}.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec commands(ne_binary(), ne_binaries()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
-spec commands(ne_binary(), ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
commands(Bin, Commands) ->
    commands(Bin, Commands, <<"application/wav">>).
commands(Bin, Commands, ContentType) ->
    commands(Bin, Commands, ContentType, <<"en-US">>).
commands(Bin, Commands, ContentType, Locale) ->
    commands(Bin, Commands, ContentType, Locale, []).
commands(Bin, Commands, ContentType, Locale, Options) ->
    case commands(?ASR_PROVIDER, Bin, Commands, ContentType, Locale, Options) of
        {'error', _R}=E ->
            lager:debug("asr failed with error ~p", [_R]),
            E;
        {'http_req_id', ReqID} ->
            lager:debug("streaming response ~p to provided option: ~p", [ReqID, props:get_value(receiver, Options)]),
            {'ok', ReqID};
        {'ok', 200, _Headers, Content} ->
            lager:debug("asr of media succeeded: ~s", [Content]),
            {'ok', kz_json:decode(Content)};
        {'ok', _Code, _Hdrs, Content} ->
            lager:debug("asr of media failed with code ~s", [_Code]),
            lager:debug("resp: ~s", [Content]),
            {'error', 'asr_provider_failure', kz_json:decode(Content)}
    end.

commands(<<"ispeech">>, Bin, Commands, ContentType, Locale, Opts) ->
    BaseUrl = kapps_config:get_string(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>),

    Commands1 = kz_binary:join(Commands, <<"|">>),

    lager:debug("sending request to ~s", [BaseUrl]),

    Props = [{<<"apikey">>, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>)}
            ,{<<"action">>, <<"recognize">>}
            ,{<<"alias">>, <<"command1|YESNOMAYBE">>}
            ,{<<"YESNOMAYBE">>, Commands1}
            ,{<<"command1">>, <<"say %YESNOMAYBE%">>}
            ,{<<"content-type">>, ContentType}
            ,{<<"output">>, <<"json">>}
            ,{<<"locale">>, Locale}
            ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/json"}],

    Body = kz_json:encode(kz_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),

    case props:get_value('receiver', Opts) of
        Pid when is_pid(Pid) ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:async_req(Pid, 'post', BaseUrl, Headers, Body, HTTPOptions);
        _ ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:post(BaseUrl, Headers, Body, HTTPOptions)
    end;
commands(_Engine, _, _, _, _, _) ->
    lager:debug("unknown or misconfigured provider: ~p", [_Engine]),
    {'error', 'unknown_provider'}.

-spec convert_content(binary(), ne_binary(), ne_binary()) -> binary() | 'error'.
convert_content(Content, <<"audio/mpeg">>, <<"application/wav">> = _ContentType) ->
    Mp3File = kazoo_speech_util:tmp_file_name(<<"mp3">>),
    WavFile = kazoo_speech_util:tmp_file_name(<<"wav">>),
    kz_util:write_file(Mp3File, Content),
    Cmd = io_lib:format("lame --decode ~s ~s &> /dev/null && echo -n \"success\"", [Mp3File, WavFile]),
    _ = os:cmd(Cmd),
    kz_util:delete_file(Mp3File),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            kz_util:delete_file(WavFile),
            WavContent;
        {'error', _R} ->
            lager:info("unable to convert mpeg to wav: ~p", [_R]),
            'error'
    end;
convert_content(_, ContentType, ConvertTo) ->
    lager:info("unsupported conversion from %s to %s", [ContentType, ConvertTo]),
    'error'.
