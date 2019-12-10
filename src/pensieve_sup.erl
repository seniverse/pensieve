-module(pensieve_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    MetaStore = application:get_env(pensieve, meta_store, pensieve_dummy_meta_store),
    ChunkStore = application:get_env(pensieve, chunk_store, pensieve_ets_chunk_store),

    persistent_term:put({pensieve, meta_store}, MetaStore),
    persistent_term:put({pensieve, chunk_store}, ChunkStore),

    {ok,
     {#{strategy => one_for_one,
        intensity => 1,
        period => 5},
      [#{id => pensieve_event,
         start => {pensieve_event, start_link, []},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => []}
      ] ++ MetaStore:childspecs() ++ ChunkStore:childspecs() ++
          [
           #{id => pensieve_server,
             start => {pensieve_server, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => []}
          ]
     }}.
