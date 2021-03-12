local library = import '_library.libsonnet';
local simple_haskell = import '_simple_haskell.libsonnet';

local irc_ctcp = simple_haskell('irc-ctcp');
local irc_conduit = simple_haskell('irc-conduit');
local irc_client = simple_haskell('irc-client');

{
  resource_types: [
    library.resource_type('feed-resource'),
  ],

  resources: irc_ctcp.resources.all + irc_conduit.resources.all + irc_client.resources.all + [
    library.feed_resource('stackage', 'https://www.stackage.org/feed'),
  ],

  jobs:
    irc_ctcp.jobs.all() + irc_conduit.jobs.all() + irc_client.jobs.all(),
}
