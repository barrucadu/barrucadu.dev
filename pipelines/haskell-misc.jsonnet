local library = import '_library.libsonnet';
local simple_haskell = import '_simple_haskell.libsonnet';

local both = simple_haskell('both');

{
  resource_types: [
    library.resource_type('event-api-resource'),
    library.resource_type('feed-resource'),
  ],

  resources: both.resources.all + [
    library.feed_resource('stackage', 'https://www.stackage.org/feed'),
  ],

  jobs: both.jobs.all(),
}
