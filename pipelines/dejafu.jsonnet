local library = import '_library.libsonnet';
local simple_haskell = import '_simple_haskell.libsonnet';

local concurrency = simple_haskell('concurrency', 'dejafu', 'concurrency');
local dejafu = simple_haskell('dejafu', 'dejafu', 'dejafu', ['dejafu/', 'dejafu-tests/']);
local hunit_dejafu = simple_haskell('hunit-dejafu', 'dejafu', 'hunit-dejafu');
local tasty_dejafu = simple_haskell('tasty-dejafu', 'dejafu', 'tasty-dejafu');

local build_script = |||
  stack="stack --no-terminal"

  # don't build all the dejafu-bench dependencies to speed up compilation
  # stack's docs suggest this should work: `stack build dejafu-tests:dejafu-tests`
  # but it doesn't, it still builds dejafu-bench too
  sed -n '/executable dejafu-bench/q;p' dejafu-tests/dejafu-tests.cabal > dejafu-tests.cabal
  mv dejafu-tests.cabal dejafu-tests/dejafu-tests.cabal

  # use a utf-8 locale so hedgehog failure output doesn't
  # cause an encoding error - this was the default in the
  # haskell:8.8.1 image but not after that.
  export LANG=C.UTF-8

  if [ -f ../stackage-feed/item ]; then
    apt-get update && apt-get install -y jq
    resolver="$(jq -r .id < ../stackage-feed/item | cut -d/ -f4)"
    $stack init --resolver="$resolver" --force
  fi

  $stack setup
  $stack build
  $stack exec dejafu-tests
|||;

local deploy_script = |||
  ver=$(grep '^version:' "${CABAL_FILE}" | sed 's/^version: *//')

  if curl -fs "http://hackage.haskell.org/package/${PACKAGE}-${ver}" >/dev/null; then
    echo "version already exists on hackage" >&2
    exit 0
  fi

  fail=false
  if ! grep -q -E "tag: *${PACKAGE}-${ver}" "${PACKAGE}/${PACKAGE}.cabal"; then
    echo "missing tag in ${PACKAGE}/${PACKAGE}.cabal" >&2
    fail=true
  fi
  if ! grep -q "^${ver}" "${PACKAGE}/CHANGELOG.rst"; then
    echo "missing header in ${PACKAGE}/CHANGELOG.rst" >&2
    fail=true
  fi
  if ! grep -q -E "Git.*${PACKAGE}-${ver}" "${PACKAGE}/CHANGELOG.rst"; then
    echo "missing tag in ${PACKAGE}/CHANGELOG.rst" >&2
    fail=true
  fi
  if ! grep -q -E "Hackage.*${PACKAGE}-${ver}" "${PACKAGE}/CHANGELOG.rst"; then
    echo "missing hackage link in ${PACKAGE}/CHANGELOG.rst" >&2
    fail=true
  fi
  if ! grep -q -E "${PACKAGE}.*${ver}" README.markdown; then
    echo "missing package version in README.markdown" >&2
    fail=true
  fi
  if ! grep -q -E "${PACKAGE}.*${ver}" doc/getting_started.rst; then
    echo "missing package version in doc/getting_started.rst" >&2
    fail=true
  fi
  if git grep -q unreleased "$PACKAGE"; then
    echo "'unreleased' appears in source" >&2
    fail=true
  fi
  if $fail; then
    exit 1
  fi
|||;

{
  resource_types: [
    library.resource_type('feed-resource'),
  ],

  resources: [
    dejafu.resources.top,
    //
    concurrency.resources.cabal,
    dejafu.resources.cabal,
    hunit_dejafu.resources.cabal,
    tasty_dejafu.resources.cabal,
    //
    library.feed_resource('stackage', 'https://www.stackage.org/feed'),
  ],

  jobs: [
    dejafu.jobs.test_snapshot(build_script) + { name: 'test-snapshot' },
    //
    concurrency.jobs.test(build_script),
    dejafu.jobs.test(build_script),
    hunit_dejafu.jobs.test(build_script),
    tasty_dejafu.jobs.test(build_script),
    //
    concurrency.jobs.deploy(deploy_script),
    dejafu.jobs.deploy(deploy_script),
    hunit_dejafu.jobs.deploy(deploy_script),
    tasty_dejafu.jobs.deploy(deploy_script),
  ],
}
