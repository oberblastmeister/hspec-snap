# hspec-snap

Snapshot tests assert values against a reference (the snapshot). This is useful when your reference values are very large or change often. Snapshot tests are also called golden tests or approval tests.

## Quick Start

You can write you hspec tests as normal but use `snap` instead of `it` when you want to use a snapshot test.

```haskell
import Test.Hspec
import Test.Hspec.Snap

describe "a description"
  snap "snapshot_test" $
    show [1, 2, 3]

  it "should work" $
    1 `shouldBe` 1
```

After the test has been run for the first time, a snapshot file will be created. If the test changes, for example changing it to

```haskell
snap "snapshot_test" $
  show [1, 2]
```

the test will fail and a file called `snapshot_test.snap.new` will be created with the contents `[1, 2]`. You can then use the cli to decide what to do with the new file.

You can run
```sh
$ hsnap review
```
to get an interactive interface to accept, reject, or skip the new snapshot file.

## CLI Options

```
Usage: hsnap COMMAND
  a wrapper to run snapshot tests

Available options:
  -h,--help                Show this help text

Available commands:
  test                     Run all the tests and then review
  diff                     Show all the snapshot diffs
  review                   Interactively review all snapshots
  accept                   Accept all snapshots
  reject                   Reject all snapshots
```

## Inspiration

This library was inspired by [insta](https://github.com/mitsuhiko/insta) and
[hspec-golden](https://github.com/stackbuilders/hspec-golden)