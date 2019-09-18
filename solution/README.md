# Challenge solution

## Build instructions

I've verified the solution builds and runs on Linux with either of these
options:

1.  [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

    ```sh
    $ cd solution
    $ stack build

    ... one eternity later

    $ ./path/to/generator | stack run
    Running http://localhost:8000
    ```

2.  [Nix](https://nixos.org/nix/)

    Included in the root of the repository is a file called `shell.nix`,
    which enables running `nix-shell` at the root of the directory.

    ```sh
    $ cd solution
    $ nix-shell

    ... one eternity later

    [nix-shell:~/git/challenge]$
    # now in a shell with 2 new binaries: generator and solution

    [nix-shell:~/git/challenge]$ generator | solution
    Running http://localhost:8000
    ```

## Possible improvements

1.  I managed to not introduce any bugs in any change I made to the
    solution of the exercise, but really the code could benefit from an
    additional test suite.  

    Some logical places would be the pure logic in Model.hs, JSON
    parsing against a recorded output of the generator and end-to-end
    against a running server.

2.  The types in Model.hs have associated typeclass instances which have
    less to do with what the program is doing and more to do with JSON
    encoding.

    Decoupling these concerns from the types would make the solution
    more complex, but I'm confident that there exists a tenable solution
    utilizing `newtype`, `DerivingVia` and strategic use of `coerce`.

3.  I consider the solution to be complete, and flexible enough to
    easily make alterations, however some of these details are hardcoded
    in the implementation which may make it seem less than obvious. 

    For example, a shell pipe can hold about ~64kb of data, but if we
    chose not rely on that we could aggregate the events in a `TQueue`.
    From there we have other options at our disposal like multiplexing
    the processing into multiple processors each consuming the queue.
    Each 'hardcoded' part can be parametrized and given from outside.
