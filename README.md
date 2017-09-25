# Emacs elm-test

This package provides some convenience functions for running `elm-test` on your Elm project.
Most functionality was ported from the fantastic [https://github.com/pezra/rspec-mode](rspec-mode), which is super awesome.


## Usage

The following functions are available. No keybindings are provided yet, sorry :-)

Function                            | Description                                                                   |
------------------------------------|-------------------------------------------------------------------------------|
`elm-test-run`                      | Run the test suites associated with the current buffer                        |
`elm-test-rerun`                    | Run the last test module again                                                |
`elm-test-watch`                    | Run the test suites associated with the current buffer and watch for changes  |
`elm-test-toggle-test-and-target`   | Toggle back and forth between a test module and its target                    |


## Customization

By default, all commands should work out of the box assuming:

* The `elm-test` binary is installed locally
* Your elm project has a `tests` directory where tests are located
* Your test modules files are named `****Test.elm`

If any of these are not true, the following settings might be useful:


Variable                          | Description | Default value |
----------------------------------|-------------|---------------|
`elm-test-command`                | The base command for `elm-test`. Useful if not installed globally or using Docker, for example | `elm-test` |
`elm-test-command-options`        | Default options used when running tests for a module | Empty |
`elm-test-preferred-test-suffix`  | If you put tests for `Foo` in the module `FooSpec`, this should be `Spec` | `Test` |
`elm-test-run-directory-for-file` | How to find the directory from where to run the command. You probably don't need to change this | The location of the top level `elm-package.json` |
`elm-test-project-root-for-file`  | How to find the elm project's root directory. You probably don't need to change this. | The location of the top level `elm-package.json` |

Both `elm-test-run-directory-for-file` and `elm-test-project-root-for-file` can be either the name or a function or a lambda that receive the current file name as a parameter.
