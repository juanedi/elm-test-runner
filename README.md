# elm-test-el

This package provides some convenience functions for running `elm-test` on your Elm project.
Most functionality was ported from the fantastic [https://github.com/pezra/rspec-mode](rspec-mode), which is super awesome.


## Usage

The following functions are available:

Function                              | Description                                                                   |
--------------------------------------|-------------------------------------------------------------------------------|
`elm-test-run`                        | Run the test suites associated with the current buffer                        |
`elm-test-rerun`                      | Run the last test module again                                                |
`elm-test-watch`                      | Run the test suites associated with the current buffer and watch for changes  |
`elm-test-toggle-test-and-target`     | Toggle back and forth between a test module and its target                    |
`elm-test-run-project`                | Run all tests in the current project                                          |
`elm-test-run-elm-test-run-directory` | Prompt for a directory to run tests on                                        |


When toggling to a test module that doesn't exist yet, you'll be prompted if you want to create it. See below for more information on how to customize the test module template.

## Customization

By default, all commands should work out of the box assuming:

* The `elm-test` binary is installed locally
* Your elm project has a `tests` directory where tests are located
* Your test modules files are named `****Test.elm`

If any of these are not true, the following settings might be useful:


Variable                          | Description | Default value |
----------------------------------|-------------|---------------|
`elm-test-command`                | Use this to override the base command for the `elm-test` runner. | `nil`, which means a locally installed node runner is tried, with fallback to a globally installed `elm-test` |
`elm-test-command-options`        | Default options used when running tests for a module | Empty |
`elm-test-preferred-test-suffix`  | If you put tests for `Foo` in the module `FooSpec`, this should be `Spec` | `Test` |
`elm-test-run-directory-for-file` | How to find the directory from where to run the command. You probably don't need to change this | Resolves to the location of the top level `elm-package.json` |
`elm-test-project-root-for-file`  | How to find the elm project's root directory. You probably don't need to change this. | Resolves to the location of the top level `elm-package.json` |
`elm-test-template-for-module`    | Function that builds the test module template when creating one | Create minimal test module with a single passing test |


All customizable function variables can be either the (quoted) name of a function or a lambda that expect:
  - `elm-test-run-directory-for-file`: current file name
  - `elm-test-project-root-for-file`: current file name
  - `elm-test-template-for-module` elm module name of the test suite being created and the target module


Hint: remember you can use `.dir-locals.el` to customize these settings on a per-project basis. If you do that, you may need to add these values to `safe-local-variable-values` to avoid emacs asking if you trust these values every time.
