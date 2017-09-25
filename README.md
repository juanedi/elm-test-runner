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
