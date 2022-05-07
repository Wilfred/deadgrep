# v0.12

Added a command `deadgrep-incremental` to provide as-you-type
searching.

Fixed support for Emacs 27 and earlier.

# v0.11 (released April 18 2022)

Added a command `deadgrep-search-term` (bound to `S` in `*deadgrep*`
buffers) to change the current search term.

Added a command `deadgrep-directory` (bound to `D` in `*deadgrep*`
buffers) to change the current search directory.

Fixed a crash in deadgrep when searching with malformed regular
expressions (#103).

# v0.10

Fixed `query-replace` when editing deadgrep results (#60).

Added `deadgrep-display-buffer-function` to control how the results
buffer is displayed (#81).

Fixed an issue where using `.ripgreprc` with `--column` could break
deadgrep (#91).

When starting a search, the prompt now informs whether you're in
regexp or text mode.

Fixed an issue with the type filter where some options were not
auto-selected (#96).

# v0.9

Results buffers are now opened with `switch-to-buffer-other-window`.

This enables deadgrep to work with the popwin package, and ensures that deadgrep
respects the value of `pop-up-frames`.

Fixed an issue with the file type suggestion not offering the correct
type for the initial file.

Fixed an issue where results weren't shown if a text search term
contained a `\`.

Fixed a crash when rg emitted an error when searching binary files.

Added command `deadgrep-kill-all-buffers` which kills all open deadgrep
buffers.

UI improvements when search term is very long.

Fixed issues on Windows hosts (see
[#64](https://github.com/Wilfred/deadgrep/pull/64)).

Edit mode improvements: `deadgrep-edit-mode` now gives instructions
when finished, and fixed a crash when switching back to
`deadgrep-mode` and re-running a search.

# v0.8

The default value for `deagrep-exectable` is now `"rg"`, rather than
explicitly searching for the binary when the library is loaded. This
is hopefully less surprising. `deadgrep-executable` is also now
exposed to Customize.

Added commands `deadgrep-forward-match` and `deadgrep-backward-match`
(bound to `N` and `P`) which move to the beginning of the matched
text, not just the match line. This is particularly useful when using
deadgrep with keyboard macros.

Unlike `deadgrep-forward` and `deadgrep-backward`, these commands
ignore buttons and file headers.

Added commands `deadgrep-forward-filename` and
`deadgrep-backward-filename` (bound to `M-n` and `M-p`) to move point
between file headers.

# v0.7

**Experimental**: It's now possible to edit results buffers! Try
`M-x deadgrep-edit-mode` in a results buffer. This feature is still
experimental, so configuration or command names may change.

Tramp support!

Added imenu for moving between files in a results buffer.

Added `deadgrep-project-root-overrides`. This enables you to change
the root directory for a specific project without writing a whole custom
function for `deadgrep-project-root-function`.

Search results in narrowed buffers are now handled correctly.

Fixed an issue where `n` and `p` (`deadgrep-forward` and
`deadgrep-backward`) did not work.

# v0.6

`next-error` and `previous-error` now work, just like they do with
`M-x grep`.

When visiting a result, highlight the last visited result with an
arrow. This is particularly useful when using
`deadgrep-visit-result-other-window` or `next-error`.

Fixed several issues when restarting searches (pressing `g` or changing
parameters). Previously, we would end up with results from the old
search or errors.

Calling deadgrep with a prefix, e.g. `C-u M-x deadgrep`, will now open
a results buffer without starting a search. This is handy if you want
to tweak the parameters before starting a search.

Added a face `deadgrep-search-term-face` to make the search term
visually distinct from search options. Improved highlighting with
`deadgrep-regexp-metachar-face`

File type picker now makes better suggestions when there are multiple
file types that match, or if ripgrep is using complex globs.

Fixed an issue where `M-x eval-buffer` on deadgrep.el would override
custom keybindings.

# v0.5

When navigating to a matched line, flash the matched part of the line
(inspired by the helm-rg readme!).

Show a message when the search has finished. This helps clarify when
searches finish really quickly but with zero results.

Added a `deadgrep-debug` command to help users investigate issues and
file bugs.

Added a `deadgrep-visit-result-other-window` command (bound to
<kbd>o</kbd>) for opening results in a separate window. This is handy
if you want the results buffer to stay visible.

Improved file type switcher: show extensions, and fixed an issue where
the wrong default was offered.

Show a divider between results when showing context around the
matching line.

# v0.4

Added the ability to stop searches with `C-c C-k`.

Deadgrep now treats search terms as a separate history list, rather
than offering all previous minibuffer inputs. Document this
functionality.

Defined separate faces for the different styles by deadgrep, so users
may customise them.

Deadgrep no longer shows 'exited abnormally with code 1'. The most
common cause of this is simply a search term with zero results. We are
now relying on deadgrep writing something to stderr if there are any
other issues that caused an exit code of 1.

When starting a search from another search buffer, remember the
original file that the original search buffer was opened from. This
ensures we can still offer useful globbing/file type defaults.

Allow users to configure the function used to find the project root
with `deadgrep-project-root-function`.

# v0.3

Added the ability to collapse results from a specific file!

Deadgrep now re-uses search settings from the last search, so if you
prefer regexp settings, you only need to set it once!

When choosing a file type to search, the default option matches the
file the search was started from.

Fixed an issue where active regions hang around after starting a
search.

# v0.2

Fixed Windows (PR by @iquiw).

Added the ability to show lines of context around search results.

# v0.1

Initial release.
