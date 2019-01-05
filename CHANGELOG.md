# v0.7

Tramp support!

Added `deadgrep-project-root-overrides`. This enables you to change
the root directory for a specific project without writing a whole custom
function for `deadgrep-project-root-function`.

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
