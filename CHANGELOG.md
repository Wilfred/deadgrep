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
