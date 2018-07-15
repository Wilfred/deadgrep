# deadgrep: use ripgrep from Emacs

Deadgrep is the fast, beautiful text search that your Emacs
deserves.

 [![Build Status](https://travis-ci.org/Wilfred/deadgrep.svg?branch=master)](https://travis-ci.org/Wilfred/deadgrep) [![Coverage Status](https://coveralls.io/repos/github/Wilfred/deadgrep/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/deadgrep?branch=master) [![MELPA](http://www.melpa.org/packages/deadgrep-badge.svg)](http://www.melpa.org/#/deadgrep)

![screenshot](docs/deadgrep_screenshot.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Usage](#usage)
    - [Installation](#installation)
    - [Keybindings](#keybindings)
- [Features](#features)
    - [Super Fast](#super-fast)
    - [Context Aware](#context-aware)
    - [Easy Filtering](#easy-filtering)
    - [Polish](#polish)
- [Alternative Projects](#alternative-projects)

<!-- markdown-toc end -->

## Usage

### Installation

1. Install [ripgrep](https://github.com/BurntSushi/ripgrep).

2. Install deadgrep from MELPA or copy it into your `~/.emacs.d`.

3. (Optional) add deadgrep to a convenient shortcut. I like F5.

``` emacs-lisp
(global-set-key (kbd "<f5>") #'deadgrep)
```

### Keybindings

| Key                           | Action                                    |
| ---                           | ---                                       |
| <kbd>RET</kbd>                | Visit the result, file or button at point |
| <kbd>n</kbd> and <kbd>p</kbd> | Move between results or buttons           |
| <kbd>g</kbd>                  | Re-run the search                         |
| <kbd>TAB</kbd>                | Expand/collapse results for a file        |
| <kbd>C-c</kbd> <kbd>C-k</kbd> | Stop a running search                     |

### Minibuffer

You use the minibuffer to enter a new search term.

You can also reuse a previous search term with <kbd>M-p</kbd> in the
minibuffer. To edit the default search term, use <kbd>M-n</kbd>.

## Features

### Super Fast

Deadgrep uses ripgrep for extremely fast text searches.

If you change your search settings, deadgrep will immediately re-run
your search.

### Context Aware

Deadgrep works hard to minimise your keystrokes.

**Search term**: If the region is active, deadgrep uses that.

**Search directory**: If your current file is in a git project,
deadgrep uses that for your search directory.

**Regexp and case sensitivity options**: Deadgrep re-uses whatever
settings you used in your last search.

**Globbing and file types**: Deadgrep suggests file types and globbing
options that match the file you started the search from.

### Easy Filtering

Didn't get the results you wanted? It's easy to change the search
term, search type, or search directory, directly from the results
buffer. Just push the relevant button.

![screenshot](docs/deadgrep_filters.png)

### Polish

Deadgrep uses spinners to give you feedback on whether your search has
finished.

![screenshot](docs/deadgrep_spinner.png)

It highlights regexp syntax according to the syntax accepted by `rg`.

![screenshot](docs/deadgrep_highlight.png)

When navigating to a line that matched, the relevant part of the line
is temporarily highlighted.

![screenshot](docs/deadgrep_highlight_relevant.png)

You can collapse and expand files with `TAB`.

![screenshot](docs/deadgrep_collapsed.png)

Deadgrep handles minified files robustly.

![screenshot](docs/deadgrep_truncated.png)

You can always jump to exactly the position that point is on, even
when searching files that contain tab characters. You can also
navigate to the file itself from the file headings.

## Alternative Projects

I believe that deadgrep is the best tool for doing Emacs text
searches, but there are some other great tools out there. See
[ALTERNATIVES](docs/ALTERNATIVES.md) for a discussion.
