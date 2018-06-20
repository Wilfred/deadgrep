# deadgrep: use ripgrep from Emacs [![Build Status](https://travis-ci.org/Wilfred/deadgrep.svg?branch=master)](https://travis-ci.org/Wilfred/deadgrep) [![Coverage Status](https://coveralls.io/repos/github/Wilfred/deadgrep/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/deadgrep?branch=master)

Deadgrep is the fast, beautiful text search that your Emacs
deserves.

![screenshot](deadgrep_screenshot.png)

## Installation

1. Install [ripgrep](https://github.com/BurntSushi/ripgrep).

2. Install deadgrep from MELPA (coming soon) or copy it into your
   ~/.emacs.d.

3. (Optional) add deadgrep to a convenient shortcut. I like F5.

``` emacs-lisp
(global-set-key (kbd "<f5>") #'deadgrep)
```

## Super Fast

Deadgrep uses ripgrep for extremely fast text searches. It works hard
to minimise your keystrokes, to get you results as soon as
possible. Try running `M-x deadgrep` with a region highlighted.

If you change your search settings, deadgrep immediately re-runs
the search.

## Easy Filtering

Didn't get the results you wanted? It's easy to change the search
term, search type, or search directory, directly from the results
buffer. Just push the relevant button.

## Polish

Deadgrep uses spinners to give you feedback on whether your search has
finished.

It highlights regexp syntax according to the syntax accepted by `rg`.

Deadgrep handles minified files robustly.

You can always jump to exactly the position that point is on, even
when searching files that contain tab characters. You can also
navigate to the file itself from the file headings.

## Alternative Projects

I believe that deadgrep is the best tool for doing Emacs text
searches, but there are some other great tools out there. See
[ALTERNATIVES](ALTERNATIVES.md) for a discussion.
