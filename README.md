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

Deadgrep uses ripgrep for extremely fast text searches. It then
minimises keystrokes to get you results as soon as possible.

If you change your search settings, deadgrep immediately re-runs
the search.

If you're searching a large project, then deadgrep shows a spinner
to give you feedback.

## Easy Filtering

Didn't get the results you wanted? It's easy to change the search
term, search type, or search directory, directly from the results
buffer. Just push the relevant button.

## Alternative Projects

I believe that deadgrep is the best tool for doing Emacs text
searches, but there are some other great tools out there. See
[ALTERNATIVES](ALTERNATIVES.md) for a discussion.
