# orgnav

Quickly navigate and search your emacs org trees; use this navigation to capture and organize.
Built with the help of helm.

## Motivational introduction

This library allows you navigate your org tree interactively with helm.
As an example using this library you might:

* Start searching your org tree at the top-level
* Increase the depth you are displaying a few times with `M-l`
* Filter to tasks that are in progress by searching for "INPROGRESS"
* Find a task that you are interested in using `M-j` and `M-k`
* Look at the ancestors of this task with `M-a`
* Find an interesting ancestor and look at all its descendents with `M-.`

To get a summary of how to use this library, run `M-x orgnav-search-root` and press `TAB`.
This will display a list of keybindings. Using these keybindings is very much encouraged.

Run `M-x orgnav<TAB>` for a list of functions: functions without `--`s in them
are public functions that you might like to call.

## Requirements

This library makes use of [lexical bindings](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html) in some functions, and so **requires emacs 24.1 or newer**.

## Installing

Orgnav is available on MELPA.
Only stable versions are released to MELPA, though the standards for stable are not that high.
`dev` is the bleeding edge branch which should be installed manually.

### Manual installation

Download the source code.
`master` is liable to have (quickly fixed) bugs.
For versions that are less likely to have bugs see `https://github.com/facetframer/orgnav/releases/`.
Place this repository on your `load-path`.

Add

```
(require `orgnav)
```

to your `init.el`.

Run some functions with `M-x`.

You may well want to set up some keybindings using `define-key` and `global-set-key`.
No defaults are provided since users of this library likely have strong opinions about such things.

## Debugging

Try setting the `orgnav-log` variable and reviewing the `*Messages*` buffer:

```
(setq orgnav-log 't)
```

## Advanced-use features
### Extensibility
Functions are moderately flexible and you can call them yourself from elisp.
Whenever you want to find org nodes to operate on from elisp this library can be useful. The synchronuos variants of functions are very relevant here.

#### Specific-purpose convenience functions
The core of this library is the navigation interface that can be used extensibly from elisp. However some common (or obvious) examples are included:


* Clocking into items (`orgnav-clock-in`)
* Refiling (`orgnav-refile`, `orgnav-refile-ancestors` and friends)
* Use with `org-capture` to select your capture target (`orgnav-capture-function-relative` and `orgnav-capture-function-global`)

## Alternatives and prior work

See [this stack overflow post](https://emacs.stackexchange.com/questions/32617/how-to-jump-directly-to-an-org-headline) for a discussion of various options. This tool was inspired by the limitiations of `helm-org`. In fact, the projects initial name of *better helm org*.

Many tools provide similar functionality, but as a rule this is a useful one-off corner case rather than a general tool to move around org files. This speaks to the value of a general navigation tool.

Looking at the alternatives reviewed:

- None of the alternatives reviewed provides the ability to search under a particular node rather than globally (though this can be dealt with by narrow).
- None of the alternatives reviewed allow one to chain together searches (for example searching under a category that you have found)
- None of the alternatives reviewed provide easy ways to use the interface programmatically from `org-refile` or `org-capture`
- None of the alternatives reviewed allow one to change the depth that you are searching at (although `org-goto` searches with a depth of 1 rather than unbounded depth like other tools)

### helm-org
`helm-org` has the feeling of a proof-of-concept library. `orgnav` is more complete, and
intends to be a complete navigation tool rather than a searching library.
However, for simple use cases, `helm-org` may be good enough and is likely more stable.

`helm-org` only allows searching from the root node whereas `orgnav` allows searching from a particularly node. This could be achieved with narrowing.

`orgnav` allows the chaining together of searches. This is particularly useful when one wants to search within a headline that you find.

`orgnav` allow one to vary the depth of search. This can be very important when one has a large tree to search.

### helm-org-rifle

[helm-org-rifle](https://github.com/alphapapa/helm-org-rifle#changelog) is quite similar to this package.
All searches are global and of arbitrary depth, though narrowing could address this point.
Searches include the contents of entries. This is analogous to `helm-swoop`.

When there is a large amount of text contained within entries (as is often the case) is is very useful to search just headlines.
Searching just the headlines means fewer characters are required to find the entry that you are looking for.

At times one explicitly wants to search content however.

### org-search-goto

*org-search-goto* allows one to search within headlines.

*org-search-goto* does not allow one to iteratively searching: a single search term is provided and searched for.
*org-search-goto` does not allow chaining of searches.

### org-goto
`org-goto` is `org`'s built-in navigation tool.
It is more of an iterative search tool (like `isearch`) than a summary tool, as such, finding a particularly entry can require one to search through all the matches in one's document one by one.

`org-goto` can be made to use a pager using `outline-path-complete`. This does not work with `helm`. Further it can only show one level of depth and always performs a global search.

Again nothing like `orgnav`s ability to chain together a number of searches starting at different points is provided.

### Judicious use of `org-capture`, `org-refile`, and `org-find-olp`
Though initially slightly cryptic if you are willing to do some
scripting these functions are very powerful. This is particularly the case if
your workflow is quite consistent.

The general purpose functionality of `orgnav` may be unnecessary for you:
hacking up some elisp that supports a very limited workflow might work better.
Specifically, this is likely to be the case if you are rarely creating new nesting
in your org file.

### org-sparse-tree
In some ways, `org-sparse-tree` overlaps with this library, as well as providing
a number of orthogonal searching criteria. The downsides of `org-sparse-tree` are
that it changes the folding of your buffer, and can show a lot of intermediate nodes
slowing down reading.
You can avoid problems related to "losing your place" by using `clone-indirect-buffer`
to create multiple views of your buffer.
`org-sparse-tree` also has the benefit of allowing in-place editing.
`helm` does not support this, but similar types of actions can be achieved in `orgnav` through
`org-capture`.

### worf-goto
*worf* is a convenient navigation tool for org files. It works by providing keybindings for motions that apply when one's cursor is over the leading "*"s of a heading.

*worf* provides a search function `worf-goto`. This allows one to interactively update search terms like `orgnav`.

Searches are global. No means of varying the depth of search is provided. There is no way to chain together searches as for *orgnav*.

### imenu
*imenu* is a general framework for deriving a named list of locations within a file. In source code files, provides a list of function or class definitions. In *org* files it provides a list of headings. As such a general tool, one would not necessarily expect its search model to fit org-mode well.

*imenu* has no concept of depth when searching. Searches are global. There is no way of varying the depth of searches. There is no way of chaining together searches at different levels.

## Caveats

* This software is not very mature
* Operations can be slow for large trees (e.g a file with 13 thousand lines)
* Sometimes pressing keys too quickly can break `helm`.
* The tree is based on character offsets, which can interfere with `helm-resume`. This could be addressed using *olp*s rather than offsets
* Some of the keybindings here almost certainly shadow `helm` defaults
* There is very limited testing at present

## Contributing
Contributions are welcome.

Development happens on the `dev` branch. I have a low threshold for deploying to this branch.
The `master` branch is used for [MELPA](https://melpa.org/) deployment (this branch should probably be called `melpa`).
Every version on *MELPA* should correspond to a release.

### Testing
Testing helm is hard. This may prove to be an issue for accepting contributions, but I can deal with
this if anyone actually contributes anything. It's probably possible to script helm for testing.

You can do some basic linting, and ensure that installation works using `test.sh`.


### Releasing
Use `./bump.sh` and `./release.sh`

Every version released on *MELPA* has a version number. These versions follow [Semantic version numbering](http://semver.org/) in spirit, but I an unwilling to guarantee every part of this.

### My checklist for deploying to MELPA
* Run `test.sh` (this does linting)
* Bump version and run `release.sh`.
* Make `melpa` can build with this version.
* Do some basic manual testing (let this branch sit on my machine for a couple of days)

I intend to automate some of these steps as time continues.

### Releasing
`orgnav-pkg.el` is generated by `cask pkg-file` if you change requirements this may need to be rerun.
This is used with MELPA.