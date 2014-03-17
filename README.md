# helm-perldoc.el

![helm-perldoc](image/helm-perldoc1.png)


## Introduction
`helm-perldoc.el` is `perldoc`, Perl documentation command, with helm interface.


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher
* [deferred.el](https://github.com/kiwanami/emacs-deferred)
* Perl


## Installation

You can install `helm-perldoc` from [MELPA](http://melpa.milkbox.net/) with package.el.


## Basic Usage

#### `helm-perldoc`

Search module and do action.

#### `helm-perldoc:setup`

Update module list for viewing documenations.

#### `helm-perldoc:carton-setup`

Update module list based [Carton](https://github.com/miyagawa/carton).
Local library path is decided on `helm-perldoc:default-carton-path`.

#### `helm-perldoc:history`

Search modules which is already searched

## Customize

#### `helm-perldoc:perl5lib`

`PERL5LIB` envirnoment variable which is set when `perl` and `perldoc`
commands are executed.

#### `helm-perldoc:default-carton-path`(Default `local/lib/perl5`)

Default carton library path.


## Actions

* View Document
* View Source code
* Insert import statement
* Check with `corelist`


## Setup

```lisp
;; helm-perldoc:setup takes long time on low power platform
(eval-after-load "cperl"
  '(progn
    (helm-perldoc:setup)))
```
