# helm-perldoc.el

![helm-perldoc](image/helm-perldoc1.png)


## Introduction
`helm-perldoc.el` is `perldoc`, Perl documentation command, with helm interface.


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher
* [deferred.el](https://github.com/kiwanami/emacs-deferred)
* Perl
* [ExtUtils::Installed](https://metacpan.org/module/ExtUtils::Installed)


## Basic Usage

#### `helm-perldoc`

Search module and do action.

#### `helm-perldoc:history`

Search modules which is already searched

## Customize

#### `helm-perldoc:perl5lib`

`PERL5LIB` envirnoment variable which is set when `perl` and `perldoc`
commands are executed.


## Actions

* View Document
* View Source code
* Insert import statement
* Check with `corelist`


## Setup

```lisp
;; helm-perldoc:setup takes long time on low power platform
(add-hook 'cperl-mode-hook 'helm-perldoc:setup)
```
