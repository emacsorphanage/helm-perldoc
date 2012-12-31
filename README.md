# helm-perldoc.el

## Introduction
`helm-perldoc.el` is `perldoc`, Perl documentation command, with helm interface.


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher
* Perl


## Basic Usage

Input search word

    M-x helm-perldoc


## Actions

* View Document
* View Source code
* Insert import statement
* Check with `corelist`


## Setup

```` elisp
(add-hook 'cperl-mode-hook 'helm-perldoc:setup)
````
