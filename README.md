# helm-perldoc.el

![helm-perldoc](image/helm-perldoc1.png)


## Introduction
`helm-perldoc.el` is `perldoc`, Perl documentation command, with helm interface.


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher
* Perl


## Basic Usage

#### helm-perldoc

Search module and do action.


## Actions

* View Document
* View Source code
* Insert import statement
* Check with `corelist`


## Setup

````elisp
;; helm-perldoc:setup takes long time on low power platform
(add-hook 'cperl-mode-hook 'helm-perldoc:setup)
````

or

```` elisp
;; use async
(run-at-time 0 nil 'helm-perldoc:setup)
````
