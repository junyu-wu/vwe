
# VWE [vwiss emacs]
> vwe is for myself use configuration, not general configuration.

[![](https://img.shields.io/badge/build-vwiss-green.svg)](https://github.com/junyu-wu/vwe/tree/master)
[![](https://img.shields.io/badge/license-GPL3-red.svg)](https://github.com/junyu-wu/vwe/tree/master)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [VWE](#vwiss-emacs)
    - [Features](#features)
    - [Prerequisites](#prerequisites)
    - [Install](#install)

<!-- markdown-toc end -->

It's geared GNU Linux. but u known, it to also work on OSX and Windows.

---
![](https://raw.githubusercontent.com/junyu-wu/vwe/tree/master/ect/icons/vwemacs-logo.png)

## Features
- It's simple and easy to use.
- Fast start-up.
- Beautiful GUI/TUI.
- Global leader key bindings, automatically switch according to different scenes.
- Support multiple programming languages
   - Assembly
   - C/C++/Object-C
   - C#/Java
   - Python/Ruby/Perl
   - Golang/Rust
   - Shell/Powershell/Bat
   - HTML/CSS/Javascript
   - XML/YAML/JSON
   - ...
- Git integration.
- Support Org/Markdown.
- Support chinese.

## Prerequisites
- The config should run on [Emacs](https://www.gnu.org/software/emacs/ "emacs download") 24.5 or greater.
- To make the most of the programming language-specific support in this config, further programs will likely be required.View dependencies in each file comment.

## Install
1. If existing emacs configuration, back it up first:

	```shell
	cd ~
	mv .emacs.d .emacs.d.bak
	```
2. Clone the repository:

	``` shell
	git clone https://github.com/junyu-wu/vwe.git
	mv vwe .emacs.d

	```
3. Install english(default font), chinese and icon font: (optional)

	``` shell
	cp -r ~/.emacs.d/assets/fonts/ ~/.fonts
	fc-cache -hv
	```
	To install fonts.
	If choice other fonts, please set custemize variable `vwe@custom--font-ascii` `vwe@custom--font-non-ascii`.

4. Run or Restart emacs:
   In the first run, it will checks the dependent packages and installs it.
## Configure
   For more information about vwiss emacs configure, please see `vwe-customize.el`.
