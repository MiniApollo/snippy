# Snippy.el

VSCode/LSP snippet support for Emacs with Yasnippet.

# Table of Contents

- [Introduction](#Introduction)
- [Features](#Features)
- [Installation](#Installation)
- [Configuration](#Configuration)
- [How it was made](how-developed)

<a id="Introduction"></a>
# Introduction

Translates VSCode/LSP snippets and expands them with Yasnippet.

Perfect for users who find yasnippet-collection too limited and want access to the vast ecosystem of modern editor templates.

Default Snippets collection is [Friendly Snippets](https://github.com/rafamadriz/friendly-snippets "Default Snippets collection")

<a id="Features"></a>
# Features
Specification:
https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#snippet_syntax

## Tabstops

## Placeholders

## Choice

## Variables

<a id="Installation"></a>
# Installation
```elisp
(use-package snippy
    :hook (after-init . global-snippy-minor-mode)
    :custom 
    (snippy-global-languages '("global")) ;; Recomended
    ;; Optional 
    ;; (snippy-install-dir (expand-file-name <Your location>))
    ;; Use different snippet collections
    ;; (snippy-source '("Your git repo" . "my-snippets-dir"))) ;; Optional
    :config 
    (snippy-install-or-update-snippets) ;; Autoupdate git repo
    (add-hook 'completion-at-point-functions #'snippy-capf)) ;; Or merge them with Yasnippet or eglot capf
```

<a id="Configuration"></a>
# Configuration
## Package Variables
- snippy-install-dir: Directory where to install/clone snippets.
- snippy-source: The source details for downloading snippets (Both needed).
  - Repository URL
  - Directory Name (Cloned repo dir name)
- snippy-global-languages: List of languages to enable globally across all major modes.
- snippy-emacs-to-vscode-lang-alist: Alist mapping Emacs major modes to VSCode language identifiers.
Check languages in Friendly Snippets [repo](https://github.com/rafamadriz/friendly-snippets "Git Repository")

## Functions
- snippy-install-or-update-snippets: Install or update snippet git repo in snippy-install-dir.
- snippy-refresh-snippets: Force an update on the snippets for the current buffer.
- snippy-expand: Expand snippet by prefix.
- snippy-expand-snippet: Expand VSCode snippet body.
- snippy-capf: Complete with snippy at point.

## Modes
- snippy-minor-mode: Toggle snippy in the current buffer.
- global-snippy-minor-mode: Toggle snippy in all buffers.

<a id="how-developed"></a>
# How it was made
This package was made as a challenge to create an Emacs package as fast as possible.

AI was heavily used. So there might be some weird/bad code here and there :P

This package was made in two days. I think it turned out really good.
Enjoy.

