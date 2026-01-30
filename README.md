# Snippy.el

VSCode/LSP snippet support for Emacs with Yasnippet.


https://github.com/user-attachments/assets/659158b0-562a-4d51-8393-70f20d974226


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

## Why use this:
Snippets are underdeveloped in Emacs:

Yasnippet and Tempel don't have a well maintained snippet collections.

Both use their own custom format. I want to use snippets that everybody uses not just Emacs users.

Emacs has a developer problem. NeoVim is very well maintained compared to Emacs.


<a id="Features"></a>
# Features
[Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#snippet_syntax)


## Tabstops and Placeholders

https://github.com/user-attachments/assets/2081fd57-a02d-4505-93ea-7eee52858601

## Automatic mode detection and choices

https://github.com/user-attachments/assets/086be2f5-3d9d-4145-a1ba-bc72f48f83fa

## Variables

https://github.com/user-attachments/assets/8938890f-2b31-4c4d-8332-3110f2707010


<a id="Installation"></a>
# Installation
```elisp
(use-package snippy
    :vc (:url "https://github.com/MiniApollo/snippy.git"
              :branch "main")
    :hook (after-init . global-snippy-minor-mode)
    :custom 
    (snippy-global-languages '("global")) ;; Recomended
    ;; Optional 
    ;; (snippy-install-dir (expand-file-name <Your location>))
    ;; Use different snippet collections
    ;; (snippy-source '("Your git repo" . "my-snippets-dir")) ;; Optional
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

