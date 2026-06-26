# Snippy.el

VSCode/LSP snippet support for Emacs with Yasnippet.


https://github.com/user-attachments/assets/659158b0-562a-4d51-8393-70f20d974226


# Table of Contents

- [Introduction](#Introduction)
- [Features](#Features)
- [Installation](#Installation)
- [Configuration](#Configuration)

<a id="Introduction"></a>
# Introduction

Snippy is a snippet translation and completion engine that uses YASnippet under the hood.

This package translates LSP/VSCode-style snippets into YASnippet format, allowing you to use all modern, external snippets seamlessly.

Since YASnippet doesn't fully support the entire LSP specification.

It can optionally fix LSP snippets that Eglot retrieves from servers (e.g., Java), working alongside YASnippet so you can use both simultaneously.

The *Default Snippets collection* is [Friendly Snippets](https://github.com/rafamadriz/friendly-snippets "Default Snippets collection").

<a id="Features"></a>
# Features
[Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#snippet_syntax)
## Automatic mode detection

https://github.com/user-attachments/assets/086be2f5-3d9d-4145-a1ba-bc72f48f83fa

## Tabstops

https://github.com/user-attachments/assets/f08701eb-71ce-4882-b455-8a71f3146833

## Placeholders


https://github.com/user-attachments/assets/2081fd57-a02d-4505-93ea-7eee52858601


## Choices


https://github.com/user-attachments/assets/c430debd-9a1d-4f0f-9085-0a8ad30e12ab


## Variables


https://github.com/user-attachments/assets/8938890f-2b31-4c4d-8332-3110f2707010

## Fix LSP server snippets


Enable: snippy-fix-lsp-snippet-mode


<a id="Installation"></a>
# Installation
```elisp
(use-package snippy
  :vc (:url "https://github.com/MiniApollo/snippy.git"
            :branch "main"
            :rev :newest) ; To get the latest updates
  :hook (after-init . global-snippy-minor-mode)
  :custom
  (snippy-global-languages '("global")) ; Recommended
  ;; Optional
  ;; (snippy-install-dir (expand-file-name <Your location>))
  ;; Use different snippet collections
  ;; (snippy-source '("Your git repo" . "my-snippets-dir"))
  :config
  (snippy-install-or-update-snippets) ; Autoupdate git repo
  ;; (snippy-fix-lsp-snippet-mode) ; Fixes lsp server snippets (e.g java lsp)
  (add-hook 'completion-at-point-functions #'snippy-capf)) ; Or merge them with Yasnippet or eglot capf
```

## Using special characters for snippets like !,@
Prefix characters like !, @ don't work with yasnippet out of the box.

With Snippy it does work, but if you are merging completion backends
make sure <b>snippy is the first capf</b> that dictates boundaries for the completion.

Other completion backends do not include special characters in the prefix.

If you want to use special characters in Yasnippet-snippets use this to fix it:
```elisp
;; This is not needed for Snippy.el
(setq yas-key-syntaxes '("w_" "w_." "^ ")) ; Fixes Yasnippet-snippets special character usage.
```
Now you can use snippets like !cdr which is defined in yasnippet-snippets package.

<a id="Configuration"></a>
# Configuration
## Package Variables
- snippy-install-dir: Directory where to install/clone snippets.
- snippy-source: The source details for downloading snippets (Both needed).
  - Repository URL
  - Directory Name (Cloned repo dir name)
- snippy-global-languages: List of languages to enable globally across all major modes.

You can use any language you like.
```elisp
;; Global Variable names
  :custom
  (snippy-global-languages '("global" "license"))
```

- snippy-emacs-to-vscode-lang-alist: Alist mapping for Emacs major modes to VSCode language names.

You can see all the languages inside the package.json file in the Friendly Snippets [repository](https://github.com/rafamadriz/friendly-snippets "Git Repository").

There are some language snippets that are optional. To use them add it to the list.
You can also add more remaps if any language is missing from the list (or you can just make a pull request).
When adding language remaps the first is the only one that is used in the list.
You also need to include all vscode snippet names not just the new one's name.

Optional remap names:
- (c++-mode . "unreal")
- (c++-ts-mode . "unreal")
- (csharp-mode . "unity")
- (csharp-ts-mode . "unity")
- (html-mode . "djangohtml")
- (html-mode . "htmldjango")
- (web-mode . "angular")
- (web-mode . "twig")
- (python-mode . "django")
- (python-mode . "django-rest")
- (python-ts-mode . "django")
- (python-ts-mode . "django-rest")

Modes that don't have a specific major-mode:
- blade
- eelixir
- rmd

```elisp
;; Choose the remaps you want to use
(add-to-list 'snippy-emacs-to-vscode-lang-alist '(super-cool-mode "coolang" "cool-doc")) ;  Example new dummy-mode

;; Add to an existing mode (You need to include all vscode snippet names not just the new one)
(add-to-list 'snippy-emacs-to-vscode-lang-alist '(c++-mode "cpp" "cppdoc" "unreal"))
(add-to-list 'snippy-emacs-to-vscode-lang-alist '(c++-ts-mode "cpp" "cppdoc"))
```

## Functions
- snippy-install-or-update-snippets: Install or update snippet git repo in snippy-install-dir.
- snippy-refresh-snippets: Force an update on the snippets for the current buffer.
- snippy-expand: Expand snippet by prefix.
- snippy-expand-snippet: Expand VSCode snippet body.
- snippy-capf: Complete with snippy at point.

## Modes
- snippy-minor-mode: Toggle snippy in the current buffer.
- global-snippy-minor-mode: Toggle snippy in all buffers.
- snippy-fix-lsp-snippet-mode: Toggle LSP server snippet fix globally.

