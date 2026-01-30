<div align="center">
# Snippy.el
</div>

# Table of Contents

- [Introduction](#Introduction)
  - [Features](#Features)
  - [How it was made](how-developed)

<a id="Introduction"></a>
# Introduction

VSCode snippets support for Emacs with Yasnippet.

Translates VSCode snippets and expands them with Yasnippet.
Default Snippets collection is Friendly Snippets

## Installation
```elisp
(use-package snippy
    :custom 
    (snippy-global-languages '("global"))
    ()
    :config 
)
```

<a id="Features"></a>
## Features
### Tabstops
### Placeholders
### Choice
### Variables

## Configuration
### Package Variables
- snippy-install-dir: Directory where to install/clone snippets.
- snippy-source: The source details for downloading snippets (Both needed).
  - Repository URL
  - Directory Name
- snippy-global-languages: List of languages to enable globally across all major modes.
- snippy-emacs-to-vscode-lang-alist: Alist mapping Emacs major modes to VSCode language identifiers.

### Functions
- snippy-install-or-update-snippets: Install or update snippet git repo in snippy-install-dir.
- snippy-refresh-snippets: Force an update on the snippets for the current buffer.
- snippy-expand: Expand snippet by prefix.
- snippy-expand-snippet: Expand VSCode snippet body.
- snippy-capf: Complete with snippy at point.

### Modes
- snippy-minor-mode: Toggle snippy in the current buffer.
- global-snippy-minor-mode: Toggle snippy in all buffers.

<a id="how-developed"></a>
## How it was made
