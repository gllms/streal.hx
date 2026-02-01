# streal.hx

A [Helix](https://github.com/helix-editor/helix/) plugin to bookmark files and quickly switch between them using numbers. Every working directory (and [optionally](#configuration) every Git branch) has its own distinct list. Heavily inspired by [otavioschwanck/arrow.nvim](https://github.com/otavioschwanck/arrow.nvim).

Feedback and ideas are very much welcome, feel free to [create a new issue](https://github.com/gllms/streal.hx/issues) and share your thoughts.

> [!WARNING]  
> Critical bugs and breaking changes may occur at any time, especially while this plugin is pre 1.0. Use at your own risk.

<img width="441" height="266" alt="Screenshot" src="https://github.com/user-attachments/assets/683a8b9a-9ded-475a-9a8d-8939b92cb008" />

## Usage

First, open the streal popup by using the `:streal-open` typable command or a hotkey you bound to it (see [Installation](#installation)), for example <kbd>\\</kbd>. This will show the list of files for the current working directory. Now you can use any of the following hotkeys to interact with it:

| Key                          | Description               |
| ---------------------------- | ------------------------- |
| <kbd>s</kbd>                 | Add / remove current file |
| <kbd>1</kbd>..<kbd>9</kbd>   | Open file                 |
| <kbd>Esc</kbd>, <kbd>q</kbd> | Close popup               |
| <kbd>C</kbd>                 | Clear list                |
| <kbd>h</kbd>                 | Open in horizontal split  |
| <kbd>v</kbd>                 | Open in vertical split    |
| <kbd>d</kbd>                 | Delete mode               |
| <kbd>e</kbd>                 | Edit current Streal file  |
| <kbd>?</kbd>                 | Show keymap               |

In delete mode, activated by pressing <kbd>d</kbd>, any number you press will remove that file from the list. The popup will stay open, so you can easily remove multiple files this way.

You can quickly edit the list by pressing <kbd>e</kbd>. This will open the Streal file in a buffer so you can add, remove and reorder paths at your will.

## Installation

0. Make sure your Helix version is compiled with the Steel plugin system, see [here](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md) for instructions.
1. You can then install this plugin using the Forge package manager:

   ```sh
   forge pkg install --git https://github.com/gllms/streal.hx.git
   ```

2. Add the following line to your `init.scm`:

   ```scheme
   (require "streal/streal.scm")
   ```

3. Now the `:streal-open` typable command will be available. You can bind this to a key in your `config.toml`. For example, to bind it to <kbd>\\</kbd>:

   ```toml
   [keys.normal]
   "\\" = ":streal-open"

   [keys.select]
   "\\" = ":streal-open"
   ```

## Configuration

To configure the plugin you can append the following command flags to the `:streal-open` command:

| Command flag   | Description                                                                                                                                                             |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `--per-branch` | Keep a separate Streal file per Git branch. If no branch can be found, Streal will silently fall back to the default behaviour of using one file per working directory. |

For example:

```toml
[keys.normal]
"\\" = ":streal-open --per-branch"
```
