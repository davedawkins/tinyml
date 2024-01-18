## TinyML

> [!TIP]
> 18-Jan-24: Examples have been updated (see [Updates]) - use `Reset` to reload

This is a follow-on project based on Tomas Petricek's Tiny Systems course:

https://d3s.mff.cuni.cz/teaching/nprg077/

The programming tasks for TinyML are here:

https://github.com/tpetricek/tiny-systems/tree/master/01-tiny-ml/tasks

The inspiration for this project came from completing these tasks, using a Jupyter Notebook with the F# extension. 

I wanted to be creative with this tiny system, and so created a parser. I added the Monaco editor, syntax highlighting, an in-browser file explorer (using browser local storage), and an AST explorer. No server is required, the whole environment runs in the browser. Everything is written in F#, using Fable, and the UI is [Sutil](https://sutil.dev).

The repo is [TinyML](https://github.com/davedawkins/tinyml) and is hosted at https://tinyml.sutil.dev.

It adds a parser and basic IDE for the TinyML language, allowing you to explore the capabilities of this powerful language. It's extraordinary what can be expressed with such a small implementation.

You will see example programs in the File explorer that you can load, compile and run. You can see examples of

- variable assigment
- basic arithmetic
- conditionals
- recursion
- currying
- partial application
- higher-order functions
- pattern matching
- sum types
- product types
- lists

Feel free to edit these yourself, and to create your own programs.

## Example Code

A few random snippets:

```fs
let rec fac n = if n = 1 then 1 else n * fac (n - 1)

let pair x y = x,y
let fst t = t#1
let snd t = t#2

let pairWith x = pair x

let ignore x = ()
let compose f g = fun x -> f (g x)

print (fac 5)
```

## About The Parser

I'm using Parsec.fs, a nearly complete API-compatible port of FParsec to Fable `cannorin` (see Links).

I had to learn how to parse indentation sensitive syntax, and I found this by the FParsec author to be
invaluable.

Error reporting is something I'm still learning how to improve. 

The parser is modeled on F#.

## About the Examples

Many `.tml` files are loaded upon first visit to the page, and stored in browser local storage. Any edits you make 
subsequently are on your local copies. You can `Download` a backup of these files as a `.zip` if you end up making
your own edits.

The examples consist of a single commented `.tml` file per feature of TinyML. Each should compile and run without error.

I find it amazing that you can express all these features in this tiny implementation.

The `tests.cfg` contains (woefully few) tests that will be executed when the `Tests` button is pressed.

## Error Messages

Improving the error messages is an art I have yet to master. Parser combinators are a relatively new skill for me and I
have yet to find the pattern that allows me to give concise messages, linking to the exact site of the issue. 

Any messages that are produced are indicated with red wavy markers in the source file. You can hover to see the error, and 
click `View Problem` to browse the message detail.

## About the UI

The UI is built with [Sutil](https://sutil.dev). The main areas in the UI are

- menu bar
- file manager
- editor
- AST viewer
- message log

### Menu Bar

- Clear       Clear all log messages
- Reset       Reload original example files
- New         Create a new file (prompt for file name)
- Download    Download all files as a `.zip`
- Save        Save current file being edited
- Tests       Run tests in `tests.cfg`
- Compile     Compile current file (`.tml` files compiled automatically upon load)
- Run         Run currently loaded file (if compiled).
- Debug       Enable parser debugging. Check and recompile to see output. Masochists only.

### Dock Panes

The dock panes can be hidden/restored by clicking on their tabs.

You can drag pane tabs to different locations, or use the drop-down menu in the pane's header.

All panes except the main central pane will hide when the browser width drops below a certain size.

### Files Pane

Upon start-up, example files are loaded into the UI. The file system is stored in your browser's local storage.

Files with `.tml` extension are TinyML source files. These can be loaded and run. Feel free to edit these, or create your
own the the `New` menu button.  Files that successfully compile will display the syntax tree in the AST window

You can download your source files with the `Download` button.

Reload the example files with the `Reset` button - this will overwrite any files you edited. Any files you created will be unaffected.

### Message Log Pane

This shows program output, compiler messages etc. You can use `Clear` button to remove all messages (in menu bar and on header of message log)

### Editor Pane

Double-click a file to start editing it.

Control-S or `Save` will save your edits.

Changes are saved automatically when opening a different file.

### AST Pane

Source files are compiled when loaded, and when you click `Compile` in the menu bar. Successful compilations show the syntax tree in the AST viewer.

Clicking on a node in viewer will show the corresponding the source token in the editor (with a wavy blue marker).

## About The Editor

This is Monaco. I've added

- syntax highlighting 
- type signatures on hint
- error underlining

## Follow-on Projects

While playing with this project, I had the following ideas I was unable to resist tinkering with:

- Compile to Javascript, with source maps (I have a prototype of this)
- Added JSX as a native expression (I have a prototype of this too)
- Extend type system (add strings, proper DUs, records)
- Introduce reactive expressions as a first-class concept (think Svelte)

## Updates

18-Jan-24: 
- No longer need parens around match expressions (updates to: `indent.tml`, `list.tml`)
- README typos, example snippets

## Links

[Tiny Programmming Systems](https://d3s.mff.cuni.cz/teaching/nprg077/)

[TinyML Tasks](https://github.com/tpetricek/tiny-systems/tree/master/01-tiny-ml/tasks)

[Fable](https://fable.io)

[Monaco Editor Playground](https://microsoft.github.io/monaco-editor/playground.html)

[Parsing indentation based syntax with FParsec](https://github.com/stephan-tolksdorf/fparsec/wiki/Parsing-indentation-based-syntax-with-FParsec)

[Parsec.fs](https://github.com/cannorin/Parsec.fs)

[Sutil](https://sutil.dev)

## Contact

Bugs, suggestions, questions:

@DaveDawkins (Twitter)
david.dawkins@gmail.com
