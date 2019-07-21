Twidlk
------

A CLI for modifying twiddler configurations.

If you want to make your own layout, or tweak an existing one, you can drop this text represenation of the layout in the repo, and actually track changes usefully rather than using the binary blob twiddler config.

Or if you want to compare two layouts, you can convert them to text and use standard text diff tools to see how they differ.

### Building

I heavily use [NixOS and the nix package manager](https://nixos.org/), so if you're willing to use that, you can just `nix-build` or `nix-env -f default.nix -i` to build it or install it.

Otherwise, as a Haskell project, it should build with [Cabal](https://www.haskell.org/cabal/); I'd expect [the standard instructions](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package) for installing packages manually to work once you grab the source.

I'm willing to attempt to provide reasonable binary downloads as well on request.

### Usage

So far, all this does is take a file containing either a twiddler "v5" binary configuration file or a twidlk "version 0" text description of a twiddler layout, and produce the other one with the appropriate extension.

So if you give it a "twiddler.cfg", from the tuner, it'll produce a "twiddler.txt" containing editable text, and if you give it "mylayout.txt", it'll product "mylayout.cfg" that can be loaded onto a twiddler or into the official Tuner.

As a minor safety feature, it will refuse to overwrite a newer file; e.g., if you edit twiddler.txt, then attempt to `twidlk twiddler.cfg`, it won't overwrite your changes.

And note that error handling is minimal; it will generally explode if things go wrong, but probably not in a way that's obviously helpful.  Feel free to file an issue on Github if you get a weird error; that will help me prioritize which ones should be handled.

### File Format ("version 0")

Right now, the text file format is designed to be simple but usable.  Each line is a pair of values; either a "flag" and value, or a twiddler chord and text output.

The simplest thing to do is probably to just take an existing twiddler.cfg, convert it to text, and see what it looks like; it should be reasonably comprehensible.  But to help, here's a basic description of what the file looks like:

#### Flags

A "flag" line is just a string describing one of the flags on the twiddler; these should all be pretty self explanatory.  Note that "version 0" is a special flag indicating the twidlk file version, and to enable maximum future flexibility, the first line of the file must be exactly the string "version 0".


#### Chords

A chord line has two space-separated strings; the first is the chord, and the second is the string to print.

A chord uses the standard twiddler notation: optionally, one or more modifiers (NACS, for Num, Alt, Ctrl, and Shift) and a plus, followed by four letters that are each either "L", "M", "R", or "O", representing the Left, Middle, Right, and empty key pressed in each of the four rows.

In addition, multiple keys in one row are supported via parenthesis.  For example, `(LM)OOO` represents the left and middle keys of the top row; and `(LM)MM(LR)` represents the (practically impossible to press) chord of the left and middle keys of the top row, the middle keys of the second and third rows, and the left and right keys of the bottom row.

#### Output sequences

Output sequences are a bit more complex.

Most characters represent themselves: "s" will send an "s", and "&" will send an "&".  However, due to how the USB HID codes work, "&" will actually send "shift-7", using the right shift key.  This usually shouldn't be an issue, but it might come up.  Moreover, if you have a chord mapping to a lowercase letter, shift plus the chord will default to the capital letter.

For non-printable or modified characters, angle brackets are used: `<space>` is the spacebar, `<return>` is the return key, etc.  To modify a key, put it in angle brackets with the modifer(s) and a dash: "C" for Control, "S" for shift, "A" for alt, or "4" for the Windows/Super/Apple key.  (I use Linux.  It's modifier 4 there, so that's what I picked.)  By default, these use the left modifier; prepend "R" to use the right modifier.

So `<C-s>` is control-s, `<4-enter>` is Windows-enter, `<RA-">` is right-alt double quote, and `<CRS-backspace>` is control right-shift  backspace.

To send a literal `<`, use `\<`; otherwise it would be interpreted as the start of a longer sequence.  And note that `<C->>` does work as expected; it sends control-greater-than, though it looks kind of funny.

Multiple character chords just concatenate the characters: `st` sends `s` followed by `t`; `<escape>:wi` sends escape, colon, w, and i.  Note that modifiers only modify a single character: to send control-x followed by control-s, use `<C-x><C-s>`.  If you used `<C-x>s`, the s would not have the control modifier.

As a side note, the official twiddler tuner uses RS by default for symbols that are automatically shifted; twidlk follows this so that converting a config file from the tuner to text and back to the binary config is identical to the original file.  (E.g., `&` is equivalent to `<RS-7>`; it could just as well be `<S-7>`)

### Vision

This is a very minimal tool at the moment; based on my experience with [grit on putty](http://gritonputty.kdf.sh/), I may want to spend more time on things before releasing them, but people may find them useful in the mean time, and if people are using something, I'm (somewhat) more motivated to improve it.  But here's where I'd like to go with this:

As a CLI, there are a number of things I see as simple usability things:

- better error handling in the tool itself; right now it'll just explode in a not very useful way if a file is malformed.  It should provide more useful error messages.

- More flexible command line arguments: e.g., a `-o` option to give an output filename or a flag to specify the file type.

I also want to add more features:

- In-line modification: in some cases it might be nicer to change flags via the CLI instead of as a text file; e.g., `twidlk twiddler.cfg --key OOMO=T --stickyShift=true`.  Not sure how useful this is, but it seems like it could come in handy.

- Validation: e.g., right now, if a chord is mapped to two different strings, the last one will win; this should be an error.  Or if should be possible to specify (perhaps via the below config file) a set of characters that should be in the layout, and the program will warn you if any are missing.  (E.g., you probably want the whole alphabet in the layout.  If you're missing a letter, the tool should let you know.)

- Generated MCC's: it would be nice to be able to simply say that pressing both "s" and "t" maps to "st"; right now they're three independent chords, so if you move "s", you have to remember to change "st" as well.  Rather than saying "ROMO st", it would be nice to just say "st st".

- Generated outputs: Some method to generate chords from existing ones; e.g., alphabetical chords should get an automatic shifted version: if "ROMO" maps to "st", then "S-ROMO" should map to "St".  This would help avoid silly errors.

- "Box diagrams": I like the compactness and overall view you get with a sort of ["box diagram"](https://github.com/AlexBravo/Twiddler/blob/master/Backspice2%20cheat%20sheet.txt).  It would be nice to generate these automatically to ensure they're in sync with the actual layout.  It would also be nice to be able to edit them and get a layout file back, though that seems significantly more difficult.

- Config file: With the above few features, it makes sense to have a configuraiton file describing the layout metadata; what files hshould be generated, which one is the source of truth, what validation should be enabled, what extra chords should be generated, etc.  So there should be an option to read a configuration file that can be stored in the repo along with the layout itself.

Once all (or some) of this is in place, it could also be interesting to set up an automated build system to make layouts more easily available; e.g., a github integration could watch for changes to a layout repo, use the config file to build the binary twiddler.cfg automatically, and put it in a shared space.  Then other tools (like [grit on putty](http://gritonputty.kdf.sh/)) could pick up these layouts and make them easily available; imagine being able to just edit a text file, push a repo, download the twiddler config to your twiddler, and go to grit on putty to practice it.

This is probably overkill, but it's the sort of integrated project I've been looking for an excuse to build.  It probably won't happen soon, but I may get to it eventually.

### Why not dido?

[Dido](https://github.com/CoohLand/Dido) looks like a fine tool, but it is currently only available as an OSX binary.  Not the friendliest.

And I didn't use Dido's file format because I forgot it existed when I started writing this, and made my own trivial format instead.  However, it looks like the dido format is pretty much a direct translation of the twiddler file format, including USB HID codes and pointers into the string table.  So I'm fine making a new format that's a bit more user-friendly.

It does look like it would be pretty easy to add a parser and generator for Dido's file format, though, if that's something that looks useful.
