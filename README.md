# renamer

A simple command-line renaming tool written in Guile Scheme. The
primary consideration in the design is the facility of adding new
operations. Inspired by Karl Voit's [date2name](https://github.com/novoid/date2name).

# Features

Multiple renaming operations:

- Add date stamp.
- Replace white spaces with the '-' character.
- Lowercase filename.
- Remove punctuation characters.
- Remove arbitrary text using a regular expression.
- Rename a PDF file using the file's metadata (requires pdftk or stapler). 
- Add duration time for media file formats (requires ffmpeg). 

# Requirements

[Guile Scheme](https://www.gnu.org/software/guile/) 2.2+

# Installation

## Manual

For convenience I'm including the configure script (this is probably
not the best practice). To create the configure script autotools is
required.

```console
git clone https://github.com/MirkoHernandez/renamer
cd renamer
autoreconf -vif
./configure
make
sudo make install
```

# Usage

renamer uses regular expressions. To provide a list of literal
filenames use the quote-rx option.

```text
Usage: 
   renamer [options]  filenames.

Renamer runs in dry run mode,  use -A  to apply changes.

Find files:
	-D, --directory           Where to search for files.
	-r, --recursive           Recursive search.

Date Operations:
	-d, --datestamp           Include date stamp.
	-c  --compact             use compact datestamp (YYYYMMDD).
	    --month               use datestamp with year and month (YYYYMM).
	    --ctime               use creation time (the modification of the file attributes) for the datestamp.
	    --atime               take last access time for the date stamp. 
	    --remove-datestamp    remove date stamp.

Text Operations:
	    --remove-text         remove text.
	-w  --whitespace          replace whitespace with the '-' character.
	-p  --remove-punctuation  remove punctuation characters.
	    --separator           Include separator before specified text.

External Program Operations:
	    --pdf                 rename using pdftk or stapler (by default the metadata's title).
	    --title               option to add title.
	    --author              option to add author.
	    --pages               option to add pages.
	    --duration            add time duration to a media file.
General Options:
	-h  --help                Display usage information.
	    --no-color            Do not colorize output.
	    --quote-rx            Quote regular expression characters in filenames.
	i   --ignore-rx           Ignore the files matching a regular expression.
	    --omit-ignore-files   If there is an ignored file in a directory, omit it.
```

# Examples

#### Replace whitespaces and lowercase text.

```console
$ renamer  -lwA "Songs on Fire Jim Guthrie - All Gone.mkv"
Songs on Fire Jim Guthrie - All Gone.mkv → songs-on-fire-jim-guthrie-all-gone.mkv
```

#### add date stamp

```console
$ renamer -dA Readme.md 
Readme.md                       → 24-02-23--Readme.md

$ renamer -cA Readme.md 
Readme.md                       → 20240223--Readme.md

$ renamer --month -A Readme.md 
Readme.md                       → 2024-02--Readme.md
```

#### remove text using regular expression.

```console
$ renamer  --remove-text -A "•|[.*" "Songs on Fire • Jim Guthrie - All Gone \[4G-oaBUD7xE\].mkv"
Songs on Fire • Jim Guthrie - All Gone [4G-oaBUD7xE].mkv → Songs on Fire  Jim Guthrie - All Gone .mkv
```

#### rename PDF using metadata

The default PDF rename operation is to use the metadata's title.

```console
$ renamer --pdf -A elementsdrawing02ruskgoog.pdf 
elementsdrawing02ruskgoog.pdf   → The Elements of Drawing.pdf
```
#### rename PDF using metadata - title and author.

The options for PDF renaming are --title --author --pages.

```console
$ renamer --pdf --title --author   -lwcA elementsdrawing02ruskgoog.pdf 
elementsdrawing02ruskgoog.pdf   → 20070718--the-elements-of-drawing-john-ruskin.pdf
```

#### Add duration to a media file.

This example requires the --quote-rx option because the filename
includes characters that are interpreted as part of a regular
expression.

```console
$ renamer --duration --quote-rx -A  "The Magic of Property Testing [4bpc8NpNHRc].webm"
The Magic of Property Testing [4bpc8NpNHRc].webm → The Magic of Property Testing [4bpc8NpNHRc]_00:11:57.webm
```

# Integration

For the most part renamer is not meant to be used from the command
line; here are some examples of different ways to execute the renaming
operations.

## Desktop Entries

Desktop entries are files that can be created in
~/.local/share/file-manager/actions, they provide file managers with
additional context menu actions.

### rename-lwp.desktop

```config
[Desktop Entry]
Type=Action
Name=rename-lwp
Profiles=rename-lwp;

[X-Action-Profile rename-lwp]
MimeTypes=*;
Exec=renamer -wlpA --quote-rx %F
```

### rename-separator.desktop

```config
[Desktop Entry]
Type=Action
Name=rename-separator
Profiles=rename-separator;

[X-Action-Profile rename-separator]
MimeTypes=*;
Exec=bash -c 'renamer -A --separator $(rofi -dmenu -p "After the text:") --quote-rx %F'
```

### rename-pdf.desktop

```config
[Desktop Entry]
Type=Action
Name=rename-pdf
Profiles=rename-pdf;

[X-Action-Profile rename-pdf]
MimeTypes=text/*;application/*;
Exec=renamer -A --pdf  --quote-rx %f
```

## Rofi integration

The following script uses rofi (or fzf using the 'fzf' argument) to select files and renaming
operations.

NOTE: it is unresponsive when using PDF operations on too many files.
I haven't figured how to add a loading icon to rofi.

```bash
#!/usr/bin/env bash
command=(rofi -show -dmenu -i -theme-str 'window{width:100%;}'  -fuzzy -font "hack 9" )
command_ms=(${command[@]} -multi-select)

if [ "$1" == "fzf" ]; then
    command=(fzf ) 
    command_ms=( ${command[@]}  -m)
fi

# Select directory
directory_list=("$HOME/Downloads"
		"$HOME/Media"
		"$HOME/Music"
		"$HOME/Books")
		
directory=$(printf "%s\n" "${directory_list[@]}" | ${command[@]})

# NOTE: all the arrays created in this script use \n as the separator to allow spaces in file names, so IFS is not restored.
SAVEIFS=$IFS 
IFS=$'\n'
# Array of select files
files=($(find "$directory" -maxdepth 1 -type f | ${command_ms[@]}))

if (( ${#files[@]} == 0 )); then
   exit 0 
fi

# Renamer options
declare -A options;
options[datestamp]="-d" 
options[compact-datestamp]="-c" 
options[month]="-m" 
options[lowercase]="-l"
options[pdf]="--pdf" 
options[title]="--title" 
options[author]="--author" 
options[pages]="--pages"
options[duration]="--duration"
options[replace-whitespace]="-w"
options[remove-punctuation]="-p"

# Array of select options
selected_options=($(printf "%s\n" "${!options[@]}" | ${command_ms[@]}))

declare -a flags;
for i in "${selected_options[@]}"; do
   flags+=(${options[$i]})
done

if (( ${#flags[@]} == 0 )); then
   exit 0 
fi

# Array of possible changes
possible_changes=($(renamer "${flags[@]}"  --quote-rx --no-color "${files[@]}" ))

if (( ${#possible_changes[@]} == 0 )); then
   exit 0 
fi

# Array of selected changes
apply_changes=($(printf "%s\n" "${possible_changes[@]}" | ${command_ms[@]} ))

declare -a renamed_files;
if  (( ${#apply_changes[@]} != 0 )); then
    for i in "${!possible_changes[@]}"; do
	if [[ ${apply_changes[@]} =~ ${possible_changes[i]} ]]
	then
	    renamed_files+=("${files[$i]}")
	fi
    done
    
    renamer "${flags[@]}" -A  --quote-rx --no-color "${renamed_files[@]}"
fi
```
