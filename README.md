https://github.com/user-attachments/assets/4b0297ae-f6ca-4064-b90b-f8dc320cf83a

Diogenes.el strives to be a complete interface to P. Heslin's
Diogenes, allowing its users to browse and search the TLG and PHI
Greek and Latin databases from within Emacs. In addition to this, it
also can interactively display the lexicographical material that comes
with Diogenes (the LSJ Greek and Lewis & Short Latin dictionaries), as
well as use its rich morphological databases to analyse Greek and
Latin forms.

This package is intended to be useful both as user facing program and
as a LISP library. At the moment, however, there is no public API yet,
but that should change soon. All functions that are intended to be
used in other LISP programs will be described either in this document
or in the info manual that should be written at some point.

Unlike the previous version of this package, it now uses custom Perl
scripts to communicate with Diogenes' Perl API and tries to cover it
entirely.


# Prerequisites

This package has been developed on Linux and GNU Emacs 29. It works on
Mac OS,too, but I have not yet had the time to make it work on
Windows, too (but it works fine using WSL2). It should run on earlier
Emacsen, too, but I haven't tested it yet.


# Installation

Please make sure that you have a working installation of Diogenes.
After that, clone this repository (e.g. into \`~/.emacs./elisp\`) and
add it to your load-path:

    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/diogenes.el"))

Now require it and set the path to the diogenes-libary-path variable
to the root of your Diogenes installation:

    (require 'diogenes)
    (setq diogenes-library-path "/path/to/diogenes)

Or, with use-package (with some handy key-bindings)

    (use-package diogenes
      :init
      (diogenes-path "/path/to/diogenes")
        :bind (("C-c d" . diogenes))
        :commands (diogenes-ad-to-ol
                 diogenes-ol-to-ad
                 diogenes-utf8-to-beta
                 diogenes-beta-to-utf8))


# Transient interface

Nearly all of the functionality of this package (besides the utility
functions, see below) is exposed via its new transient user interface,
which can be invoced with the `diogenes` command. Note that this new
interface is still experimental, so expect some rough edges.


# Usage

As there is a plethora of corpora that Diogenes can search and browse,
diogenes.el defines for each of these copora in each of the following
categories a specialised command. In order to avoid redundancy, the
placeholder CORPUS will be used in command names that are defined for
each of the following corpora:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Abbreviation</th>
<th scope="col" class="org-left">Full Name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">tlg</td>
<td class="org-left">Thesaurus Lingae Graecae</td>
</tr>


<tr>
<td class="org-left">phi</td>
<td class="org-left">PHI Latin Texts</td>
</tr>


<tr>
<td class="org-left">ddp</td>
<td class="org-left">Duke Documentary Papyri</td>
</tr>


<tr>
<td class="org-left">ins</td>
<td class="org-left">Classical Inscriptions</td>
</tr>


<tr>
<td class="org-left">chr</td>
<td class="org-left">Christian Inscriptions</td>
</tr>


<tr>
<td class="org-left">misc</td>
<td class="org-left">Miscellaneous PHI Texts</td>
</tr>


<tr>
<td class="org-left">cop</td>
<td class="org-left">PHI Coptic Texts</td>
</tr>
</tbody>
</table>


## Searching the Corpora

The command `M-x diogenes-search-CORPUS` starts a search in a corpus.
You can narrow down the scope of the search to individual authors. At
the moment, the search result produced by Diogenes is inserted without
any processing; this should however change soon.

When used with a prefix argument (e.g. `C-u M-x
diogenes-search-CORPUS`), a more complex search query can be
constructed. Note, however, that this interface is perliminary and is
likely to change in the near future.


## Browsing the Corpora

There are two sets of commands that display the text of a specific
work. The first one, `diogenes-dump-CORPUS`, just dumps itvin its
entirety into a dedicated buffer without any post processing. This is
primarily intended if you plan to use this to construct larger corpora
of plain text files that should be used by other programs. Note,
however, that due to some limitations in the current implementation,
Diogenes prints some lines beyond the end of the requested work. At
the moments, these must be removed manually. Other post-processing
tasks include the deletion of the integrated citations with a
rectangle command, or the removal of the hyphenations that interfere
with searches. Some utilities for these tasks can be found in
diogenes-legacy.el, but should be replaced in the near future.

The second command, `diogenes-browse-CORPUS`, opens an interactive
browser at a specified location in the corpus. It post processes the
output so that every line gets its own citation, and puts the citation
even in its text properties so that it will be preserved even if it is
copied elsewhere (use `M-x describe-text-properties` to inspect it).
You can browse forward and backward with `C-c C-n`
(diogenes-browser-forward) and `C-c C-p` (diogenes-browser-backward),
or simply by reaching the beginning or end of the buffer and using the
arrow keys to go beyond the boundaries of the current buffer. 

Additionally, there are commands in the browser mode that facilitate
the post processing of the texts. `diogenes-browser-toggle-citations-
(bound to ~C-c C-t`) removes or reinserts all citations from the
buffer. `diogenes-browser-remove-hyphenation` (bound to `C-c C--`)
joins all hyphenated words at the line-ends, while
`diogenes-browser-reinsert-hyphenations` (`C-c C-+`) restores them to
their original form.


## Parsing and Dictionary Lookup

The command `diogenes-lookup-greek` and `diogenes-lookup-latin` search the
LSJ Greek Dictionary and the Lewis & Short Latin dictionary for the
entered headword. If nothing can be found, the nearest result is
displayed in Diogenes Lookup Mode. While only a subset of the TEI XML
tags is currently recognized and handled, this mode can display the
most prominent markup of the files and, most importantly, the embedded
citations that can be used to browse the texts in Browser Mode
(activate them by either typing RETURN when they have the point or by
double-clicking. The command `diogenes-perseus-action` (bound to
`C-c C-c`) can also activate these links, but additionally tries to
parse and lookup every word that is marked either as Latin or Greek in
the XML tags.

The commands `diogenes-parse-and-lookup-greek` and
`diogenes-parse-and-lookup-latin` also do a dictionary lookup, but
first try to analyse the form by using the morphological databases
that come with Diogenes. When they fail to get a literal match (Greek
diacritics included), they fall back to a simple dictionary lookup.
This is also the function used by `diogenes-perseus-action`.

Last, there are the commands `diogenes-parse-greek` and
`diogenes-parse-latin`. These commands are quite expensive (at any
rate when executed the first time in a Emacs session) because they
parse and load an entire analysis file into memory. This in turn
allows the user to query these databases in a more general way. In
this type of search, the queries do not have to be literal matches.
Instead, thr user can supply a specialised function to do the lookup.
The predefined functions are `string=` (literal matches),
`string-prefix-p` (matches at the beginning), `string-suffix-p`
(matches at the end), `string-search` (matches anywhere in the form),
and `string-match-p` (using regular expressions), but any function, or
even a custom lambda can be supplied. All these functions can match
not the forms disregarding both the letter case and the diacritics.

(A note on Greek input: You can enter greek words either in Unicode or
in Beta Code. Note, however, that internally, all Greek is converted
to Beta Code, so it may be in some cases more reliable to use Beta
Code. Please inform me if you spot something that only works with Beta
Code!)


## Utilities

At the moment, the package provides two utilites.
`diogenes-beta-to-utf8` and `diogenes-utf8-to-beta` can be used to convert
form and to beta code, both interactively in the minibuffer or in the
current region, and `diogenes-ol-to-ad` and `diogenes-ad-to-ol` convert between 
dates in BC/AD and Olympiads.

