
diogenes.el
===========

Diogenes.el is a collection of some Emacs Lisp functions that use the
CLI interface of P. Heslin's Diogenes to browse and search the TLG und
PHI Greek and Latin databases from within Emacs. They are quite hacky
in design, but tend to do their job.

Installation
------------

Please make sure that you have a working installation of Diogenes; due
to a bug in the main repository of diogenes, at the time of this
writing you can either use my
[fork](https://github.com/nitardus/diogenes) or patch the original:

	# when executing this command, make sure
	# 1) to alter the path /usr/local/... to the place where your diogenes installation resides
	# 2) to have write permissions to this directory (su root, ...)
	patch /usr/local/diogenes/server/diogenes-cli.pl < diogenes-cli.patch
	
After that, diogenes-cli.pl should be working. It is advisable to symlink diogenes-cli.pl to your $PATH; alternatively, you have to alter all occurences of "diogenes-cli.pl" in diogenes.el to hold the full path to the diogenes-cli.pl script.

	ln -s /usr/local/diogenes/server/diogenes-cli.pl /somewhere/in/your/path/
	
Test it now by executing

	diogenes-cli.pl
	
Now copy diogenes.el into the elisp subfolder of your .emacs.d (creating it when necessary). Adapt the path if necessary

	cp diogenes.el ~/.emacs.d/elisp/
	
Now add these two lines to your .emacs or init.el

	(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
	(require 'diogenes)

Finally, check the keybinding at the end of the diogenes.el file and alter them if they do not fit you.


Usage
-----

  * Search the corpora with M-x diogenes-search-greek or M-x
    diogenes-search-latin
  * Tidy up the search results with M-x
    diogenes-tidy-up-search-results
  * Browse the TLG with M-x diogenes-browse-greek and the PHI with M-x
    diogenes-browse-latin
  * Tidy up the browser output with M-x
    diogenes-tidy-up-browser-output-greek and M-x
    diogenes-tidy-up-browser-output-latin
  * You can delete hyphenations with M-x diogenes-unhyphen-greek and M-x diogenes-unhyphen
  * To get line numbers in every line of the browser output, use M-x
    diogenes-browser-insert-line-numbers
  * To get rid of the line numbers, use M-x diogens-delete-line-numbers
  * To reformat the apostrophe character to be typographically
    correct, use diogenes-apostrophe
  * To convert dates between BC/AD and olympiads, use diogenes-ol-to-ad and diogenes-ad-to-ol
  
  




