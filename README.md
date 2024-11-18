diogenes.el
===========

Diogenes.el strives to be a complete interface to P. Heslin's Diogenes
that allows to browse and search the TLG und PHI Greek and Latin
databases from within Emacs, as well as to lookup words at point in
the lexica that come with Diogenes. Unkike the previsus version of
this package, it now uses custom perl scripts to communicate with
Diogenes' perl API and tries to cover it entirely. For a collection of
command line scripts and perl modules that offer similar
functionalities, see my DiogenesUtils.

Installation
------------

Please make sure that you have a working installation of Diogenes.
After that, copy the diogenes.el someplace in your elisp load path,
e.g. to ~/.emacs.d/elisp (make sure that this folder exists and is in
the load-path list, like so: 

	(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
	
Now require it and set the path to the diogenes-libary-path variable
to the "server" subfolder of your Diogenes installation:

	(require 'diogenes)
	(setq diogenes-library-path "/path/to/diogenes/server)

Or, using use-package (with some handy key-bindings)

	(use-package diogenes
	  :config
	  (setq diogenes-path "/path/to/diogenes/")
	  :bind 
	  ("C-c d b g" . diogenes-browse-tlg)
	  ("C-c d b l" . diogenes-browse-phi)
	  ("C-c d b i" . diogenes-browse-ddp)
	  ("C-c d d g" . diogenes-dump-tlg)
	  ("C-c d d l" . diogenes-dump-phi)
	  ("C-c d d i" . diogenes-dump-ddp)
	  ("C-c d s g" . diogenes-search-tlg)
	  ("C-c d s l" . diogenes-search-phi)
	  ("C-c d s i" . diogenes-search-ddp)
	  ("C-c d l l" . diogenes-lookup-latin)
	  ("C-c d l g" . diogenes-lookup-greek)
	  ("C-c d p l" . diogenes-parse-latin)
	  ("C-c d p g" . diogenes-parse-greek))


Usage
-----

  * Search the corpora with M-x diogenes-search-tlg, M-x
    diogenes-search-phi or M-x diogenes-search-ddp
  * Browse the corpora with M-x diogenes-browse-tlg,
    diogenes-browse-phi and diogenes-browse-ddp. This opens the text
    in the Diogenes Browser Mode. Here,
	* load the next page of text by going to the next line at the end
      of the buffer (diogenes-browse-forward)
	* load the previous page of text by going to the previous line at
      the start of the buffer (diogenes-browse-backward)
  	* toggle the citations with C-c C-n
        (diogenes-browser-toggle-citations)
	* remove hyphenations with C-c C--
      (diogenes-diogenes-browser-remove-hyphenation; call it with a
      prefix to mark the deleted hyphens with a vertical bar)
	* reinsert the hyphenations with C-c C-+
      (diogenes-browser-reinsert-
  * Parse and/or look up the word at point in the dictionaries
    (diogenes-lookup-greek, diogenes-lookup-latin,
    diogenes-parse-greek, diogenes-parse-latin)
  * Dump whole works from the corpora with M-x diogenes-dump-tlg,
    diogenes-dump-phi, diogenes-dump-ddp
  * Convert dates between BC/AD and olympiads with diogenes-ol-to-ad
    and diogenes-ad-to-ol





