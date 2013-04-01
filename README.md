Emacs config
============

One more ugly way for setting up Emacs.

## Manual installation ##

    $ mkdir "~/.emacs.d" && cd "~/.emacs.d"
    $ git clone git://github.com/taryk/emacs.d.git .
    $ git submodule update --init

Then you can run emacs

    $ echo '(load-file "~/.emacs.d/init.el")' > ~/.emacs
    $ emacs

or just

    $ emacs -q -l ~/.emacs.d/init.el

## Structure ##

Directory/File | Description |
---- | ---- |
packages/ | Contains manually installed 3rd party packages |
elpa/ | Installed ELPA packages |
snippets/ | Contains snippets splitted by major-mode in YASnippet format |
custom.el | Various emacs settings  |
init-autocomplete.el | autocomplete-mode configs  |
init-cc.el | cc-mode set up |
init-cedet.el | Cedet customization |
init-clojure.el | clojure-mode customization |
init-cmake.el | cmake-mode customization |
init-common-lisp.el | common-lisp-mode customization |
init-cua.el | cua-mode customization |
init-dirtree.el | dirtree-mode customization  |
init-ecb.el | ECB customization  |
init-ediff.el | ediff customization |
init-elisp.el | elisp-mode customization |
init-elpa.el | ELPA customization |
init-geiser.el | geiser customization |
init-general-defuns.el | misc functions |
init-git.el | git-related modes customization |
init-haskell.el | haskell-mode customization |
init-heroku.el | heroku customization |
init-iedit.el | iedit-mode customization |
init-irc.el | ERC customization |
init-jabber.el | jabber.el customization |
init-jira.el | jira-mode customization |
init-js.el | javascript-related modes customization |
init-keybinds.el | Keybindings customization |
init-linum.el | Line numbers |
init-lua.el | lua-mode customization |
init-markdown.el | markdown-mode customization |
init-mercurial.el | mercurial-related modes customization |
init-minimap.el | minimap-mode customization |
init-mmm.el | Multi Major Mode customization |
init-modeline.el | Emacs modeline customization |
init-multiplecursors.el | multiple-cursors-mode customization |
init-multiterm.el | multi-term customization |
init-nxhtml.el | nxhtml-mode customization  |
init-nyan.el | nyan-mode customization |
init-org.el | org-mode customization |
init-package.el | Ability to define custom packages |
init-perl.el | Perl-related modes customization |
init-perspective.el | persp-mode customization |
init-php.el | php-related modes customization |
init-profile.el | provide profiles (not implemented yet) |
init-python.el | Python-related modes customization |
init-ruby.el | Ruby-related modes customization |
init-scala.el | scala-mode2 customization |
init-scheme.el | scheme-related modes customization |
init-session.el | emacs sessions customization |
init-shell-script.el | sh-mode customization |
init-sml.el | sml-mode customization |
init-sqlite.el | sqlite support (not implemented yet) |
init-svn.el | SVN-related modes support customization |
init-twitter.el | twitter support |
init-vline.el | vline-mode customization |
init-w3m.el | w3m customization |
init-window-number.el | window-number-mode customization |
init-workspaces.el | workspaces customization |
init-yaml.el | yaml-mode customization |
init-yasnippet.el | yasnippet |
init.el | Starting point of initialization |

## Packages ##

### Programming Languages ###
* [cperl-mode][25]
* [Sepia][27]
* [PDE][28]
* [SLIME][29]
* [perlbrew-mini][33]
* [perlcritic][34]
* [perltidy][35]
* [cc-mode][7]
* [haskell-mode][8]
* [haml-mode][12]
* [Pymacs][14]
* [js2-refactor][18]
* [sml-mode][21]
* [geiser][22]
* [scala-mode2][23]
* [clojure-mode][24]

### Programming Tools ###
* [auto-complete][26]
* [yasnippet][9]
* [anything][30]
* [linum][31]
* [linum-relative][10]
* [multiple-cursors][13]
* [mmm-mode][15]
* [markdown-mode][19]

### Version Control Systems ###
* [ahg][1]
* [egg][2]
* [egit][3]

### Themes ###
* [zenburn-emacs][11]
* [twilight-emacs][20]

### Misc ###
* [perspective][16]
* [MiniMap][32]
* [emacs-soap-client][4]
* [nyan-mode][5]
* [heroku][6]
* [mark-multiple][17]

[1]: https://bitbucket.org/agriggio/ahg
[2]: https://github.com/byplayer/egg.git
[3]: https://github.com/jimhourihan/egit.git
[4]: https://code.google.com/p/emacs-soap-client/
[5]: https://github.com/TeMPOraL/nyan-mode.git
[6]: https://github.com/technomancy/heroku.el.git
[7]: https://github.com/emacsmirror/cc-mode.git
[8]: https://github.com/haskell/haskell-mode.git
[9]: https://github.com/capitaomorte/yasnippet.git
[10]: https://github.com/coldnew/linum-relative.git
[11]: https://github.com/bbatsov/zenburn-emacs.git
[12]: https://github.com/nex3/haml-mode.git
[13]: https://github.com/magnars/multiple-cursors.el.git
[14]: https://github.com/pinard/Pymacs.git
[15]: https://github.com/purcell/mmm-mode.git
[16]: https://github.com/nex3/perspective-el.git
[17]: https://github.com/magnars/mark-multiple.el.git
[18]: https://github.com/magnars/js2-refactor.el.git
[19]: git://jblevins.org/git/markdown-mode.git
[20]: git://github.com/crafterm/twilight-emacs.git
[21]: git://github.com/emacsmirror/sml-mode.git
[22]: git://github.com/jaor/geiser.git
[23]: git://github.com/hvesalai/scala-mode2.git
[24]: https://github.com/technomancy/clojure-mode.git
[25]: git://github.com/jrockway/cperl-mode.git
[26]: http://cx4a.org/software/auto-complete/
[27]: https://metacpan.org/module/Sepia
[28]: https://metacpan.org/module/Emacs::PDE
[29]: http://common-lisp.net/project/slime/
[30]: http://www.emacswiki.org/Anything
[31]: http://www.emacswiki.org/LineNumbers
[32]: http://www.emacswiki.org/emacs/MiniMap
[33]: git://github.com/dams/perlbrew-mini.el.git
[34]: git://github.com/emacsmirror/perlcritic.git
[35]: git://github.com/emacsmirror/perltidy.git
