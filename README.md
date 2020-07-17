my_elisp
========

My emacs lisp to customize keymap, create template file etc...

Target Version
--------------
* [Emacs For Mac OS X](https://emacsformacosx.com/)

    ```
    (version)
    "GNU Emacs 26.2 (build 1, x86_64-apple-darwin18.2.0, NS appkit-1671.20 Version 10.14.3 (Build 18D109))
    of 2019-04-13"
    ```

* Ubuntu 

    ```
    (version)
    "GNU Emacs 26.3 (build 2, x86_64-pc-linux-gnu, GTK+ Version 3.22.30)
    of 2019-09-16"
    ```

How to use
----------
1.

    ```
    mkdir -p $HOME/lib/
    ```
    
2. clone this source

    ```
    git clone https://github.com/mamewotoko/my_elisp.git
    git submodule update --init
    ```
    
3. add symlink to init.el

    ```
    ln -s $HOME/lib/emacs/.emacs.d $HOME/.emacs.d
    ```

4. start emacs, customize,... enjoy

Features
--------
* Multiple shell buffers (f5,f6,f7,f8)
* pushd to shell buffer (C-f5,C-f6,C-f7,C-f8)
* manual, apropos
* error jump from shell buffer(C-j)
* [helm](https://github.com/emacs-helm/helm)
* [magit](https://github.com/magit/magit)

----
Takashi Masuyama < mamewotoko@gmail.com >  
http://mamewo.ddo.jp/
