asteroids
=========

Forked from ASTeroids by github user andyhd.  See README.old and the repo.
Thank you andyhd!

Clean-up
--------

Code has been re-organized and cleaned up.  Some names have been altered
to make it more sensible.  AST pun removed as too annoying.

The game is also visually upgraded - the ship and the rocks are being 
adjusted to look better.  The game is now keyboard controlled (the mouse 
was not for me).  Gameplay has been severely whomped upon.

I also added audio using MAME asteroids samples.  lispbuilder-sdl-mixer is 
fragile and very sensitive to being shut down correctly in an interactive 
environment.

Background music is courtesy http://teknoaxe.com/Home.php.

Installation
------------

Install SBCL and quicklisp.

As long as SBCL works from the commandline, type './asteroids.sh'.  I haven't
tested it in winders, but see what the script does and make a batch file.

The game requires lispbuilder-sdl, lispbuilder-sdl-gfx, and 
lispbuilder-sdl-mixer.  If you have problems, use quicklisp to install these.


TODO:
-----

* allow remapping of keys
* fix pause