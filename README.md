asteroids
=========

The game has been tuned for playability, added sounds, keyboard controls
and a built-in REPL.

Forked from ASTeroids by github user andyhd.  See README.old and the repo.
Thank you andyhd!

Clean-up 
--------

Code has been re-organized and cleaned up.  Names have been altered
to make it more sensible and bugs fixed.

ESC now brings you to a REPL.  Examine the game state, recompile anything
(carefully).

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
lispbuilder-sdl-mixer.  If you have problems, use quicklisp to install 
lispbuilder.

Instructions
------------

The game plays like original asteroids, except instead of saucers you get 
powerups.  Ram the powerups to pick up super-missiles that go through anything,
shields, or to stop time.

Keyboard controls:

Q   - quit 
A   - rotate ship left
F   - rotate ship right
J   - thrust
SPC - fire
ESC - REPL

P starts the game in attract mode.

TODO:
-----

* allow remapping of keys
* fix pause