asteroids
=========

The classic asteroids game using SDL.

Originally ported from Python by andyhd.  The game now has sounds, keyboard
controls, a built-in REPL (triggered by ESC key) and is very playable.

See README.old and the repo.  Many thank you andyhd!

Clean-up 
--------

Code has been re-organized and cleaned up.  Names have been altered
to make it more sensible and bugs fixed.  Now loading with quicklisp

ESC now brings you to (cla REPL.  Examine the game state, recompile anything
(carefully).

I also added audio using MAME asteroids samples.  lispbuilder-sdl-mixer is 
fragile and very sensitive to being (defun foo (x y)
    (declare (type integer x y))
    (logxor x y))shut down correctly in an interactive 
environment.

Background music is courtesy http://teknoaxe.com/Home.php.


Installation
------------

Install SBCL and quicklisp.

The game requires lispbuilder-sdl, lispbuilder-sdl-gfx, and 
lispbuilder-sdl-mixer.  Before you quicklisp these, please install the actual
sdl libraries for your system.  For my Ubuntu I installed:
-libsdl-dev
-libsdl-gfx1.2-dev

Clone the git repo into a directory visible to quicklisp/asdf. Type:

(ql:quickload "asteroids")
(asteroids:main)


Play Instructions
-----------------

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