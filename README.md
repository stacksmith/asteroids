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
was not for me).

I also added audio using MAME asteroids samples.  It works.  However, 
lispbuilder-sdl-mixer is buggy on my system, and it's hard to work
with (as it will not re-initialize after being shut down the first time 
and will in fact lock up SBCL tight).  Not shutting it down makes SBCL 
not crash, but sadly no sound comes out of it.  I have to develop in 
emacs/slime repl, save the file, and load it into a fresh SBCL image to 
test it.  Like a C compiler...  If anyone has any thoughts on this, 
please help...


TODO:
-----

* allow remapping of keys
* fix the main-loop jumble, move logic into world
* separate attract screen
* fix pause