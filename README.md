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

I also added audio using MAME asteroids samples.  lispbuilder-sdl-mixer is 
fragile and very sensitive to being shut down correctly in an interactive 
environment.


TODO:
-----

* allow remapping of keys
* fix the main-loop jumble, move logic into world
* separate attract screen
* fix pause