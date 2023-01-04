# SpaceInvaders
A Space Invaders-inspired game made in Beginner Student Language in DrRacket. 

This was my final project for the edX course How To Code: Simple Data offered by UBC. This was also my first programming project.

Files:

HtC Simple Data Final Project - Space Invaders.rkt
- Source code of the Space Invaders game. The program was implemented using universe.rkt (read more: https://docs.racket-lang.org/teachpack/2htdpuniverse.html) to make it interactive and handle events (e.g. keyboard).
- Invaders spawn randomnly at the top of the screen and move downwards at a 45 degree angle. The tank moves using the left and right keys and fires missiles with the space key. When a missile and invader get close enough, they both disappear. The game ends if an invader reaches the bottom of the screen.

HtC Simple Data Final Project Domain Analysis.jpg
- This is an analysis of the problem domain. In order to create a specific solution for an ambiguously-posed problem, a sequence of game states was drawn. Constant and changing info were then identified to determine what data needed to be managed in the code.
