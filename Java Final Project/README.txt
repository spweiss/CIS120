=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=
CIS 120 Game Project README
PennKey: spweiss
=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=

===================
=: Core Concepts :=
===================

- List the four core concepts, the features they implement, and why each feature
  is an appropriate use of the concept. Incorporate the feedback you got after
  submitting your proposal.

  1. I modeled the game board as a 2D array of 900 cells. The layout of the board is randomized
  before the start of each game. The array determines which cell is clicked based on the location
  and passes the MouseEvent to the elements of the array. Data in the array is displayed to users
  in response to clicks and the array is traversed when determining whether the game has ended and
  when determining the number of mines surrounding each cell.

  2. I made a high scores feature using I/O. When the game ends, the user is prompted to enter
  his or her first name. When this is complete, the name and the percentage of the board that was
  swept out is recorded in the highscores.txt file. Each person can only have one high score. If
  there is an error related to the formatting of the name, the user is asked to retry and if there
  is an error with an internal file, the user is informed with an error window. When the High
  Scores button is clicked in the game, the user can see the top ten scores and scorers in the game.

  3. I used inheritance and sub-typing to take advantage of dynamic dispatch for the different types
  of Spaces on the board. Space is an abstract class that implements some helper methods used by
  Blank and Mine. Blank and Mine override some of the methods in Space. The board is filled with
  Spaces, so that methods in GameBoard can be called on any cell and the appropriate method is run
  via dynamic dispatch. This is necessary, since Blank and Mine have different methods for things
  like drawing the cells, handling clicks and setting their counts, but share mechanisms for their
  location and image processing.

  4. I used recursion to implement a cascading algorithm in the game. When a user clicks a cell that
  is surrounded by zero mines, all surrounding spaces that have zero mines as neighbors are also
  revealed, as well as the spaces directly surrounding those. This is best implemented using
  recursion, because the method must be run an indeterminate number of times until the spaces
  revealed do not change, and then once more for the surrounding cells. This function is cascade in
  the GameBoard class. In my proposal, I stated that I would use JUnit testing, but as I was
  building my game, I realized that recursion would have to be used significantly, so I used it as
  a core concept.


=========================
=: Your Implementation :=
=========================

- Provide an overview of each of the classes in your code, and what their
  function is in the overall game.
  Game contains the run and main methods that set up the general layout of the game. GameCourt
  contains the foundation of the event handling for the game, as well as the implementations of the
  reset, instructions and high scores windows. GameBoard sets up the array and contains methods to
  set up the mine counts, determine if the game is over, and reveal the board at the end. Space
  contains helper methods like setting up images and getting the count of mines. Blank contains
  methods to determine how it should look based on the number of surrounding mines, determine if it
  has been flagged and handle user clicks. Mine has methods to determine how it should look and
  handle user clicks.

- Were there any significant stumbling blocks while you were implementing your
  game (related to your design, or otherwise)?
  The most significant stumbling block was the recursive cascade method. The program initially
  thought that nearly all spaces should be in the cascade. I determined that this was because it
  thought that all cells had zero mines as neighbors. I figured out that this was due to an error
  in assigning the mine count values, and not with the cascade algorithm at all! After that, it was
  fairly trivial to fix.

- Evaluate your design. Is there a good separation of functionality? How well is
  private state encapsulated? What would you re-factor, if given the chance?
  I think that the functionality is fairly well separated, with each element of the game determining
  how it should look and handle clicks. Private state is encapsulated with setters and getters,
  besides intentionally public static variables like images, which are the same for the whole
  program.


========================
=: External Resources :=
========================

- Cite any external resources (libraries, images, tutorials, etc.) that you may
  have used while implementing your game.
  I got each of the images for the cells from a Wikimedia repository for Minesweeper images at
  https://commons.wikimedia.org/wiki/Category:Minesweeper

