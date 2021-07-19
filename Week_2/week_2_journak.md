19/07/2021 : 
Here is the explanation of my first architecture : 
 - class lcl_random_generator with a method that generates a random number between 0 and 2. I will use it to generate randomly the game board with the different state of cells.
   0 represents None / 1 represents a died cell / 2 represents an alive cell

 - class lcl_state that represents a state of a cell. I use it as a composition into the next class. 
	0 represents None / 1 represents a died cell / 2 represents an alive cell. They are defined as a public static constant attributes.

 - class lcl_cell that represents a unique cell. The creation of a cell is done randomly using the random generator to give an initial state to my cell. 
   A cell has an attribute state that gives the state of the cell (0 represents None / 1 represents a died cell / 2 represents an alive cell)

 - class lcl_game_board that represents the game board 3 x 3. When starting playing, each cell will have its initial state defined randomly.
 
 How do I see the game? When starting playing, the game board is initialized with a 3x3 cells. 
 The player will press a button "Next Step" and the game will calculate the next state of each cells related to neighbors.
 
 I didn't write unit test. This is a mistake and tomorrow when I will start from scratch I will start by writing unit test before to use TDD.
 This will surely highlight errors in my architecture. 
 
 