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
 
 
20/07/2021 :
I started by writing unit test. At the begining I was confused as the cell state are generate randomly 
but with some search I found that we can use assert_number_between to ensure that the number generated is between the bound.
The difference with yesterday is that while writing my unit tests I realized that my cell and state classes were not testable. 
I had to create methods to return the state of the cell. These methods return a value and are therefore testable.


21/07/2021 : 
I changed my approach about the random generator. In the old version of my random generator, I used Integers, whereas I think that using the State object is more appropriate (everything is an object).
This has brought some changes especially in the Cell object at the initialization of a cell.  
During my next session I will build on this architecture with the TDD methodology. 

22/07/2021 : 
I used the TDD approach. With my version of the generator that returns a random state object I am stuck for the unit test. 
Either my code is not testable because it returns a random object, or I'm testing the wrong thing.
For the next session I will have to find a way to test my state generation class differently.

23/07/2021 :
It is very complicated to test classes and methods that return random values. I had to create a get_state method which returns the state's value.
This method doesn't fit the Tell Don't Ask principle. It is only used to test if what I am building randomly is what is expected.