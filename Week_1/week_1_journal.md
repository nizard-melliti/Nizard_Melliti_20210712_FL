12/07/2021 : 
I choose the Kata "Convert Roman". Before Coding I started with the Message and Data-Flow Diagram and I identified the operation and integration methods.
During my 50 minutes'coding, I used the TDD methodology. I thing it's too early to think about what I can improve.
My fear is that every day I haven't enough time to code something new as I need to delete my coding everyday... 

13/07/2021 : 
I spent 15 minutes to write my code from yesterday. I decided first to make a focus on the solution with the input parameter. 
On a second time, I will evolve my program to let the user specify a text file containing a number to convert on each line.

Here is my program architecture: 

 - The highest level of my solution is the Converter (class lcl_converter). It converts an arabic number to roman number OR a roman number to arabic number.
	It uses a roman converter or arabic conveter depending on the input.
 
 - I have a roman converter (class lcl_converter_to_roman) that will split the number (class for splitter to be created) into units/tens/hundreds/thousands 
     and convert each item (units/tens/hundreds/thousands) to roman and combine them by a concatenation (class Combiner to be created)
 
 
 - I also have an arabic converter (class lcl_converter_to_arabic) using the same logic as roman converter but for arabic representation.
 
 As my class for units/tens/hundreds/thousands representation use the same logic, I think that I have to create an interface to have an abstract 
 representation as we have the same methods. This is my goal for tomorrow.
My fear is that now I have to rewrite tomorrow 350 lines of code before starting coding....


14/07/2021 :
I have created an abstraction about units/tens/hundreds/thousands representation with an interface YIF_NUMBER_REPRESENTATION. 
I think that I can use the same logic for the converter to roman and the converter to arabic as we have the same pattern.
Tommorrow I will create an interface YIF_NUMBER_CONVERTER and this interface will be used by the converter to roman and the converter to arabic.
I really want to finish this exercice and I think that I need to spend more than 50 minutes coding by day as every day we have to start from zero line of codes...
About 30 minutes was dedicated to rewrite everything....