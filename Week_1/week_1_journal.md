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


15/07/2021:
Here is my 50 minutes coding : 
Today I decided to focus on the roman converter and arabic converter. They use the splitter and the combiner. 
I created a class lcl_number_representation with 4 attributes : units/tens/hundreds/thousands representation
The splitter will return the object lcl_number_representation
The combiner will use the object lcl_number_representation to concatenate units/tens/hundreds/thousands converted values.
I have a problem with the input parameter. I loose the value splitted to be converted at a moment so I need to have a reflexion tomorrow to refactor my coding 
and resolve the issue without loosing the value to convert.


16/07/2021
Today I resolve my yesterday issues when I loose at a moment some input parameters so I decided to store (attributes) the value to be converted 
into the class that represents units/tens/hundreds/thousands. I follow my architecture of yesterday but I didn't have enough time 
to rewrite my ABAP Unit Test and to finish my coding of the highest level but I confirm that I wanted to follow what I've done the day 2. 
I also didn't have enough time  to implements the enhancement to let the user specify a text file containing a number to convert on each line.
I wanted to do it in 2 steps : first step with only input parameter and 2nd step the enhancement with the text file. Why ?
Because I wanted to check if my architecture will respect the Open/Closed Principle. 
And if not to work on it to allow my program to respect the Open/Closed Principle. But 50 minutes of coding per day is not enough...