#tag Module
Protected Module FormatNumbers
	#tag Method, Flags = &h21
		Private Function CreateFirstNum(inputValue As Double, uncertainty As Double) As String
		  //This method creates the first of two numbers which will be combined in the FN method 
		  //It does this by creating a temporary string, tempString, which is passed into the Format method along with the input value
		  
		  
		  Var inputMag As Integer= Magnitude(InputValue) //Defines magnitude of the input value, See Magnitude method for more information
		  Var uncertaintyMag As Integer = Magnitude(uncertainty) //Defines magnitude of the uncertainty value
		  Var tempString as String //Creates a temporary string to be passed into Format() at the end
		  
		  
		  If uncertainty > inputValue or uncertainty = inputValue then //Checks if uncertainty is greater than or equal to inputValue
		    if inputValue = 0 and uncertaintyMag > -5 then
		      tempString = "0.0"
		      Var x As Integer
		      for x = 0 to -(UncertaintyMag+1) //This for loop adds the correct number of zeroes to tempString 
		        tempString = tempString + "0"
		      next
		    else 
		      return MakePretty(inputValue)
		    end if
		  else
		    
		    if Abs(inputMag - uncertaintyMag) > 5 then
		      return MakePretty(inputValue)
		      
		    elseif inputMag > 4 or inputMag < -4 then //Checks whether the number should be writted in scientific notation
		      tempString = "#." 
		      Var x As Integer
		      For x = 0 to (inputMag - uncertaintyMag) //This for loop adds the correct number of zeroes for scientific notation formatting
		        tempString = tempString + "0"
		      next
		      tempString = tempString+"e"
		      
		    else
		      tempString = "#"
		      Var x As Integer
		      if UncertaintyMag < 1 then // If the uncertainty is less than 10, we add a decimal point 
		        tempString = tempString +"."
		      end if 
		      for x = 0 to -(UncertaintyMag) //This for loop adds the correct number of zeroes to tempString 
		        tempString = tempString + "0"
		      next
		      
		    end if 
		    
		  end if
		  
		  return Format(inputValue, tempString) //Returns a for
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function CreateSecondNum(inputValue As Double, uncertainty As Double) As String
		  Var inputMag As Integer= Magnitude(inputValue) //Defines magnitude of the input value, See Magnitude method for more information
		  Var uncertaintyMag As Integer = Magnitude(uncertainty) //Defines magnitude of the uncertainty value
		  Var outputString As String //Creates the string that will become our formatted uncertainty 
		  
		  
		  If uncertainty > inputValue or uncertainty = inputValue then //Checks if the uncertainty is greater than or equal to the input
		    
		    
		    if uncertaintyMag < 5 and uncertaintyMag > -5 then 
		      if uncertaintyMag > 0 then 
		        outputString = "("+Format(uncertainty, "#.")+")"
		      elseif uncertainty = 0 then
		        outputString = "("+Format(uncertainty, "0.0")+")"
		      elseif Abs(uncertaintyMag - inputMag) > 5 then
		        outputString = "("+ Str(Round(uncertainty*10^(-uncertaintyMag+1))*10^(uncertaintyMag-1)) +")"
		      else
		        outputString = "("+ Str(Round(uncertainty*10^(-uncertaintyMag+1))*10^(uncertaintyMag-1)) +")"
		      end if 
		      
		      
		      
		    elseif abs(uncertaintyMag - inputMag) < 4 and abs(uncertaintyMag - inputMag) > 0 then
		      outputString = "("+Str(Round((uncertainty*10^(-uncertaintyMag))*(10^(uncertaintyMag-inputMag))))+".)"
		    elseif Abs(uncertaintyMag - inputMag) < 1 then
		      outputString = "("+ Format(uncertainty*10^(-uncertaintyMag),"0.0")+")"
		    else 
		      outputString = "("+ Format(uncertainty,"0.#e")+")"
		    end if 
		    
		  elseif Abs(inputMag - uncertaintyMag) > 5 then
		    if uncertaintyMag > 4 or uncertaintyMag < - 4 then
		      outputString = "("+Format(uncertainty, "#.#e")+")"
		    elseif uncertaintyMag > 0 then
		      outputString = "("+Format(uncertainty, "#.")+")"
		    elseif uncertaintyMag > -1 then 
		      outputString = "("+Format(uncertainty, "#.#")+")"
		    else
		      outputString = "("+ Str(Round(uncertainty*10^(-uncertaintyMag+1))*10^(uncertaintyMag-1)) +")"
		    end if 
		    
		    
		  elseif inputMag > 4 or inputMag < -4 then  //Checks if uncertainty needs to be formatted into a number in scientific notation
		    if inputMag = uncertaintyMag then
		      outputString = "("+Format(Round(uncertainty*10^(-uncertaintyMag + 1))/10,"0.0")+")"
		    else
		      outputString = "(" + Str(Round(uncertainty*10^(-uncertaintyMag + 1)))+")" //Scales the uncertainty so that the first two digits are hundreds and tens, rounds the number to the nearest int 
		    end if
		  elseif uncertaintyMag > 1 then          //If The uncertainty is greater than 100 and less than 10,000 we place a period after the uncertainty                                                                              
		    outputString = "("+Str(Round(uncertainty))+".)"
		    
		  elseif uncertaintyMag > 0 then //If the uncertainty between 100 and 10 we simply round the number to the nearest integer 
		    outputString = "("+Str(Round(uncertainty))+")"
		    
		  elseif uncertaintyMag > -1 then // If the uncertainty is between 10 and 1 we format it with a decimal point
		    outputString = "("+Format(uncertainty,"0.0")+")"
		    
		  else //For all other numbers we simply place the first two significant digits in parenthesis. 
		    outputString = "(" + Str(Round(uncertainty*10^(-uncertaintyMag + 1)))+")"
		    
		  end if 
		  
		  return outputString
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FN(inputValue as Double, uncertainty as double) As String
		  Var inputMag As Integer = Magnitude(inputValue) //Defines magnitude of the input value, See Magnitude method for more information
		  Var uncertaintyMag As Integer = Magnitude(uncertainty) //Defines magnitude of the uncertainty value
		  Var firstNum as String = CreateFirstNum(inputValue,uncertainty) //Formats the input value and assigns it a variable
		  Var secondNum As String = CreateSecondNum(inputValue,uncertainty) //Formats the uncertainty and assigns it a variable
		  Var splitArray(2) As String //Creates an empty array used for combining the two numbers
		  Var totString As String //Creates a string used to return the combined numbers
		  
		  if uncertainty < inputValue and (inputMag > 4 or inputMag < -4) and Abs(inputMag - uncertaintyMag) < 6 then //Checks if our output is in scientific notation
		    if uncertainty <> 0 then
		      splitArray = firstNum.SplitBytes("e")                                                 //If it is we split the first number at the "e" so that 1.79e-11 becomes an array containing "1.79" and "-11"
		      totString = splitArray(0)+secondNum+"e"+splitArray(1) //Creates our final output using the split array and formatted uncertainty 
		    else
		      totString = firstNum + secondNum
		    end if 
		    
		  elseif uncertainty > inputValue or uncertainty = inputValue then
		    if Abs(uncertaintyMag - inputMag) < 4 and (uncertaintyMag > 4 or uncertaintyMag < -4) then
		      splitArray = firstNum.SplitBytes("e")                                                
		      totString = splitArray(0)+secondNum+"e"+splitArray(1)
		    else
		      totString = firstNum + secondNum
		    end if
		    
		  else
		    totString = firstNum+secondNum //If the number is not in scientific notation we simply put the strings together
		    
		  end if 
		  Return TotString
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function Magnitude(number As Double) As integer
		  //This function returns the floor of the log base 10 of an inputted number
		  
		  if number > 0 then // Checks if the input greater than 0
		    if number = 1e6 then
		      return 6 
		    else
		       return Floor(Log(number)/Log(10)) //Calculates log base 10 using change of base, and the natural log
		    end if
		  else 
		    
		    return 0 //If the input is 0 or negative, we return 0
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function MakePretty(inputValue As Double) As String
		  //This method formats long doubles to make them more readable.
		  
		  Var inputMag As Integer = Magnitude(inputValue) //defines the magnitude of the input, see Magnitude Method for more
		  Var retString As String //Creates a string that will be used by the Format method
		  
		  If inputMag > 4 or inputMag < -4 then  //Checks to see if the number should be in scientific notation
		    retString = Format(inputValue, "#.00e")
		    
		  elseif inputValue = 0 then //If the input is 0 we write 0 as 0.0
		    retString = Format(inputValue,"0.0")
		    
		  else //For all other cases we simply covert the double to a string. 
		    retString = Str(inputValue)
		    
		  end if
		  
		  return retString
		  
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
