#tag Class
Protected Class Vector
	#tag Method, Flags = &h0
		Function Add(rhs as vector) As Vector
		  // adds two vectors and returns their sum
		  return New Vector(rhs.X + X, rhs.Y + Y, rhs.Z + Z)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Clone() As Vector
		  return New Vector(X,Y,Z)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Compare(rhs as vector) As integer
		  // compares the magnitude of two vectors
		  Var Mag1  As Double = GetMagnitude
		  Var Mag2 as Double = rhs.GetMagnitude
		  if Mag1 = Mag2 then 
		    return 0
		  Elseif Mag1 > Mag2 then 
		    return 1
		  Else
		    return -1
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // empty constructor to allow the programmer to use a vector without components
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(X0 as double, Y0 as double, Z0 as double)
		  // creates the vector constructor with the three vector components
		  X = X0
		  Y = Y0
		  Z = Z0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CopyOfVector(b as vector)
		  X = b.X
		  Y = b.Y
		  Z = b.Z
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function CrossProduct(rhs as vector) As Vector
		  // returns the cross product of two vectors
		  return New Vector(Y*rhs.Z - Z*rhs.Y, Z*rhs.X - X*rhs.Z, X*rhs.Y - Y*rhs.X)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Divide(rhs as double) As vector
		  // return the resulting vector when dividing by a scalar
		  Return New Vector(X/rhs, Y/rhs, Z/rhs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DotProduct(rhs as vector) As Double
		  // calculates and returns the dot product of two vectors
		  return rhs.X*X + rhs.Y*Y + rhs.Z*z
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetMagnitude() As double
		  // returns the magnitude of a vector
		  return sqrt(X^2+Y^2+Z^2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSquare() As double
		  // returns the square of a vector
		  return X^2+Y^2+Z^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetString() As String
		  return "( " + X.ToString + " , " + Y.ToString + " , " + Z.ToString + " )"
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetUnitVector() As Vector
		  Var Mag As Double = GetMagnitude
		  if Mag = 0.0 then
		    Return Nil
		  else
		    Return New Vector(X/Mag, Y/Mag, Z/Mag)
		  end if 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Negate() As vector
		  // returns the negated version of a vector
		  return New Vector(-X, -Y, -Z)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Add(a as vector) As Vector
		  // overrides the + operator and uses the method Add created earlier to add vectors
		  return Add(a)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Divide(a as double) As vector
		  // this method allows a vector to be divided by a scalar
		  return Divide(a)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(a as Double) As Vector
		  //overrides the multiplication * symbol and calculates the product of a scalar with a vector
		  return New Vector(a*X, a*Y, a*Z)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(a as Vector) As Double
		  // again overrides the multiplication symbol * and
		  // returns the dot product of two vectors using the dotProduct method
		  return DotProduct(a)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_MultiplyRight(lhs as double) As vector
		  // overrides the multplication symbol * so it accepts the vector quantity 
		  // to be in the left side of the operation
		  return New Vector(lhs*X, lhs*Y, lhs*Z)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Power(rhs as vector) As vector
		  // overrides the power operator ^ to produce the cross product of two vectors
		  // uses the crossProduct method to achieve this
		  return CrossProduct(rhs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Subtract(rhs as vector) As Vector
		  // overrides the subtraction operator - and returns the difference between two vectors
		  return New Vector(X-rhs.X, Y-rhs.Y, Z-rhs.Z)
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		X As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Z As Double
	#tag EndProperty


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
		#tag ViewProperty
			Name="X"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
