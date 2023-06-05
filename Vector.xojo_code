#tag Class
Protected Class Vector
	#tag Method, Flags = &h0
		Function Add(rhs as vector) As Vector
		  // adds two vectors and returns their sum
		  dim sum as New Vector
		  sum.x = rhs.x + x
		  sum.y = rhs.y + y
		  sum.z = rhs.z + z
		  return sum
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Compare(rhs as vector) As integer
		  // compares the magnitude of two vectors
		  dim magnitude1 as double = sqrt(x^2+y^2+z^2)
		  dim magnitude2 as double = sqrt(rhs.x^2+rhs.y^2+rhs.z^2)
		  if magnitude1 = magnitude2 then 
		    return 0
		  end if 
		  if magnitude1 > magnitude2 then 
		    return 1 
		  end if 
		  if magnitude2 > magnitude1 then
		    return 2
		  end if 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // empty constructor to allow the programmer to use a vector without components
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(x0 as double, y0 as double, z0 as double)
		  // creates the vector constructor with the three vector components
		  x = x0
		  y = y0
		  z = z0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function crossProduct(rhs as vector) As Vector
		  // returns the cross product of two vectors
		  dim crossProduct as New Vector
		  crossProduct.x = y*rhs.z - z*rhs.y
		  crossProduct.y = z*rhs.x - x*rhs.z
		  crossProduct.z = x*rhs.y - y*rhs.x
		  return crossProduct
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Divide(rhs as double) As vector
		  // return the resulting vector when 
		  // a vector is divided by a scalar
		  dim Divide as new vector
		  Divide.x = x/rhs
		  Divide.y = y/rhs
		  Divide.z = z/rhs
		  return Divide
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function dotProduct(rhs as vector) As double
		  // calculates and returns the dot product of two vectors
		  dim dotProduct as double 
		  dotProduct = rhs.x*x + rhs.y*y + rhs.z* z
		  return dotProduct
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getMagnitude() As double
		  // returns the magnitude of a vector
		  return sqrt(x^2+y^2+z^2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getSquare() As double
		  // returns the square of a vector
		  return x^2+y^2+z^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetString() As String
		  return "( " + Str(x) + " , " + Str(y) + " , " + Str(z) + " )"
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getUnitVector() As Vector
		  dim unitVector as New Vector
		  if me.getMagnitude = 0 then
		    return me
		  else 
		    unitVector.x = x / me.getMagnitude
		    unitVector.y = y / me.getMagnitude
		    unitVector.z = z / me.getMagnitude
		    return unitVector
		  end if 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getVector() As Vector
		  return me
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getX() As double
		  return x
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getY() As double
		  return y
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getZ() As double
		  return z
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Negate() As vector
		  // returns the negated version of a vector
		  dim negateVector as New Vector 
		  negateVector.x = -x
		  negateVector.y = -y
		  negateVector.z = -z
		  return negateVector
		  
		  
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
		Function Operator_Multiply(a as double) As vector
		  //overrides the multiplication * symbol and calculates the product of a scalar with a vector
		  dim scalarProduct as New Vector
		  scalarProduct.x = x*a
		  scalarProduct.y = y *a
		  scalarProduct.z = z*a
		  return scalarProduct
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(a as vector) As double
		  // again overrides the multiplication symbol * and
		  // returns the dot product of two vectors using the dotProduct method
		  return dotProduct(a)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_MultiplyRight(lhs as double) As vector
		  // overrides the multplication symbol * so it accepts the vector quantity 
		  // to be in the left side of the operation
		  dim product as New Vector
		  product.x = x*lhs
		  product.y = y *lhs
		  product.z = z*lhs
		  return product
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Power(rhs as vector) As vector
		  // overrides the power operator ^ to produce the cross product of two vectors
		  // uses the crossProduct method to achieve this
		  return crossProduct(rhs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Subtract(rhs as vector) As Vector
		  // overrides the subtraction operator - and returns the difference between two vectors
		  dim difference as New Vector
		  difference.x = x - rhs.x  
		  difference.y = y - rhs.y  
		  difference.z = z - rhs.z
		  return difference
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setVector(b as vector)
		  me.setX(b.x)
		  me.setY(b.y)
		  me.setZ(b.z)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setX(b as double)
		  x = b
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setY(b as double)
		  y = b
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setZ(b as double)
		  z = b
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		z As Double
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
			Name="x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
