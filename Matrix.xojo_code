#tag Class
Protected Class Matrix
	#tag Method, Flags = &h0
		Sub Constructor(entries(, ) As Double)
		  // Constructs a numeric Matrix object from an array of entries. 
		  // Parameters: entries, a 2D array of doubles (presumed to be square)
		  
		  Var n As Integer = entries.LastIndex     // gets dimensions of entries
		  
		  pDim = n + 1              // since Xojo is 0-indexed, dimension is 1 more than highest entry index
		  pData.ResizeTo(n, n)
		  pData = entries           // stores entries as pData
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(NDimensions As Integer)
		  // Initialize a zero matrix with a certain number of dimensions
		  pDim = nDimensions     // stores dimension of matrix as property
		  Var n As Integer = nDimensions - 1
		  pData.ResizeTo(n, n)  // resizes matrix data array appropriately (n-1 used because of 0-indexing)
		  // This part is probably not necessary, but just to be safe
		  For i As Integer = 0 To n      // loop over matrix entries
		    For j As Integer = 0 To n
		      pData(i,j) = 0.0
		    Next
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DiagNormz(n As Integer)
		  // This method "normalizes" a matrix, or upper-left submatrix of specified size, by dividing all elements in each row of the (sub)matrix
		  // by the diagonal entry in that row. The method modifies the original matrix.
		  // Parameters: n is the dimension of the submatrix to be normalized. The algorithm will only perform normalization on the upper 
		  //             left-hand square submatrix, up to row n-1 and column n-1. Setting n = pDim will normalize entire matrix.
		  
		  
		  Var nm1 As Integer = n - 1       // "n minus 1," largest index of submatrix to be normalized
		  
		  Var a(-1, -1) As Double          // array storing the entries of the matrix to be normalized
		  a.ResizeTo(pDim - 1, pDim - 1)   // resized as appropriate
		  a = pData                   // store data in variable a
		  
		  Var diag As Double   // variable which stores the diagonal in each row
		  For i As Integer = 0 To nm1   // loop over rows of submatrix
		    diag = a(i, i)     // get diagonal entry value
		    If diag <> 0.0 Then // Normalization can only occur if diagonal entry is nonzero
		      diagfactors.Add(diag)  // store the factor of the diagonal normalization in case we need to later reverse this process
		      For j As Integer = 0 To nm1 // loop over entries in each row
		        a(i, j) = a(i, j)/diag   // normalize row
		      Next
		    Else
		      diagfactors.Add(1)  // store a placeholder factor
		    End If
		  Next
		  
		  pData = a   // Self matrix entries replaced by normalized entries
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetIdentity(nDim As Integer) As Matrix
		  Var IM As New Matrix
		  IM.pDim = nDim     // stores dimension of matrix as property
		  Var n As Integer = nDim - 1
		  IM.pData.ResizeTo(n, n)  // resizes matrix data array appropriately (n-1 used because of 0-indexing)
		  
		  For i As Integer = 0 To n      // loop over matrix entries
		    For j As Integer = 0 To n
		      If i = j Then                  // if entry is on the diagonal...
		        IM.pData(i, j) = 1  // set entry = 1
		      Else                             // otherwise...
		        IM.pData(i, j) = 0  // set entry = 0
		      End If
		    Next
		  Next
		  Return IM
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetZeroMatrix(nDim As Integer) As Matrix
		  Var ZM As New Matrix
		  ZM.pDim = nDim     // stores dimension of matrix as property
		  Var n As Integer = nDim-1
		  ZM.pData.ResizeTo(n,n)  // resizes matrix data array appropriately (nDim-1 used because of 0-indexing)
		  
		  For i As Integer = 0 To n      // loop over matrix entries
		    For j As Integer = 0 To n
		      ZM.pData(i,j) = 0.0
		    Next
		  Next
		  Return ZM
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GuessInverse(n As Integer) As Matrix
		  // This method returns a "guess" for the inverse of the current matrix for use in the NumInvert method.
		  // Parameters: n gives the "depth of inversion." Method will obtain a guess only for the inverse of the submatrix with
		  //             maximum index n. Choosing n = pDim - 1 will yield a guess for the entire matrix
		  // The return matrix will only have highest index n (for use in NumInvert).
		  // Guess is determined using the procedures outlined in Soleymani (2012). 
		  
		  Var subMx As Matrix          
		  subMx = Self.ULSubmatrix(n)   // Create submatrix that we're guessing inverse for as separate Matrix object.
		  
		  Var guessData(-1, -1) As Double  // create array in which to store entries of guess matrix
		  guessData.ResizeTo(n, n)         // resize as appropriate
		  If IsDiagDom Then                // If matrix is diagonally dominant, use first "guess" algorithm...
		    For i As Integer = 0 To n
		      guessData(i, i) = 1/subMx.pData(i, i)   // guess matrix is a diagonal matrix whose entires are reciprocals of diagonals of original matrix.
		      Var guess As New Matrix(guessData)      // create guess matrix object
		      return guess
		    Next
		  Else   // If matrix not diagonally dominant, use second "guess" algorithm
		    Var normO As Double = subMx.Norm1  // gets the 1-norm...
		    Var normI As Double = subMx.NormInfty  // ...and the infinity-norm
		    Var guess As Matrix                    // create guess matrix object
		    guess = subMx.Transpose/(normO*normI)  // guess matrix is transpose of original matrix divided by product of two norms
		    return guess
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsDiagDom() As Boolean
		  // Returns true if the matrix is strictly diagonally dominant -- i.e., if in each row, the absolute value of the diagonal entry
		  // is greater than the sums of the absolute values of all the other entries -- and false otherwise.
		  // Used as a helper method in GuessInverse method.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var sum As Double             // sum is a dummy variable storing the sum of the absolute values of the non-diagonal entries for each row.
		  For i As Integer = 0 To n   // loop over rows
		    sum = 0
		    For j As Integer = 0 To n  // loop over row entries
		      If j <> i Then               // if entry is non-diagonal...
		        sum = sum + abs(pData(i,j))  // add absolute value to sum
		      End If
		    Next
		    If abs(pData(i, i)) <= sum Then   // if sum is greater than absolute value of diagonal entry...
		      return false                    // matrix not diagonally dominant, return false
		    End If
		  Next
		  return true                         // if we get here, all rows pass diag. dom. criterion, so matrix is diag. dom.
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub LUBackSub(n As Integer, indx() As Integer, soln() As Double)
		  // This is a "backsubstitution method." Given an LU-decomposed matrix, or a matrix with an LU-decomposed upper-left
		  // submatrix, the method solves the matrix equation LU*x = b for x.
		  // Parameters: n is the dimension of the decomposed submatrix
		  //             indx() is the row index storing the maximum normalized values, generated using the LUDecomp method.
		  //             soln() is the vector in which the solution is to be stored. Should be passed as an empty vector.
		  // Returns: nothing, but modifies the vector soln to now contain the solution x. 
		  
		  
		  Var nm1 As Integer = n - 1       // "n minus 1," largest index of submatrix decomposed in the LUDecomp method
		  
		  Var ii As Integer = -1
		  Var sum As Double
		  for i As Integer = 0 to nm1      // loop over columns of submatrix
		    Var ll As Integer = indx(i)    // index(i) is the row index of the original (undecomposed) matrix which had the max normalized value in col i
		    //                             // (generated) by LUDecomp method
		    sum=soln(ll)                   // stores value of solution vector in maximizing row (ll) as variable sum
		    soln(ll)=soln(i)               // update solution vector value 
		    if ii <> -1 then
		      Var im1 As Integer = i-1
		      For j As Integer = ii to im1
		        sum=sum-pData(i,j)*soln(j)
		      next
		    elseif sum <> 0 then
		      ii=i
		    end if
		    soln(i)=sum
		  next
		  for i As Integer = nm1 downto 0
		    sum=soln(i)
		    if i < nm1 then
		      Var ip1 As Integer = i+1
		      for j As Integer = ip1 to nm1
		        sum=sum-pData(i,j)*soln(j)
		      next
		    end if
		    soln(i)=sum/pData(i,i)
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function LUDecomp(n As Integer, indx() As Integer) As Integer
		  // Performs LU decomposition on the matrix using Crout's algorithm. Overwrites original matrix. Adapted from code included in
		  // William H. Press et al., "Numerical Recipes in Fortran 77: The Art of Scientific Computing", Cambridge UP, 1992.
		  // Parameters: n is the dimension of the submatrix to be decomposed. The algorithm will only perform decomposition on the upper 
		  //             left-hand square submatrix, up to row n-1 and column n-1. Setting n = pDim will decompose entire matrix.
		  //             indx() is an integer array used in pivoting in Crout's algorithm. It is modified by LUDecomp and then later used
		  //             in future back-substitution and inversion subroutines. 
		  // Returns: 0 if decomposition proceeds normally. If decomposition fails, returns the (1-based) index of the row that causes failure.
		  //          The method also modifies the original array a(,), overwriting its entries with the entries of the upper/lower matrices.
		  //          (We can choose the diagonal entries of the lower matrix to be all 1; diagonal entries of the modified matrix are of the
		  //          upper matrix.) 
		  
		  Var a(-1, -1) As Double   // initializes array for storing and updating matrix entries
		  a.resizeTo(pDim - 1, pDim - 1)  // resizes as appropriate
		  a = pData                 // fills with original matrix data
		  
		  Var nm1 As Integer = n - 1  // "n minus 1," largest index of submatrix to be decomposed
		  
		  indx.resizeTo(nm1)    // resizes indx() to be of same size as inverted submatrix
		  Var scale() As Double // create array of "scaling factors"
		  scale.ResizeTo(nm1) 
		  
		  // FIRST LOOP: check if there are any empty rows and populate scaling array.
		  Var aamax As Double                                 // aamax stores the largest entry in each row
		  For i As Integer = 0 To nm1                       // loop over submatrix rows                                   
		    aamax=0
		    For j As Integer = 0 To nm1
		      If abs(a(i,j)) > aamax Then aamax=abs(a(i,j))   // find largest element in row and sets aamax to have that value
		    Next
		    If aamax=0 Then                                   // if row is all zeroes...
		      Return i + 1                                    // returns the (1-indexed) row number; in this case decomposition algorithm won't work.
		    Else                                              // if row isn't all zeroes...
		      scale(i)=1/aamax                              // stores scaling factor for the row
		    End If
		  Next
		  
		  // SECOND LOOP: where the actual decomposition takes place
		  For j As Integer = 0 To nm1                    // loops over columns
		    Var sum As Double                                 // dummy variable used in calculating decomposed matrix entries
		    Var jm1 As Integer = j-1                          // "j minus one"
		    Var imax As Integer
		    for i As Integer = 0 to jm1               // loops over rows up to and including row jm1. These are the "upper triangular" entries
		      sum=a(i,j)                                      // updates value of sum to matrix entry (i, j) -- row i, col j
		      Var im1 As Integer = i-1                        // "i minus one"
		      for k As Integer = 0 to im1
		        sum=sum - a(i,k)*a(k,j)                       // calculates upper triangular entry...
		      next
		      a(i,j)=sum                                      // and places it in the matrix
		    next
		    aamax = 0
		    for i As Integer = j to nm1                                    // now we find the "lower triangular" entries, including diagonal elements...
		      sum=a(i,j)
		      for k As Integer = 0 to jm1
		        sum = sum - a(i,k)*a(k,j)                     // calculates lower triangular entry, but omits step where you divide by pivot
		      next
		      a(i,j) = sum                                    // places entry in the matrix
		      Var dum1 As Double = scale(i)*abs(sum)                       // multiplies value of the entry by scaling factor from first loop.
		      if dum1 >= aamax then                          // this loop determines which row in column j has the maximum normalized value; this is in case we have to pivot
		        imax=i                                        // if value satisfies maximizing criterion, store it in imax
		        aamax=dum1                                   // 
		      end if
		    next
		    if j <> imax then                                 // if max normalized value isn't in the diagonal entry, then we need to switch rows...
		      for k As Integer = 0 to nm1                                // runs over entire row
		        Var dum2 As Double =a(imax,k)                               // swaps row j with row imax
		        a(imax,k)=a(j,k)
		        a(j,k)=dum2
		      next
		      scale(imax)=scale(j)                        // updates scale array with proper value
		    end if
		    indx(j)=imax                                      // indx() stores which row (of the original matrix) had max normalized value, to "remember" where we swapped rows
		    if a(j,j)=0 then return j                         // If diagonal entry is zero, decomposition fails; return row which fails
		    if j <> nm1 then
		      Var dum3 As Double =1/a(j,j)                                    
		      Var jp1 As Integer = j+1
		      for i As Integer = jp1 to nm1
		        a(i,j)=a(i,j)*dum3                           // divide all non-diagonal entries by pivot!
		      next
		    end if
		  next
		  pData = a
		  return 0   
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function LUInvert(n As Integer) As Integer
		  // Inverts the n x n upper-left submatrix of the matrix object on which the method is called. 
		  // Returns: 0 if inversion proceeds as expected; if a row of the original matrix causes the inversion to fail,
		  //          returns the 1-based index of that row.
		  
		  Var b(-1) As Double
		  Var inverse(-1, -1) As Double
		  Var ipvt() As Integer
		  Var k As Integer = LUDecomp(n, ipvt)
		  
		  if k > 0 then
		    return k 
		  else
		    //  If we get here, things are OK, so we finish the inversion.
		    Var nm1 As Integer = n - 1
		    b.ResizeTo(nm1)
		    inverse.ResizeTo(nm1, nm1)
		    for j As Integer = 0 to nm1
		      for i As Integer = 0 to nm1
		        b(i) = 0
		      next
		      b(j) = 1
		      LUBackSub(n, ipvt, b)
		      for i As Integer = 0 to nm1
		        inverse(i,j) = b(i)
		      next
		    next
		    Var result(-1, -1) As Double
		    result.ResizeTo(pDim - 1, pDim - 1)
		    for i As Integer = 0 to pDim - 1
		      for j As Integer = 0 to pDim - 1
		        If i < n And j < n Then
		          result(i, j) = inverse(i, j)
		        Else
		          result(i, j) = pData(i, j)
		        End if
		      Next
		    Next
		    pData = result
		    return k
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Norm1() As Double
		  // Returns the 1-norm of the matrix.
		  // The 1-norm is the largest of the sums of the absolute values of the elements in each column of the matrix.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var sum As Double  // sum is a dummy variable storing the values of the column sums
		  Var maxSum As Double  // maxSum will store the largest value of sum among all columns
		  For j As Integer = 0 To n  // for each column...
		    sum = 0
		    For i As Integer = 0 To n // loop over column entries to calculate sum
		      sum = sum + abs(pData(i, j))  
		    Next
		    If sum > maxSum Then   // If sum is greater than the "candidate" maxSum...
		      maxSum = sum         // update maxSum's value with sum's value
		    End If
		  Next
		  
		  return maxSum  // true maxSum is the desired 1-norm.
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function NormInfty() As Double
		  // Returns the infinity-norm of the matrix.
		  // The infinity norm is the largest of the sums of the absolute values of the elements in each row of the matrix.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var sum As Double  // sum is a dummy variable storing the values of the row sums
		  Var maxSum As Double // maxSum will store the largest value of sum among all rows
		  For i As Integer = 0 To n  // for each row...
		    sum = 0
		    For j As Integer = 0 To n   // loop over row entries to calculate sum
		      sum = sum + abs(pData(i, j))  
		    Next
		    If sum > maxSum Then   // If sum is greater than the "candidate" maxSum...
		      maxSum = sum         // update maxSum's value with sum's value
		    End If
		  Next
		  
		  return maxSum  // true maxSum is the desired norm.
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function NumInvert(n As Integer, guess As Matrix, stepNo As Integer) As Integer
		  // This method uses an iterative algorithm introduced in F. Soleymani, "A Rapid Numerical Algorithm to Compute Matrix Inversion," Int'l Journal 
		  // of Math. and Math. Sci. (2012), doi:10.1155/2012/134653, to approximate the matrix inverse, especially useful for ill-conditioned matrices.
		  // Parameters: n is the dimension of the submatrix to be inverted. The algorithm will only perform inversion on the upper 
		  //             left-hand square submatrix, up to row n-1 and column n-1. Setting n = pDim will invert entire matrix.
		  //             guess is an initial guess for the matrix inverse. "Good" guesses can be generated using the GuessInverse method.
		  //             stepNo is the number of iterations of the algorithm to carry out.
		  // Returns: integer: 0 if inversion is OK. Also replaces original matrix with its inverse.
		  
		  Var subMx As Matrix   // creates submatrix object
		  subMx = Self.ULSubmatrix(n)
		  
		  Var nextGuess As Matrix            // initializes matrix variable for result of each step of iteration process
		  Var ident As Matrix = GetIdentity(n)       // creates identity matrix of same size as subMx
		  For i As Integer = 1 To stepNo                // iterates the process the specified number of times
		    nextGuess = (1.0/16.0)*guess*(120*ident + Self*guess*(-393*ident + Self*guess*(735*ident + Self*guess*(-861*ident + _
		    Self*guess*(651*ident + Self*guess*(-315*ident + Self*guess*(93*ident + Self*guess*(-15*ident + Self*guess))))))))
		    guess = nextGuess                // after each step, result of calculation becomes next guess
		  Next
		  
		  Var resData(-1, -1) As Double   // initializes array containing entries of our result matrix
		  resData.resizeTo(pDim-1, pDim-1)          // result matrix has same size as original matrix
		  For i As Integer = 0 To pDim - 1
		    For j As Integer = 0 To pDim - 1
		      If i < n And j < n Then                // if we're in the upper left-hand submatrix area...
		        resData(i, j) = nextGuess.pData(i, j)  // add inverted submatrix's entries
		      Else                                     // otherwise...
		        resData(i, j) = Self.pData(i, j)       // keep original matrix entries
		      End If
		    Next
		  Next
		  
		  
		  Self.pData = resData     // Update original matrix with partial inversion results
		  return 0                 // Return 0 to show inversion worked.
		  // NOTE: in theory, NumInvert should return something other than 0 if matrix isn't invertible, but haven't figured out a way to implement
		  // that with this method. Changes will need to be made.
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Add(rhs As Matrix) As Matrix
		  // Overloads the + (subtraction) operator to subtract two Matrix objects of the same size elementwise.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var s(-1, -1) As Double       // Initialize array of sum matrix entries
		  s.ResizeTo(n, n)              // Change size of array appropriately
		  
		  For i As Integer = 0 To n                      // loop over all matrix indices
		    For j As Integer = 0 To n
		      s(i, j) = self.pData(i, j) + rhs.pData(i, j)   // perform the matrix addition for each entry
		    Next
		  Next
		  
		  Var sum As New Matrix(s)    // convert sum array into Matrix object
		  return sum
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Divide(rhs As Double) As Matrix
		  // Overloads the / (divide) operator to perform elementwise division of a Matrix by a scalar.
		  
		  return (1/rhs)*Self     // dividing by a scalar is the same as multiplying by its reciprocal
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(rhs As Double) As Matrix
		  // Overloads the * (multiply) operator to multiply a Matrix object by a scalar (with scalar on the right).
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var a(-1, -1) As Double       // Initialize array of product matrix entries
		  a.ResizeTo(n, n)              // Change size of array appropriately
		  
		  For i As Integer = 0 To n     // loop over all matrix indices
		    For j As Integer = 0 To n
		      a(i, j) = rhs*pData(i, j)     // perform scalar multiplication for each entry
		    Next
		  Next
		  
		  Var product As New Matrix(a)      // convert product array into Matrix object
		  return product
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(rhs As Matrix) As Matrix
		  // Overloads the * (multiply) operator to multiply a Matrix by another Matrix (of the same size). 
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var a(-1, -1) As Double       // Initialize array of product matrix entries
		  a.ResizeTo(n, n)              // Change size of array appropriately
		  
		  // Do the matrix multiplication
		  For i As Integer = 0 To n                              // Loop over all indices of the *product* matrix
		    For j As Integer = 0 To n
		      Var aValue As Double                                   // Initialize variable that stores the (i, j)th value in the product matrix
		      For k As Integer = 0 To n                          // (i, j)th entry of product found by dotting row i of self with col j of rhs...
		        aValue = aValue + Self.pData(i, k)*rhs.pData(k, j)   // ... so loop over entries of corresponding row/col and sum their products
		      Next
		      a(i, j) = aValue                                       // store each value of product matrix
		    Next
		  Next
		  
		  Var product As New Matrix(a)  // create product as matrix object
		  return product
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(rhs As NVector) As NVector
		  // Overloads the * (multiply) operator to perform multiplication between a Matrix and a (column) Vector of appropriate size.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix/vector entries
		  
		  Var v() As Double             // Initialize array of product vector entries
		  v.ResizeTo(n)                 // Change size of array appropriately
		  
		  // Do the multiplication
		  
		  For i As Integer = 0 To n                        // loop over all indices of the product vector
		    Var vValue As Double                               // Initialize variable that stores the ith value in the product vector
		    For j As Integer = 0 To n                          // ith element in product found by dotting row i of self with rhs...
		      vValue = vValue + Self.pData(i, j)*rhs.pData(j)  // ... so loop over entries of row/vector and sum their products
		    Next
		    v(i) = vValue                                      // store each entry value
		  Next
		  
		  Var product As New NVector(v) // create product as vector object
		  return product
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_MultiplyRight(lhs As Double) As Matrix
		  // Overloads the * (multiply) operator to multiply a Matrix object by a scalar (with scalar on the left).
		  
		  return Self*lhs         // multiplying by a scalar on the left is equivalent to multiplying by a scalar on the right
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Subtract(rhs As Matrix) As Matrix
		  // Overloads the - (subtraction) operator to subtract two Matrix objects of the same size elementwise.
		  
		  return Self + (-1)*rhs   // subtracting a matrix is the same as adding a matrix with its elements negated
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RemoveRow(row As Integer)
		  // Removes the specified (1-indexed) row and column from the matrix, reducing the dimension of the matrix by one.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var newData(-1, -1) As Double // newData stores the data for the row- and column-deleted matrix
		  newData.ResizeTo(n-1, n-1) // modified matrix is one index smaller than original in both directions
		  
		  Var rm1 As Integer = row - 1 // "row number minus one"
		  Var cm1 As Integer = row- 1 // "column number minus one"
		  For i As Integer = 0 To n // loop over rows
		    For j As Integer = 0 To n // loop over columns
		      If i < rm1 And j < cm1 Then  // Case 1: entry is above/to the left of deleted row and column
		        newData(i, j) = pData(i, j) // index same as original
		      ElseIf i > rm1 And j < cm1 Then // Case 2: entry is below/to the left of deleted row and column
		        newData(i-1, j) = pData(i, j) // shift row index down by 1
		      ElseIf i < rm1 And j > cm1 Then // Case 3: entry is above/to the right of deleted row and column
		        newData(i, j-1) = pData(i, j) // shift column index down by 1
		      ElseIf i > rm1 And j > cm1 Then // Case 4: entry is below/to the right of deteled row and column
		        newData(i-1, j-1) = pData(i, j) // shift both indices down by 1
		      End If // elements in deleted row/column passed over
		    Next
		  Next
		  pData = newData  // set matrix to have new data
		  pDim = pDim - 1  // reduce matrix dimension number by 1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ResizeTo(NDimensions As Integer)
		  // Resizes the matrix to the appropriate value and updates the stored dimension. Also assumes the user will input a square matrix.
		  Var LastIndex As Integer = NDimensions - 1
		  pData.ResizeTo(LastIndex, LastIndex)
		  pDim = NDimensions
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToString() As String
		  // Creates and returns a representation of the matrix as a string.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  
		  Var matrixRep As String                                     // initialize string representation of matrix
		  For i As Integer = 0 To n                                   // loop over all matrix indices
		    For j As Integer = 0 To n - 1
		      matrixRep = matrixRep + Format(pData(i, j), "-#.##e") + "; "     // matrix elements in same row separated by semicolons
		    Next
		    matrixRep = matrixRep + Format(pData(i, n), "-#.##e") + EndOfLine  // last element in row has no semicolon; line break before next row
		  Next
		  
		  return matrixRep
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Transpose() As Matrix
		  // Returns the transpose of the matrix, which is the same as the original matrix but with rows and columns flipped.
		  // The original matrix object is preserved.
		  
		  Var n As Integer = pDim - 1   // n is the largest (0-based) index of matrix entries in both dimensions
		  Var tData(-1,-1) As Double    // initialize array containing transpose elements
		  tData.ResizeTo(n, n)          // change size to match original matrix
		  
		  For i As Integer = 0 To n     // loop over all original matrix indices
		    For j As Integer = 0 To n   
		      tData(j, i) = pData(i, j)  // fill in transpose array, switching indices to flip rows and columns
		    Next
		  Next
		  
		  Var myTranspose As New Matrix(tData)  // create transpose as matrix object
		  return myTranspose
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ULSubmatrix(n As Integer) As Matrix
		  // When called on a matrix, this method returns the upper-left n x n submatrix as a new matrix object. 
		  // Choosing n = pDim returns the original matrix.
		  // Used as a helper method in GuessInverse and NumInvert.
		  
		  Var nm1 As Integer = n - 1      // nm1 ("n minus 1") is the largest (0-based) index of the submatrix
		  
		  Var subData(-1, -1) As Double   // subData contains the entries of the submatrix being inverted
		  subData.ResizeTo(nm1, nm1)      // resize as appropriate
		  
		  For i As Integer = 0 To nm1     // loop over indices of submatrix
		    For j As Integer = 0 To nm1
		      subData(i, j) = pData(i, j)    // populate subData with submatrix entries
		    Next
		  Next
		  
		  Var subMx As New Matrix(subData)   // creates submatrix object
		  return subMx
		End Function
	#tag EndMethod


	#tag Note, Name = Description
		The Matrix class allows for the creation and manipulation of square matrices.
		Class properties: 
		 - pData is a 2D array which stores the entries of the matrix.
		 - pDim is an integer which stores the dimension of the matrix (the number of rows/columns). 
		
		Matrix methods include operator overloads for basic matrix arithmetic operations (addition, scalar
		multiplication, matrix multiplication) and several common linear algebra operations, including
		transposition, calculation of the sum and max norms, LU decomposition, and two methods for computing
		matrix inverses. 
		
		IdentityMatrix is a subclass of Matrix that allows for easy creation of identity matrices.
		
	#tag EndNote


	#tag Property, Flags = &h0
		diagfactors() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		pData(-1,-1) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		pDim As Integer
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
			Name="pDim"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
