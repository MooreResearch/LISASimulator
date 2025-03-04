#tag Class
Protected Class IotaFuncsClass
	#tag Method, Flags = &h0
		Sub SetDerivs(theIota As Double)
		  // Set up basic frequencies
		  c2 = Cos(theIota)
		  s2 = Sin(theIota)
		  c1 = Cos(0.5*theIota)
		  s1 = Sin(0.5*theIota)
		  Var c3 As Double = c2*c1 - s2*s1
		  Var s3 As Double = s2*c1 + c2*s1
		  c4 = c2*c2-s2*s2
		  s4 = 2*c2*s2
		  Var c5 As Double = c4*c1 - s4*s1
		  Var s5 As Double = s4*c1 + c4*s1
		  c6 = c5*c1 - s5*s1
		  s6  = s5*c1 + c5*s1
		  Var c7 As Double = c6*c1 - s6*s1
		  Var s7 As Double = s6*c1 + c6*s1
		  c8 = c7*c1 - s7*s1
		  s8 = s7*c1 + c7*s1
		  Var c9 As Double = c8*c1 - s8*s1
		  Var s9 As Double = s8*c1 + c8*s1
		  Var c10 As Double = c9*c1 - s9*s1
		  Var s10 As Double = s9*c1 + c9*s1
		  
		  // set up basic powers
		  c12 = c1*c1
		  Var c13 As Double = c12*c1
		  c14 = c13*c1
		  Var c15 As Double = c14*c1
		  c16 = c15*c1
		  Var c17 As Double = c16*c1
		  c18 = c17*c1
		  Var c19 As Double = c18*c1
		  c110 = c19*c1
		  c23 = c2*c2*c2
		  s12 = s1*s1
		  s13 = s12*s1
		  s14 = s13*s1
		  Var s15 As Double = s14*s1
		  s16 = s15*s1
		  Var s17 As Double = s16*s1
		  s18 = s17*s1
		  Var s19 As Double = s18*s1
		  s110 = s19*s1
		  s22 = s2*s2
		  s23 = s22*s2
		  s24 = s23*s2
		  s25 = s24*s2
		  
		  // Now set up derivatives for functions not listed above
		  // Note that there is a factor of 0.5 associated with each derivative
		  // compared to what you might expect, because
		  // d(s1)/dι = 0.5*c1 and d(c1)/dι = -0.5*s1
		  
		  c12s14 = -c1*s1*s14 +2.0*c12*s13*c1
		  c12s16 = -c1*s1*s14 +3.0+c12*s15*c1
		  c12s18 = -c1*s1*s14 +4.0+c12*s17*c1
		  c13s1 = -1.5*c12*s1*s1 + c13*0.5*c1
		  c13s15 = -1.5*c12*s1*s15 + 2.5*c13*s14*c1
		  c13s17 = -1.5*c12*s1*s15 + 3.5*c13*s16*c1
		  c13s3 = -1.5*c12*s1*s3 + 1.5*c13*c3
		  c13s5 = -1.5*c12*s1*s3 + 2.5*c13*c5
		  c13s7 = -1.5*c12*s1*s3 + 3.5*c13*c7
		  c14s12 = -2.0*c13*s1*s12 + c14*s1*c1
		  c14s16 = -2.0*c13*s1*s12 + 3.0*c15*s1*c1
		  c15s1 = -2.5*c14*s1*s1 + 0.5*c15*c1
		  c15s12 = -2.5*c14*s1*s1 + c15*s1*c1
		  c15s13 = -2.5*c14*s1*s1 + 1.5*c15*s12*c1
		  c16s12 = -3.0*c15*s1*s12 + c16*s1*c1
		  c16s14 = -3.0*c15*s1*s14 + 2.0*c16*s13*c1
		  c17s1 = -3.5*c16*s1*s1 + 0.5*c17*c1
		  c17s13 = -3.5*c16*s1*s1 + 1.5*c17*s12*c1
		  c18s12 = -4.0*c17*s1*s12 + c18*s1*c1
		  c19s1 = -4.5*c18*s1*s1 + 0.5*c19*c1
		  c1c2s13 = -0.5*s1*c2*s13 - c1*s2*s13 + 1.5*c1*c2*s12*c1
		  c1c2s15 = -0.5*s1*c2*s15 - c1*s2*s15 + 2.5*c1*c2*s14*c1
		  c1c2s17 = -0.5*s1*c2*s17 - c1*s2*s17 + 3.5*c1*c2*s16*c1
		  c1c4s13 = -0.5*s1*c4*s13 - 2.0*c1*s4*s13 + 1.5*c1*c4*s12*c1
		  c1c4s15 = -0.5*s1*c4*s15 - 2.0*c1*s4*s15 + 2.5*c1*c4*s14*c1
		  c1c6s13 = -0.5*s1*c6*s13 - 3.0*c1*s6*s13 + 1.5*c1*c6*s12*c1
		  c1s13 = -0.5*s1*s13 + 1.5*c1*s12*c1
		  c1s15 = -0.5*s1*s15 + 2.5*c1*s14*c1
		  c1s17 = -0.5*s1*s17 + 3.5*c1*s16*c1
		  c1s19 = -0.5*s1*s19 + 4.5*c1*s18*c1
		  c2c12 = -s2*c12 - c2*c1*s1
		  c2c12s14 = -s2*c12*s14 - c2*c1*s1*s14 + 2.0*c2*c12*s13*c1
		  c2c13s1 = -s2*c13*s1 - 1.5*c2*c12*s1*s1 + 0.5*c2*c13*c1
		  c2c13s15 = -s2*c13*s1 - 1.5*c2*c12*s1*s1 + 2.5*c2*c13*s14*c1
		  c2c14 = -s2*c14 - 2.0*c2*c13*s1
		  c2c14s12 = -s2*c14*s12 - 2.0*c2*c13*s1*s12 + c2*c14*s1*c1
		  c2c15s1 = -s2*c15*s1 - 2.5*c2*c14*s1*s1 + 0.5*c2*c15*c1
		  c2c15s12 = -s2*c15*s12 + 2.5*c2*c14*s1*s12 + c2*c15*s1*c1
		  c2c15s13 = -s2*c15*s13 - 2.5*c2*c14*s1*s13 + 1.5*c2*c15*s12
		  c2c16 = -s2*c16 - 3.0*c2*c15*s1
		  c2c17s1 = -s2*c17*s1 - 3.5*c2*c16*s1*s1 + 0.5*c2*c17*c1
		  c2s12 = -s2*s12 + c2*s1*c1
		  c2s14 =  -s2*s14 + 2.0*c2*s13*c1
		  c2s16 =  -s2*s16 + 4.0*c2*s15*c1
		  c2s2 = -s2*s2 + c2*c2
		  c2s22 = -s2*s22 + 2.0*c2*s2*c2
		  c2s23 = -s2*s23 + 3.0*c2*s22*c2
		  c3s13 = -1.5*s3*s13 + 1.6*c3*s12*c1
		  c4c12 = -2.0*s4*c12 - c4*c1*s1
		  c4c12s14 = -2.0*s4*c12*s14 - c4*c1*s1*s14 + 2.0*c4*c12*s13*c1
		  c4c13s1 = -2.0*s4*c13*s1 - 1.5*c4*c12*s1*s1 + 0.5*c4*c13*c1
		  c4c14 = -2.0*s4*c14 - 2.0*c4*c13*s1
		  c4c14s12 = -2.0*s4*c14*s12 - 2.0*c4*c13*s1*s12 + c4*c14*s1*c1
		  c4c15s1 = -2.0*s4*c15*s1- 2.5*c4*c14*s1*s1 + 0.5*c4*c15*c1
		  c4c16 = -2.0*s4*c16 - 3.0*c4*c15*s1
		  c4s12 = -2.0*s4*s12 + c4*s1*c1
		  c4s14 = -2.0*s4*s14 + 2.0*c4*s13*c1
		  c4s16 = -2.0*s4*s16 + 3.0*c4*s15*c1
		  c4s2 = -2.0*s4*s2 + c4*c2
		  c4s22 = -2.0*s4*s22 + 2.0*c4*s2*c2
		  c4s23 = -2.0*s4*s23 + 3.0*c4*s22*c2
		  c5s13 = -2.5*s5*s13 + 1.5*c5*s12*c1
		  c6c12 = -3.0*s6*c12 - c6*c1*s1
		  c6c13s1 = -3.0*s6*c13*s1 - 1.5*c6*c1*s1*s1 + 0.5*c6*c13*c1
		  c6s12 = -3.0*s6*s12 + c6*s1*c1
		  c6s2 = -3.0*s6*s2 + c6*c2
		  c7s13 = -3.5*s7*s13 + 1.5*c7*s12*c1
		  c8c12 = -4.0*s8*c12 - c8*c1*s1
		  c8s2 = -4.0*s8*s2 + c8*c2
		  
		  // now reset the basic functions to their derivatives
		  // work backwards to continue using the basic definitions
		  // we established at the top
		  
		  // set up the basic power derivatives
		  c12 = -c1*s1
		  c14 = -2.0*c13*s1
		  c16 = -3.0*c15*s1
		  c18 = -4.0*c17*s1
		  c110 = -5.0*c19*s1
		  c23 = -3.0*c2*c2*s2
		  s110 = 5.0*s19*c1
		  s18 = 4.0*s17*c1
		  s16 = 3.0*s15*c1
		  s14 = 2.0*s13*c1
		  s13 = 1.5*s12*c1
		  s12 = s1*c1
		  s25 = 5.0*s24*c2
		  s24 = 4.0*s23*c2
		  s23 = 3.0*s22*c2
		  s22 = 2.0*s2*c2
		  
		  // set up temporary derivatives the sine variables so that we can
		  // use the sine powers when we set up cosine derivatives
		  
		  Var s1d As Double = 0.5*c1
		  Var s2d As Double = c2
		  Var s4d As Double = 2.0*c4
		  Var s6d As Double = 3.0*c6
		  Var s8d As Double = 4.0*c8
		  Var s10d As Double = 5.0*c10
		  
		  // Now set the the basic derivatives
		  c1 = -0.5*s1
		  s1 = s1d
		  c2 = -s2
		  s2 = s2d
		  c4 = -2.0*s4
		  s4 = s4d
		  c6 = -3.0*s6
		  s6 = s6d
		  c8 = -4.0*s8
		  s8 = s8d
		  c10 = -5.0*s10
		  s10 = s10d
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetValues(theIota As Double)
		  // Set up basic frequencies
		  c2 = Cos(theIota)
		  s2 = Sin(theIota)
		  c1 = Cos(0.5*theIota)
		  s1 = Sin(0.5*theIota)
		  Var c3 As Double = c2*c1 - s2*s1
		  Var s3 As Double = s2*c1 + c2*s1
		  c4 = c2*c2-s2*s2
		  s4 = 2*c2*s2
		  Var c5 As Double = c4*c1 - s4*s1
		  Var s5 As Double = s4*c1 + c4*s1
		  c6 = c5*c1 - s5*s1
		  s6  = s5*c1 + c5*s1
		  Var c7 As Double = c6*c1 - s6*s1
		  Var s7 As Double = s6*c1 + c6*s1
		  c8 = c7*c1 - s7*s1
		  s8 = s7*c1 + c7*s1
		  Var c9 As Double = c8*c1 - s8*s1
		  Var s9 As Double = s8*c1 + c8*s1
		  Var c10 As Double = c9*c1 - s9*s1
		  Var s10 As Double = s9*c1 + c9*s1
		  
		  // set up basic powers
		  c12 = c1*c1
		  Var c13 As Double = c12*c1
		  c14 = c13*c1
		  Var c15 As Double = c14*c1
		  c16 = c15*c1
		  Var c17 As Double = c16*c1
		  c18 = c17*c1
		  Var c19 As Double = c18*c1
		  c110 = c19*c1
		  c23 = c2*c2*c2
		  s12 = s1*s1
		  s13 = s12*s1
		  s14 = s13*s1
		  Var s15 As Double = s14*s1
		  s16 = s15*s1
		  Var s17 As Double = s16*s1
		  s18 = s17*s1
		  Var s19 As Double = s18*s1
		  s110 = s19*s1
		  s22 = s2*s2
		  s23 = s22*s2
		  s24 = s23*s2
		  s25 = s24*s2
		  
		  // Now set up values for functions not listed above
		  
		  c12s14 = c12*s14
		  c12s16 = c12*s16
		  c12s18 = c12*s18
		  c13s1 = c13*s1
		  c13s15 = c13*s15
		  c13s17 = c13*s17
		  c13s3 = c13*s3
		  c13s5 = c13*s5
		  c13s7 = c13*s7
		  c14s12 = c14*s12
		  c14s16 = c14*s16
		  c15s1 = c15*s1
		  c15s12 = c15*s12
		  c15s13 = c15*s13
		  c16s12 = c16*s12
		  c16s14 = c16*s14
		  c17s1 = c17*s1
		  c17s13 = c17*s13
		  c18s12 = c18*s12
		  c19s1 = c19*s1
		  c1c2s13 = c1*c2*s13
		  c1c2s15 = c1*c2*s15
		  c1c2s17 = c1*c2*s17
		  c1c4s13 = c1*c4*s13
		  c1c4s15 = c1*c4*s15
		  c1c6s13 = c1*c6*s13
		  c1s13 = c1*s13
		  c1s15 = c1*s15
		  c1s17 = c1*s17
		  c1s19 = c1*s19
		  c2c12 = c2*c12
		  c2c12s14 = c2*c12*s14
		  c2c13s1 = c2*c13*s1
		  c2c13s15 = c2*c13*s15
		  c2c14 = c2*c14
		  c2c14s12 = c2*c14*s12
		  c2c15s1 = c2*c15*s1
		  c2c15s12 = c2*c15*s12
		  c2c15s13 = c2*c15*s13
		  c2c16 = c2*c16
		  c2c17s1 = c2*c17*s1
		  c2s12 = c2*s12
		  c2s14 =  c2*s14
		  c2s16 =  c2*s16
		  c2s2 = c2*s2
		  c2s22 = c2*s22
		  c2s23 = c2*s23
		  c3s13 = c3*s13
		  c4c12 = c4*c12
		  c4c12s14 = c4*c12*s14
		  c4c13s1 = c4*c13*s1
		  c4c14 = c4*c14
		  c4c14s12 = c4*c14*s12
		  c4c15s1 = c4*c15*s1
		  c4c16 = c4*c16
		  c4s12 = c4*s12
		  c4s14 = -c4*s14
		  c4s16 = c4*s16
		  c4s2 = c4*s2
		  c4s22 = c4*s22
		  c4s23 = c4*s23
		  c5s13 = c5*s13
		  c6c12 = c6*c12
		  c6c13s1 = c6*c13*s1
		  c6s12 = c6*s12
		  c6s2 = c6*s2
		  c7s13 = c7*s13
		  c8c12 = c8*c12
		  c8s2 = c8*s2
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		c0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c110 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c12s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c12s16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c12s18 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s17 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c13s7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c14s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c14s16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c15s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c15s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c15s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c16s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c16s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c17s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c17s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c18 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c18s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c19s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c2s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c2s15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c2s17 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c4s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c4s15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1c6s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1s15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1s17 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1s19 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c23 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c12s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c13s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c13s15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c14s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c15s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c15s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c15s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2c17s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s22 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2s23 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c3s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c12s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c13s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c14s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c15s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4c16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s22 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4s23 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c5s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6c13s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c7s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c8c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c8s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c8s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s110 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s18 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s22 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s23 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s24 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s25 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s8 As Double
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
			Name="c0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c23"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c110"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c12s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c12s16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c12s18"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s15"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s17"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c13s7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c14s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c14s16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c15s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c15s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c15s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c16s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c16s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c17s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c17s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c18"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c18s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c19s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c2s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c2s15"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c2s17"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c4s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c4s15"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1c6s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1s15"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1s17"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1s19"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c12s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c13s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c13s15"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c14s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c15s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c15s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c17s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s22"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c12s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c13s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c14s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c15s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4c16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s22"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4s23"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c5s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6c12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6c13s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c7s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8c12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s22"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s23"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s24"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s25"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s13"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s14"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s16"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s18"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s110"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2c15s12"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2s23"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
