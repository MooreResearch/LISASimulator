#tag Class
Protected Class vEquationSolver
	#tag Method, Flags = &h0
		Sub Constructor(myδ As Double, χ1ℓ As Double, χ2ℓ As Double)
		  // The vEquationSolver class contains a series of functions that make it easier to write equations in the evolver class. 
		  // All necessary constants and fixed parameters are set up by the constructor, and then any of the equation methods 
		  // can be called to calculate values of the wanted derivatives.
		  
		  //Define the various constants used in the evolution equations
		  π = 3.141592653589793238
		  c1 = 32/5
		  c2 = 743/336
		  c3 = 11/4
		  c4 = 47/3
		  c5 = 25/4
		  c6 = 34103/18144
		  c7 = 13661/2016
		  c8= 59/18
		  c9 = 31811/1008
		  c10 = 5039/84
		  c11 = 473/84
		  c12 = 1231/56
		  c13 = (4159*π)/672
		  c14 = (189*π)/8
		  c15 = 16447322263/139708800
		  c16 = 1712/105
		  c17 = (16*π^2)/3
		  c18 = 56198689/217728
		  c19 = (451*π^2)/48
		  c20 = 541/896
		  c21 = 5605/2592
		  c22 = 856/105
		  c23 = 4415/4032
		  c24 = 358675/6048
		  c25 = 91495/1512
		  γ = 0.5772156649015328606        //This is the Euler constant
		  
		  //Define the fixed paramters necessary to compute the equations
		  δ = myδ                                            //δ comes from the main program, and it's defined to be (m1 - m2)/M, where m1 is the larger mass
		  η = (1-δ^2)/4                                    //Defined to be m1m2/M^2
		  χs =(1/4)*( χ1ℓ*(1+δ)^2 + χ2ℓ*(1-δ)^2 )  //A necessary paramter to calculate vdot. Depends on the dot product between spin vectors and total angular momentum, as well as masses
		  χa = (1/2)*( χ2ℓ*(1-δ) - χ1ℓ*(1+δ) )          //Same as above
		  
		  
		  
		  //Define the necessary derivatives of η, χs, and χa with respect to δ, χ1ℓ, and χ2ℓ
		  dηdδ = -δ/2
		  dχsdδ = (1/2)*(χ1ℓ*(1+δ) - χ2ℓ*(1-δ))
		  dχsdχ1ℓ = (1/4)*(1+δ)^2
		  dχsdχ2ℓ = (1/4)*(1-δ)^2
		  dχadδ = -(1/2)*(χ2ℓ + χ1ℓ)
		  dχadχ1ℓ = -(1/2)*(1+δ)
		  dχadχ2ℓ = (1/2)*(1-δ)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdδ(v As Double, dvdδdot As Double) As Double
		  // This method is called by InitDvdδ and uses the time derivative of dvdδ to calculate dvdδ.
		  // The main equation is found by solving for dvdδ in terms of dvdδdot and v using the equation
		  // for dvdδdot.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _
		  + (c6 +c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  Var dvdδ As Double = (dvdδdot*(1/(c1*η*v^9)) _
		  - (1/η)*dηdδ*ζ _
		  - c3*v^2*dηdδ _
		  + (-c4*dχsdδ - c5*(χa + δ*dχadδ))*v^3 _
		  + dηdδ*(c7 + 2*c8*η)*v^4 _
		  + ((-c9 + c10*η)*dχsdδ + dηdδ*(c10*χs + δ*c12*χa + c14) + (-c11 + c12*η)*(χa + δ*dχadδ))*v^5 _
		  + (dηdδ*((-c18 + c19) + 2*c20*η - 3*c21*η^2))*v^6 _
		  + π*dηdδ*(c24 + 2*c25*η)*v^7) _
		  *((9/v)*ζ _
		  - 2*(c2 + c3*η)*v _
		  + 3*(4*π - c4*χs - δ*c5*χa)*v^2 _
		  + 4*(c6 + c7*η + c8*η^2)*v^3 _
		  + 5*((-c9 + c10*η)*χs + δ*χa*(-c11 + c12*η) + c13 + c14*η)*v^4 _
		  + 6*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - (c22/3))*v^5 _
		  + 7*π*(-c23 + c24*η + c25*η^2)*v^6)^(-1)                                                                           //This equation computes dvdδ from its time derivative and v
		  
		  Return dvdδ
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdδdot(v As Double, dvdδ As Double) As Double
		  // This method acts as a function, taking the values of v and dvdδ to produce dvdδdot.
		  // The main equation is found by taking the δ derivative of the vdot equation.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  Var dζdδ As Double = (-2*dvdδ*(c2 + c3*η))*v + (-c3*dηdδ + 3*dvdδ*(4*π - c4*χs - δ*c5*χa))*v^2 _
		  + (-c4*dχsdδ - c5*(χa + δ*dχadδ) + 4*dvdδ*(c6 + c7*η + c8*η^2))*v^3 _
		  + (dηdδ*(c7 + 2*c8*η) + 5*dvdδ*((-c9 + c10*η)*χs + δ*χa*(-c11 + c12*η) + c13 + c14*η))*v^4 _
		  + ((-c9 + c10*η)*dχsdδ + dηdδ*(c10*χs + δ*c12*χa + c14) + (-c11 + c12*η)*(χa + δ*dχadδ) + 6*dvdδ*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - c22/3))*v^5 _
		  + (dηdδ*((-c18 + c19)+2*c20*η - 3*c21*η^2) + 7*π*dvdδ*(-c23 + c24*η + c25*η^2))*v^6 + π*v^7*dηdδ*(c24 + 2*c25*η)    //This equation is meant purely for organization
		  Var dvdδdot As Double = c1*v^9*ζ*dηdδ + 9*c1*η*ζ*v^8*dvdδ + c1*η*v^9*dζdδ       //This gives the time derivative of dvdδ
		  
		  Return dvdδdot    //And this returns the value for the program to use
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdχ1ℓ(v As Double, dvdχ1ℓdot As Double) As Double
		  // This method is called by InitDvdχ1ℓ and uses the time derivative of dvdχ1ℓ to calculate dvdχ1ℓ.
		  // The main equation is found by solving for dvdχ1ℓ in terms of dvdχ1ℓdot and v using the equation
		  // for dvdχ1ℓdot.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π-  c4*χs - δ*c5*χa)*v^3 _
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  
		  Var dvdχ1ℓ As Double = (dvdχ1ℓdot*(1/(c1*η)) + (c4*dχsdχ1ℓ + δ*c5*dχadχ1ℓ)*v^12 _
		  - ((-c9 + c10*η)*dχsdχ1ℓ + δ*(-c11 + c12*η)*dχadχ1ℓ)*v^14) _
		  *(9*ζ*v^8 _
		  - 2*(c2 + c3*η) + 3*(4*π - c4*χs - δ*c5*χa)*v^2 _
		  + 4*(c6 + c7*η*c8*η^2)*v^3 _
		  + 5*((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^4 _
		  + 6*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - (c22/3))*v^5 _
		  + 7*π*(-c23 + c24*η + c25*η^2)*v^6)^(-1)                                                                           //This equation computes dvdχ1ℓ from its time derivative and v
		  
		  Return dvdχ1ℓ
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdχ1ℓdot(v As Double, dvdχ1ℓ As Double) As Double
		  // This method acts as a function, taking the values of v and dvdχ1ℓ to produce dvdχ1ℓdot.
		  // The main equation is found by taking the χ1ℓ derivative of the vdot equation.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 +(-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  Var dζdχ1ℓ As Double = (-2*dvdχ1ℓ*(c2 + c3*η))*v + (3*dvdχ1ℓ*(4*π - c4*χs - δ*c5*χa))*v^2 _
		  + (-c4*dχsdχ1ℓ - δ*c5*dχadχ1ℓ + 4*dvdχ1ℓ*(c6 + c7*η + c8*η^2))*v^3 _
		  + 5*dvdχ1ℓ*((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^4 _
		  + ((-c9 + c10*η)*dχsdχ1ℓ + δ*(-c11 + c12*η)*dχadχ1ℓ + 6*dvdχ1ℓ*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - c22/3))*v^5 _
		  + 7*π*dvdχ1ℓ*(-c23 + c24*η + c25*η^2)*v^6 //This equation is meant purely for organization
		  Var dvdχ1ℓdot As Double = c1*η*(9*v^8*dvdχ1ℓ*ζ + v^9*dζdχ1ℓ)   //This gives the time derivative of dvdχ1ℓ
		  
		  Return dvdχ1ℓdot    //And this returns the value for the program to use
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdχ2ℓ(v As Double, dvdχ2ℓdot As Double) As Double
		  // This method is called by InitDvdχ2ℓ and uses the time derivative of dvdχ2ℓ to calculate dvdχ2ℓ.
		  // The main equation is found by solving for dvdχ2ℓ in terms of dvdχ2ℓdot and v using the equation
		  // for dvdχ2ℓdot.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2+c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  
		  Var dvdχ2ℓ As Double = (dvdχ2ℓdot*(1/(c1*η)) + (c4*dχsdχ2ℓ + δ*c5*dχadχ2ℓ)*v^12 _
		  - ((-c9 + c10*η)*dχsdχ2ℓ + δ*(-c11 + c12*η)*dχadχ2ℓ)*v^14) _
		  *(9*ζ*v^8 _
		  - 2*(c2 + c3*η) + 3*(4*π - c4*χs - δ*c5*χa)*v^2 _
		  + 4*(c6 + c7*η*c8*η^2)*v^3 _
		  + 5*((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^4 _
		  + 6*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - (c22/3))*v^5 _
		  + 7*π*(-c23 + c24*η + c25*η^2)*v^6)^(-1)                                                                           //This equation computes dvdχ2ℓ from its time derivative and v
		  
		  Return dvdχ2ℓ
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDvdχ2ℓdot(v As Double, dvdχ2ℓ As Double) As Double
		  // This method acts as a function, taking the values of v and dvdχ2ℓ to produce dvdχ2ℓdot.
		  // The main equation is found by taking the χ2ℓ derivative of the vdot equation.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7    //This equation is meant purely for organization
		  Var dζdχ2ℓ As Double = (-2*dvdχ2ℓ*(c2 + c3*η))*v + (3*dvdχ2ℓ*(4*π - c4*χs - δ*c5*χa))*v^2 _
		  + (-c4*dχsdχ2ℓ - δ*c5*dχadχ2ℓ + 4*dvdχ2ℓ*(c6 + c7*η + c8*η^2))*v^3 _
		  + 5*dvdχ2ℓ*((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^4 _
		  + ((-c9 + c10*η)*dχsdχ2ℓ + δ*(-c11 + c12*η)*dχadχ2ℓ + 6*dvdχ2ℓ*(c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2 - c22/3))*v^5 _
		  + 7*π*dvdχ2ℓ*(-c23 + c24*η + c25*η^2)*v^6 //This equation is meant purely for organization
		  Var dvdχ2ℓdot As Double = c1*η*(9*v^8*dvdχ2ℓ*ζ + v^9*dζdχ2ℓ)   //This gives the time derivative of dvdχ2ℓ
		  
		  Return dvdχ2ℓdot    //And this returns the value for the program to use
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetVdot(v As Double) As Double
		  // This method acts as a function, taking the value of v to return vdot.
		  
		  Var Ln16v2 As Double = Log(16*v^2) //This variable makes it easier to format the equations
		  
		  Var ζ As Double = 1 - (c2 + c3*η)*v^2 + (4*π - c4*χs - δ*c5*χa)*v^3 _                       //This equation is meant purely for organization
		  + (c6 + c7*η + c8*η^2)*v^4 + ((-c9 + c10*η)*χs + δ*(-c11 + c12*η)*χa + c13 + c14*η)*v^5 _
		  + (c15 - c16*γ + c17 + (-c18 + c19)*η + c20*η^2 - c21*η^3 - c22*Ln16v2)*v^6 + π*(-c23 + c24*η + c25*η^2)*v^7
		  Var vdot As Double = c1*η*v^9*ζ     //This equation gives the time derivative of v
		  
		  Return vdot     //And this returns the value for the program to use
		  
		  
		  'Here are the constant values for reference:
		  'c1 = 32/5
		  'c2 = 743/336
		  'c3 = 11/4
		  'c4 = 47/3
		  'c5 = 25/4
		  'c6 = 34103/18144
		  'c7 = 13661/2016
		  'c8= 59/18
		  'c9 = 31811/1008
		  'c10 = 5039/84
		  'c11 = 473/84
		  'c12 = 1231/56
		  'c13 = (4159*π)/672
		  'c14 = (189*π)/8
		  'c15 = 16447322263/139708800
		  'c16 = 1712/105
		  'c17 = (16*π^2)/3
		  'c18 = 56198689/217728
		  'c19 = (451*π^2)/48
		  'c20 = 541/896
		  'c21 = 5605/2592
		  'c22 = 856/105
		  'c23 = 4415/4032
		  'c24 = 358675/6048
		  'c25 = 91495/1512
		  'γ = 0.5772156649015328606
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		#tag Note
			//Value: 32/5
		#tag EndNote
		c1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 5039/84
		#tag EndNote
		c10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 473/84
		#tag EndNote
		c11 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 1231/56
		#tag EndNote
		c12 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: (4159*π)/672
		#tag EndNote
		c13 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: (189*π)/8
		#tag EndNote
		c14 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 16447322263/139708800
		#tag EndNote
		c15 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 1712/105
		#tag EndNote
		c16 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: (16*π^2)/3
		#tag EndNote
		c17 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 56198689/217728
		#tag EndNote
		c18 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: (451*π^2)/48
		#tag EndNote
		c19 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 743/336
		#tag EndNote
		c2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 541/896
		#tag EndNote
		c20 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 5605/2592
		#tag EndNote
		c21 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 856/105
		#tag EndNote
		c22 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 4415/4032
		#tag EndNote
		c23 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 358675/6048
		#tag EndNote
		c24 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 91495/1512
		#tag EndNote
		c25 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 11/4
		#tag EndNote
		c3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 47/3
		#tag EndNote
		c4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 25/4
		#tag EndNote
		c5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 34103/18144
		#tag EndNote
		c6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 13661/2016
		#tag EndNote
		c7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 59/18
		#tag EndNote
		c8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 31811/1008
		#tag EndNote
		c9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dηdδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 0.5772156649015328606
		#tag EndNote
		γ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χa As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χs As Double
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
			Name="dηdδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ1ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ2ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ1ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ2ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="δ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="η"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χa"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χs"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="γ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="π"
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
			Name="c2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3"
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
			Name="c5"
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
			Name="c7"
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
			Name="c9"
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
			Name="c11"
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
			Name="c13"
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
			Name="c15"
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
			Name="c17"
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
			Name="c19"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c20"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c21"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c22"
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
			Name="c24"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c25"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
