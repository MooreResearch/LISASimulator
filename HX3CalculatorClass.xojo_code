#tag Class
Protected Class HX3CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 1, -2)
		  AddTerm(AddressOf GetA2, 2, -2)
		  AddTerm(AddressOf GetA3, 1, 2)
		  AddTerm(AddressOf GetA4, 2, 2)
		  AddTerm(AddressOf GetA5, 1, -5)
		  AddTerm(AddressOf GetA6, 2, -5)
		  AddTerm(AddressOf GetA7, 3, -5)
		  AddTerm(AddressOf GetA8, 4, -5)
		  AddTerm(AddressOf GetA9, 5, -5)
		  AddTerm(AddressOf GetA10, 1, -3)
		  AddTerm(AddressOf GetA11, 2, -3)
		  AddTerm(AddressOf GetA12, 3, -3)
		  AddTerm(AddressOf GetA13, 4, -3)
		  AddTerm(AddressOf GetA14, 5, -3)
		  AddTerm(AddressOf GetA15, 1, -1)
		  AddTerm(AddressOf GetA16, 2, -1)
		  AddTerm(AddressOf GetA17, 3, -1)
		  AddTerm(AddressOf GetA18, 4, -1)
		  AddTerm(AddressOf GetA19, 5, -1)
		  AddTerm(AddressOf GetA20, 0, 1)
		  AddTerm(AddressOf GetA21, 0, 3)
		  AddTerm(AddressOf GetA22, 1, 1)
		  AddTerm(AddressOf GetA23, 2, 1)
		  AddTerm(AddressOf GetA24, 1, 3)
		  AddTerm(AddressOf GetA25, 2, 3)
		  AddTerm(AddressOf GetA26, 3, 3)
		  AddTerm(AddressOf GetA27, 4, 3)
		  AddTerm(AddressOf GetA28, 5, 3)
		  AddTerm(AddressOf GetA29, 1, 5)
		  AddTerm(AddressOf GetA30, 2, 5)
		  AddTerm(AddressOf GetA31, 3, 5)
		  AddTerm(AddressOf GetA32, 4,5)
		  AddTerm(AddressOf GetA33, 5, 5 )
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = True  // This class is plus polarization
		  PNOrder = 2 // and for zeroth order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return 8*π*AP.C1*AP.Sβ*AP.S1^3
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^2*(-4923/512*AP.S2β+AP.C2*(459/128*AP.S2β-2079/256*AP.S4β)-945/1024*AP.S4β+AP.C4*(567/512*AP.S2β+1701/1024*AP.S4β))*AP.S1^4+AP.C1^2*(22203/1024*AP.S2β-AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)+945/2048*AP.S4β+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1*(27/16+1233/128*AP.C2β+27/128*AP.C4β+(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1^5+AP.C1*(-27/32-4689/256*AP.C2β-27/256*AP.C4β-(27/16+27/32*AP.C2β+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1^5)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*((4761/1024-1377/1024*AP.C2β)*AP.S2β+(837/256-621/256*AP.C2β)*AP.C2*AP.S2β+(243/1024-2187/1024*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6+((-11673/2048+1377/2048*AP.C2β)*AP.S2β+(-837/512+621/512*AP.C2β)*AP.C2*AP.S2β+(-243/2048+2187/2048*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1*((81/32-27/16*AP.C2β)*AP.Sβ^2-(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7+AP.C1*((-81/64+27/32*AP.C2β)*AP.Sβ^2+(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(81/64-81/32*AP.η)*AP.Cβ*AP.C1^2*AP.Sβ^3*AP.S1^8
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(683/16384*AP.Cβ*AP.Sβ+(557/4096-11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β-1/16384*AP.Cβ*AP.S3β+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(85/8192*AP.Cβ*AP.Sβ+(-679/6144+11/6144*AP.C2β)*AP.C4*AP.S2β-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β+1/8192*AP.Cβ*AP.S3β+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β)+AP.C8*(-37/24576*AP.S2β+91/16384*AP.S4β))+AP.C8*(37/49152*AP.S2β-91/32768*AP.S4β)+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*(19/512*AP.C4β*AP.C3+9/512*AP.C4β*AP.C5+AP.C1*(-11/16-35/128*AP.C2β+79/1536*AP.C4β+(1/32-37/256*AP.C2β)*AP.C2+(1/32+3/128*AP.C2β)*AP.C4-1/768*AP.C2β*AP.C6)-1/512*AP.C4β*AP.C7)*AP.S1^3+(-19/1024*AP.C4β*AP.C3-9/1024*AP.C4β*AP.C5+AP.C1*(19/32-23/768*AP.C2β-79/3072*AP.C4β-(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4+1/1536*AP.C2β*AP.C6)+1/1024*AP.C4β*AP.C7)*AP.S1^3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^2*(-355/1024*AP.S2β-AP.C2*(13/256*AP.S2β+11/512*AP.S4β)+AP.C4*(-1/1024*AP.S2β+9/2048*AP.S4β)-5/2048*AP.S4β)*AP.S1^4+AP.η*AP.C1^2*(-29/512*AP.S2β+AP.C4*(1/512*AP.S2β-9/1024*AP.S4β)+AP.C2*(13/128*AP.S2β+11/256*AP.S4β)+5/1024*AP.S4β)*AP.S1^4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA18(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^3*((7/48+1/24*AP.C2β)*AP.Sβ^2-(1/48+1/24*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5+AP.C1^3*(-(7/96+1/48*AP.C2β)*AP.Sβ^2+(1/96+1/48*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA19(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(1/96-1/48*AP.η)*AP.Cβ*AP.C1^4*AP.Sβ^3*AP.S1^6
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return -4*π*AP.Cβ*AP.S1^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA20(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-77/256+1/256*AP.Cβ)*AP.Sβ^2*AP.S4+(5/512+7/512*AP.C2β)*AP.Sβ^2*AP.S8+AP.η*((45/128-1/128*AP.C2β)*AP.Sβ^2*AP.S4-(5/256-7/256*AP.C2β)*AP.Sβ^2*AP.S8))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA21(AP As AmplitudeParameters) As Double
		  Return Parameters.δ* (135/64+189/64*AP.C2β-AP.η*(135/32+189/32*AP.C2β))*AP.C2*AP.Sβ^2*AP.S2^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA22(AP As AmplitudeParameters) As Double
		  Return Parameters.δ* (-683/16384*AP.Cβ*AP.Sβ+(-557/4096+11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β+AP.Cβ*AP.Sβ/16384+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(-85/8192*(AP.Cβ)*AP.Sβ+(679/6144-11/6144*AP.C2β)*AP.C4*AP.S2β-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β-AP.Cβ*AP.S3β/8192+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)+AP.C8*(37/24576*AP.S2β-91/16384*AP.S4β)-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β))+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β)+AP.C8*(-37/49152*AP.S2β+91/32768*AP.S4β))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA23(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^3*(19/32-23/768*AP.C2β-79/3072*AP.C4β+(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4-AP.C2β*AP.C6/1536)*AP.S1+19*AP.C4β*AP.C1^3*AP.S3/1024-9*AP.C4β*AP.C1^3*AP.S5/1024-AP.C4β*AP.C1^3*AP.S7/1024+AP.η*(AP.C1^3*(-11/16-35/128*AP.C2β+79/1536*AP.C4β+(-1/32+37/256*AP.C2β)*AP.C2+(1/32+3/128*AP.C2β)*AP.C4+1/768*AP.C2β*AP.C6)*AP.S1-19/512*AP.C4β*AP.C1^3*AP.S3+9/512*AP.C4β*AP.C1^3*AP.S5+1/512*AP.C4β*AP.C1^3*AP.S7))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA24(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^4*(4923/512*AP.S2β+AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)-945/2048*AP.S4β+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA25(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^5*(27/16+1233/128*AP.C2β+27/128*AP.C4β-(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1+AP.C1^5*(-27/32-4689/256*AP.C2β-27/256*AP.C4β+(27/16+27/32*AP.C2β+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA26(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^6*(11673/2048*AP.S2β+AP.C4*(243/2048*AP.S2β-2187/4096*AP.S4β)+AP.C2*(-837/512*AP.S2β+621/1024*AP.S4β)-1377/4096*AP.S4β)+AP.η*AP.C1^6*(-4761/1024*AP.S2β+AP.C2*(837/256*AP.S2β-621/512*AP.S4β)+1377/2048*AP.S4β+AP.C4*(-243/1024*AP.S2β+2187/2048*AP.S4β)))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA27(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^7*((-81/64+27/32*AP.C2β)*AP.Sβ^2-(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1+AP.η*AP.C1^7*((81/32-27/16*AP.C2β)*AP.Sβ^2+(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA28(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(81/32*AP.η-81/64)*AP.Cβ*AP.C1^8*AP.Sβ^3*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA29(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(4375/384*AP.S2β+4375/256*AP.S4β-AP.η*(4375/192*AP.S2β+4375/128*AP.S4β))*AP.C1^6*AP.S1^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return -8*π*AP.C1^3*AP.Sβ*AP.S1
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA30(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^7*AP.S1^3
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA31(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/256*AP.S2β-5625/512*AP.S4β+AP.η*(-625/128*AP.S2β+5625/256*AP.S4β))*AP.C1^8*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA32(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1^9*AP.Sβ^2*AP.S1
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA33(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/96*AP.η-625/192)*AP.Cβ*AP.C1^10*AP.Sβ^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return -4*π*AP.Cβ*AP.C1^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^4*(-4375/384*AP.S2β-4375/256*AP.S4β)*AP.S1^6+AP.η*AP.C1^4*(4375/192*AP.S2β+4375/128*AP.S4β)*AP.S1^6)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^3*AP.S1^7
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(-625/256*AP.S2β+5625/512*AP.S4β+AP.η*(625/48*AP.S2β-5625/256*AP.S4β))*AP.C1^2*AP.S1^8
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1*AP.Sβ^2*AP.S1^9
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(625/192-625/96*AP.η)*AP.Cβ*AP.Sβ^3*AP.S1^10
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetAllTerms(TheValues As CurrentValuesClass, TheDerivatives As CurrentDerivativesClass) As HTermData()
		  // Provide references to the current values and current derivatives. "CurrentValues" and
		  // "CurrentDerivatives" are properties of the superclass. These classes contain (as properties)
		  // all the information needed to calculate the wave and its derivatives.
		  CurrentValues = TheValues
		  CurrentDerivatives = TheDerivatives
		  
		  // Clear the TermData array for this PN polarization term
		  TermData.RemoveAll
		  Var AP As AmplitudeParameters = new AmplitudeParameters(Parameters)
		  // The calculation of all subterms in this polarization term should appear here. Write a method
		  // to calculate each subterm, then append the result to the TermData array. Something like
		  // TermData.Add(GetTermK), where GetTermK (K = 0, 1, 2, ...) that returns an HTermData instance.
		  // The GetTermK method in turn should call a separate method GetAK(AP As AmplitudeParameters)
		  // that calculates the amplitude alone (since we will need to call this multiple times to handle side cases)
		  // with tweaked values of the the parameters. The rest of the code in GetTermK should create a new
		  // instance of HTermData and set its properties appropriately (ignore the derivatives for the moment).
		  //TermData.Add(GetTerm1(AP))
		  //TermData.Add(GetTerm2(AP))
		  //TermData.Add(GetTerm3(AP))
		  //TermData.Add(GetTerm4(AP))
		  //TermData.Add(GetTerm5(AP))
		  //TermData.Add(GetTerm6(AP))
		  //TermData.Add(GetTerm7(AP))
		  //TermData.Add(GetTerm8(AP))
		  //TermData.Add(GetTerm9(AP))
		  //TermData.Add(GetTerm10(AP))
		  'TermData.Add(GetTerm11(AP))
		  'TermData.Add(GetTerm12(AP))
		  'TermData.Add(GetTerm13(AP))
		  'TermData.Add(GetTerm14(AP))
		  'TermData.Add(GetTerm15(AP))
		  'TermData.Add(GetTerm16(AP))
		  'TermData.Add(GetTerm17(AP))
		  'TermData.Add(GetTerm18(AP))
		  
		  // At the end, return the array you have created.
		  Return TermData
		  
		  
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="h"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDα"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΨr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDλ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cross"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNOrder"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
End Class
#tag EndClass
