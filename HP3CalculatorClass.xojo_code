#tag Class
Protected Class HP3CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()

		  AddTerm(AddressOf GetA1, 2, 2, False)
		  AddTerm(AddressOf GetA2, 1, 2, False)
		  AddTerm(AddressOf GetA3, 1, -2, False)
		  AddTerm(AddressOf GetA4, 2, -2, False)
		  AddTerm(AddressOf GetA5, 0, 2, False)
		  AddTerm(AddressOf GetA6, 5, 5, False)
		  AddTerm(AddressOf GetA7, 1, 1, False)
		  AddTerm(AddressOf GetA8, 3, 3, False)
		  AddTerm(AddressOf GetA9, 4, 5, False)
		  AddTerm(AddressOf GetA10,4, 3, False)
		  AddTerm(AddressOf GetA11, 5, 3, False)
		  AddTerm(AddressOf GetA12, 1, -1, False)
		  AddTerm(AddressOf GetA13, 3, 1, False)
		  AddTerm(AddressOf GetA14, 3, 5, False)
		  AddTerm(AddressOf GetA15, 1, 3, False)
		  AddTerm(AddressOf GetA16, 2, 5, False)
		  AddTerm(AddressOf GetA17, 4, 1, False)
		  AddTerm(AddressOf GetA18, 5, 1, False)
		  AddTerm(AddressOf GetA19, 3, -1, False)
		  AddTerm(AddressOf GetA20, 1, 5, False)
		  AddTerm(AddressOf GetA21, 1, -3, False)
		  AddTerm(AddressOf GetA22, 4, -1, False)
		  AddTerm(AddressOf GetA23, 5, -1, False)
		  AddTerm(AddressOf GetA24, 3, -3, False)
		  AddTerm(AddressOf GetA25, 1, -5, False)
		  AddTerm(AddressOf GetA26, 2, -5, False)
		  AddTerm(AddressOf GetA27, 4, -3, False)
		  AddTerm(AddressOf GetA28, 5, -3, False)
		  AddTerm(AddressOf GetA29, 3, -5, False)
		  AddTerm(AddressOf GetA30, 4, -5, False)
		  AddTerm(AddressOf GetA31, 5, -5, False)
		  AddTerm(AddressOf GetA32, 0, 3, False)
		  AddTerm(AddressOf GetA33, 0, 5, False)
		  AddTerm(AddressOf GetA34, 2, 3, False)
		  AddTerm(AddressOf GetA35, 2, -3, False)
		  AddTerm(AddressOf GetA36, 2, 1, False)
		  AddTerm(AddressOf GetA37, 2, -1, False)
		  AddTerm(AddressOf GetA38, 0, 1, False)
		  

		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = False  // This class is plus polarization
		  PNOrder = 3 // and for third post-Newtonian order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod


	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return -(3*π+π*AP.C2β)*AP.C1^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2+AP.η*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2))*AP.C1^7*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((AP.η*(243/128+81/128*AP.C2β)-(243/256+81/256*AP.C2β))*AP.C1^8*AP.Sβ^3*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-43723/98304*AP.Sβ+9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ+1901/8192*AP.S3β-35/24576*AP.S5β)+AP.C4*(-1103/24576*AP.Sβ+2833/16384*AP.S3β-21/16384*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)+AP.C8*(7/98304*AP.Sβ+7/65536*AP.S3β-35/65536*AP.S5β)-155/196608*AP.S5β)*AP.S1^2+AP.η*(7449/16384*AP.Sβ+331/32768*AP.S3β+AP.C8*(-7/49152*AP.Sβ-7/32768*AP.S3β+35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Sβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C4*(-337/12288*AP.Sβ+47/8192*AP.S3β+21/8192*AP.S5β)+AP.C2*(1873/2048*AP.Sβ+19/4096*AP.S3β+35/12288*AP.S5β)+155/98304*AP.S5β)*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^4*(1675/4096*AP.Sβ+825/8192*AP.S3β-AP.C4*(7/4096*AP.Sβ+13/8192*AP.S3β+15/8192*AP.S5β)+AP.C2*(27/1024*AP.Sβ-151/2048*AP.S3β+3/2048*AP.S5β)-13/8192*AP.S5β)*AP.S1^2+AP.η*AP.C1^4*(245/2048*AP.Sβ-57/4096*AP.S3β+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)+AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)+13/4096*AP.S5β)*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β)-(4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β))*AP.C1^8*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^4*(20475/4096*AP.Sβ-149391/8192*AP.S3β+AP.C2*(2187/1024*AP.Sβ+10017/2048*AP.S3β-1701/2048*AP.S5β)+7371/8192*AP.S5β+AP.C4*(-567/4096*AP.Sβ-1701/8192*AP.S3β+8505/8192*AP.S5β))*AP.S1^2+AP.η*AP.C1^4*(-3195/2048*AP.Sβ+45711/4096*AP.S3β+AP.C4*(567/2048*AP.Sβ+1701/4096*AP.S3β-8505/4096*AP.S5β)-7371/4096*Ap.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β))*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^7*AP.S1^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^5*((-37/384*AP.Cβ+1/384*AP.Cβ*AP.C2β)*AP.Sβ^2-(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3+AP.η*AP.C1^5*((37/192*AP.Cβ-1/192*AP.Cβ*AP.C2β)*AP.Sβ^2+(7/192*AP.Cβ+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA18(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((AP.η*(1/64+1/192*AP.C2β)-(1/128+1/384*AP.C2β))*AP.C1^6*AP.Sβ^3*AP.S1^4)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA19(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^2*(-245/2048*AP.Sβ+57/4096*AP.S3β-AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)-13/4096*AP.S5β)*AP.S1^4+AP.C1^2*(-1675/4096*AP.Sβ-825/8192*AP.S3β+AP.C2*(27/1024*AP.Sβ-151/2048*AP.S3β+3/2048*AP.S5β)+AP.C4/4096*(7*AP.Sβ+13*AP.S3β+15*AP.S5β)*AP.S1^4))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return -4*π*AP.C1^3*AP.S2β*AP.S1
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA20(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(4375*AP.η*AP.C1^6*(1/768*AP.Sβ+1/512*AP.S3β-5/512*AP.S5β)*AP.S1^4+4375*AP.C1^6*(-1/1536*AP.Sβ-1/1024*AP.S3β+5/1024*AP.S5β)*AP.S1^4)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA21(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^2*(-20475/4096*AP.Sβ+149391/8192*AP.S3β+AP.C4/4096*(567*AP.Sβ+1701/2*AP.S3β-8505/2*AP.S5β)+AP.C2/2048*(4374*AP.Sβ+10017*AP.S3β-1701*AP.S5β)-7371/8192*AP.S5β)*AP.S1^4+AP.η*AP.C1^2*(3195/2048*AP.Sβ-45711/4096*AP.S3β+7371/4096*AP.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β)+AP.C4*(-567/2048*AP.Sβ-1701/4096*AP.S3β+8505/4096*AP.S5β))*AP.S1^4)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA22(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^3*((37/192*AP.Cβ-AP.Cβ*AP.C2β/192)*AP.Sβ^2-(7/192+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5+AP.C1^3*((-37/384*AP.Cβ+AP.Cβ*AP.C2β/384)*AP.Sβ^2+(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA23(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((1/128+1/384*AP.C2β-AP.η*(1/64+1/192*AP.C2β))*AP.C1^4*AP.Sβ^3*AP.S1^6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA24(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*((14067/4096+4689/1024*AP.C2β-5751/4096*AP.C4β)*AP.Sβ+(-297/1024+1053/256*AP.C2β-2187/1024*AP.C4β)*AP.C2*AP.Sβ-(5103/4096+1701/1024*AP.C2β+3645/4096*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6+((-55539/8192-8145/2048*AP.C2β+5751/8192*AP.C4β)*AP.Sβ+(297/2048-1053/512*AP.C2β+2187/2048*AP.C4β)*AP.C2*AP.Sβ+(5103/8192+1701/2048*AP.C2β+3645/8192*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA25(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^4*(4375/1536*AP.Sβ+4375/1024*AP.S3β-21875/1024*AP.S5β)*AP.S1^6+AP.η*AP.C1^4*(-4375/768*AP.Sβ-4375/512*AP.S3β+21875/512*AP.S5β)*AP.S1^6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA26(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^3*AP.S1^7)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA27(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7+AP.C1*((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA28(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((243/256-81/256*AP.C2β-AP.η*(243/128+81/128*AP.C2β))*AP.C1^2*AP.Sβ^3*AP.S1^8)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA29(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β-AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β))*AP.C1^2*AP.S1^8)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return 4*π*AP.C1*AP.S2β*AP.S1^3
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA30(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1*AP.Sβ^2*AP.S1^9)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA31(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((625/256+625/768*AP.C2β-AP.η*(625/128+625/384*AP.C2β))*AP.Sβ^3*AP.S1^10)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA32(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*((10197/2048*AP.Cβ-3969/2048*AP.Cβ*AP.C2β)*AP.Sβ^2-(1701/2048*AP.Cβ+5103/2048*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3+((-44757/4096*AP.Cβ+3969/4096*AP.Cβ*AP.C2β)*AP.Sβ^2+(1701/4096*AP.Cβ+5103/4096*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA33(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((21875/4096*AP.Cβ+13125/4096*AP.C3β-AP.η*(21875/2048*AP.Cβ+13125/2048*AP.C3β))*AP.Sβ^2*AP.S2^5)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA34(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-37071/16384*AP.Cβ*AP.C2β+AP.Cβ*(-7641/8192+567/32768*AP.C4β)-(10917/8192*AP.Cβ+2835/1024*AP.Cβ*AP.C2β)*AP.C2+(-10089/16384*AP.Cβ+135/8192*AP.Cβ*AP.C2β)*AP.C4+513/8192*AP.Cβ*AP.C6+5167/32768*AP.Cβ*AP.C8)*AP.S2-81/8192*AP.Cβ*AP.C4β*AP.S4+1053/65536*AP.Cβ*AP.C4β*AP.S6+(2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10+AP.η*((5967/8192*AP.Cβ*AP.C2β+AP.Cβ*(2457/4096-567/16384*AP.C4β)+(4005/4096*AP.Cβ+243/512*AP.Cβ*AP.C2β)*AP.C2+(6633/8192*AP.Cβ-5319/4096*AP.Cβ*AP.C2β)*AP.C4-513/4096*AP.Cβ*AP.C6-567/16384*AP.Cβ*AP.C8)*AP.S2+81/4096*AP.Cβ*AP.C4β*AP.S4-1053/32768*AP.Cβ*AP.C4β*AP.S6-(2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA35(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-18603/8192*AP.Cβ*AP.C2β+AP.Cβ*(-20475/32768+567/32768*AP.C4β))*AP.S2+(2835/2048*AP.Cβ*AP.C2β+AP.Cβ*(5715/8192+81/8192*AP.C4β))*AP.S4+(135/16384*AP.Cβ*AP.C2β+AP.Cβ*(-20745/65536+1053/65536*AP.C4β))*AP.S6-(513/16384*AP.Cβ+2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(567/65536*AP.Cβ+243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10+AP.η*((5643/4096*AP.Cβ*AP.C2β+AP.Cβ*(3195/16384-567/16384*AP.C4β))*AP.S6+(513/8192*AP.Cβ+2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(567/32768*AP.Cβ+243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA36(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((319/24576*AP.Cβ*AP.C2β+AP.Cβ*(871/4096+AP.C4β/49152)+(933/4096*AP.Cβ+133/1536*AP.Cβ*AP.C2β)*AP.C2+(625/24576*AP.Cβ+211/4096*AP.Cβ*AP.C2β)*AP.C4-11/12288*AP.Cβ*AP.C6-7/49152*AP.Cβ*AP.C8)*AP.S2-AP.Cβ*AP.C4β*AP.S4/12288+AP.Cβ*AP.C4β*AP.S6/32768-(45/16384*AP.C3β+AP.C5β/16384)*AP.S8-(AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((257/12288*AP.Cβ*AP.C2β-AP.Cβ*(1493/6144+AP.C4β/24576)+(-1391/6144+11/768*AP.Cβ*AP.C2β)*AP.C2+(-49/12288*AP.Cβ+77/2048*AP.Cβ*AP.C2β)*AP.C4+11/6144*AP.Cβ*AP.C6+7/24576*AP.Cβ*AP.C8)*AP.S2+AP.Cβ*AP.C4β*AP.S4/6144-AP.Cβ*AP.C4β*AP.S6/16384+(45/8192*AP.C3β+AP.C5β/8192)*AP.S8+(AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA37(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-157/12288*AP.Cβ*AP.C2β+AP.Cβ*(9287/49152+AP.C4β/49152))*AP.S2+(-133/3072*AP.Cβ*AP.C2β+AP.Cβ*(-1405/12288+AP.C4β/12288))*AP.S4+(211/8192*AP.Cβ*AP.C2β+AP.Cβ*(419/32768+AP.C4β/32768))*AP.S6+(11/24576*AP.Cβ+45/16384*AP.C3β+AP.C5β/16384)*AP.S8-(7/98304*AP.Cβ+AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((13/6144*AP.Cβ*AP.C2β-AP.Cβ*(5923/24576+AP.C4β/24576))*AP.S2+(-11/1536*AP.Cβ*AP.C2β+AP.Cβ*(701/6144-AP.C4β/6144))*AP.S4+(77/4096*AP.Cβ*AP.C2β-AP.Cβ*(35/16384+AP.C4β/16384))*AP.S6-(11/12288*AP.Cβ+45/8192*AP.C3β+AP.C5β/8192)*AP.S8+(7/49152*AP.Cβ+AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA38(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((-341/8192*AP.Cβ+AP.Cβ*AP.C2β/8192)*AP.Sβ^2*AP.S2+(-3411/16384*AP.Cβ+7/16384*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6+(35/32768*AP.Cβ+21/32768*AP.C3β)*AP.Sβ^2*AP.S10+AP.η*((-43/4096*AP.Cβ-AP.Cβ*AP.C2β/4096)*AP.Sβ^2*AP.S2+(-429/8192*AP.Cβ+7/8192*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6+(-35/16384*AP.Cβ-21/16384*AP.C3β)*AP.Sβ^2*AP.S10))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return -(3*π+π*AP.C2β)*AP.S1^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  Return -3*π*AP.Sβ^2*AP.S2^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*(625/128+625/384*AP.C2β)-(625/256+625/768*AP.C2β))*AP.C1^10*AP.Sβ^3
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.η*AP.C1^2*(-7449/16384*AP.Sβ-331/32768*AP.S3β+AP.C4*(337/12288*AP.Sβ-47/8192*AP.S3β-21/8192*AP.S5β)+AP.C8*(7/49152*AP.Sβ+7/32768*AP.S3β-35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Cβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C2*(1873/2048*AP.Sβ+19/4096*AP.S3β+35/12288*AP.S5β)-155/98304*AP.S5β)+AP.C1^2*(43723/98304*AP.Sβ-9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ+1901/8192*AP.S3β-35/24576*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)+AP.C8*(-7/98304*AP.Sβ-7/65536*AP.S3β+35/65536*AP.S5β)+AP.C4*(1103/24576*AP.Sβ-2833/16384*AP.S3β+21/16384*AP.S5β)+155/196608*AP.S5β))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.C1^6*(39249/8192*AP.Sβ+38331/16384*AP.S3β-AP.C4*(1701/8192*AP.Sβ+3159/16384*AP.S3β+3645/16384*AP.S5β)+AP.C2*(2403/2048*AP.Sβ-6399/4096*AP.S3β+2187/4096*AP.S5β)-5751/16384*AP.S5β)+AP.η*AP.C1^6*(-4689/4096*AP.Sβ-24507/8192*AP.S3β+AP.C2*(-2403/1024*AP.Sβ+6399/2048*AP.S3β-2187/2048*AP.S5β)+AP.C4*(1701/4096*AP.Sβ+3159/8192*AP.S3β+3645/8192*AP.S5β)+5751/8192*AP.S5β))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1^9*AP.Sβ^2*AP.S1)
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="HAdjusted"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sn2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
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
