#tag Class
Protected Class NoiseClass
	#tag Method, Flags = &h0
		Sub Constructor(dT As Double)
		  ΔT = dT
		  b1 = 2.0654e-43
		  b2 = 4.7315e-61
		  b3 = 1.4125e-47
		  fb1 = 7.08e-4
		  fb2 = 1.778e-3
		  fc2 = 1.0e-3
		  sa = 2.31e-52
		  sx = 1.257e-41
		  
		  // New parameters
		  
		  p1 = .171
		  p2 = 292.0
		  p3 = 1020.0
		  p4 = 1680.0
		  fk = .00215
		  
		  POMS = (1.5e-11)*(1.5e-11)
		  PACCBASE = (3e-15)*(3e-15)
		  A = 9e-45
		  L = 2.5e9
		  
		  e = 2.7182818284
		  pi = 3.14159265
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoise(fn As Double) As Double
		  //  Class subroutine to generate the noise in LISA at each frequency
		  //  This dates from a very early version of the code.
		  
		  Var stot, sb as Double
		  stot = sqrt(sx*sx + (sa/(fn*fn*fn*fn))*(sa/(fn*fn*fn*fn)))
		  stot = stot*(1+(fn*fn)/fc2)
		  sb = b1/(fn^(1.9))
		  If fn >= fb1 Then sb = b2/(fn^(7.5))
		  If fn >= fb2 Then sb = b3/(fn^(2.6))
		  stot = sqrt(stot*stot + 4*sb*sb)
		  
		  //  This correction specifies the noise value that matches
		  //  the Benacquista data at f = 0.002 Hz.  Note that this is
		  //  ONLY valid at that frequency.
		  
		  Return stot/(2*ΔT) // return the calculated noise
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoiseNew(fn As Double) As Double
		  Var Sc, Sn, PACC As Double
		  
		  Var tanhArg As Double
		  
		  TanhArg = p4*(fk-fn)
		  
		  If fn < .015 then 
		     Sc = A*(fn)^(-7/3) * e^(-fn*p1 + p2*fn*sin(p3*fn))*(1 + (e^TanhArg - e^-TanhArg)/(e^TanhArg + e^-TanhArg))
		  else
		    Sc = 0 
		  end if 
		  
		  PACC = PACCBASE*(1.0+(.0004/fn)*(.0004/fn))
		  
		  Sn = (10.0/(3.0*L*L))*(POMS + (4.0*PACC)/((2.0*pi*fn)*(2.0*pi*fn)*(2.0*pi*fn)*(2.0*pi*fn)))*(1.0+.6*(fn/.0191)*(fn/.0191))+Sc
		  
		  Return Sn/(2*ΔT)
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		A As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		b1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		b2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		b3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		e As double
	#tag EndProperty

	#tag Property, Flags = &h0
		fb1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fb2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fc2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fk As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		L As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		p1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		p2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		p3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		p4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PACCBASE As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		pi As double
	#tag EndProperty

	#tag Property, Flags = &h0
		POMS As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sa As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΔT As Double
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
			Name="ΔT"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fb1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fb2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fc2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sa"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sx"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
