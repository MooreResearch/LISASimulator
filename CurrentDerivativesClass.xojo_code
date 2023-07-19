#tag Class
Protected Class CurrentDerivativesClass
	#tag Method, Flags = &h0
		Sub Set2Interpolation(DN As CurrentDerivativesClass, DP As CurrentDerivativesClass, FracFromP As Double)
		  // We use this method to get the derivatives at the main step when we are
		  // interpolating between longer source steps. The final parameter specifies
		  // the fraction of the whole distance between the past and present source steps
		  // that the main step represents.
		  Var FracFromN As Double = 1.0 - FracFromP
		  
		  // Interpolate Dv derivatives
		  DvDV0 = DN.DvDV0*FracFromN + DP.DvDV0*FracFromP
		  DvDZ = DN.DvDZ*FracFromN + DP.DvDZ*FracFromP
		  DvDδ = DN.DvDδ*FracFromN + DP.DvDδ*FracFromP
		  DvDχ10x = DN.DvDχ10x*FracFromN + DP.DvDχ10x*FracFromP
		  DvDχ10y = DN.DvDχ10y*FracFromN + DP.DvDχ10y*FracFromP
		  DvDχ10z = DN.DvDχ10z*FracFromN + DP.DvDχ10z*FracFromP
		  DvDχ20x = DN.DvDχ20x*FracFromN + DP.DvDχ20x*FracFromP
		  DvDχ20y = DN.DvDχ20y*FracFromN + DP.DvDχ20y*FracFromP
		  DvDχ20z = DN.DvDχ20z*FracFromN + DP.DvDχ20z*FracFromP
		  
		  // Interpolate Dι derivatives
		  DιDV0 = DN.DιDV0*FracFromN + DP.DιDV0*FracFromP
		  DιDZ = DN.DιDZ*FracFromN + DP.DιDZ*FracFromP
		  DιDδ = DN.DιDδ*FracFromN + DP.DιDδ*FracFromP
		  DιDχ10x = DN.DιDχ10x*FracFromN + DP.DιDχ10x*FracFromP
		  DιDχ10y = DN.DιDχ10y*FracFromN + DP.DιDχ10y*FracFromP
		  DιDχ10z = DN.DιDχ10z*FracFromN + DP.DιDχ10z*FracFromP
		  DιDχ20x = DN.DιDχ20x*FracFromN + DP.DιDχ20x*FracFromP
		  DιDχ20y = DN.DιDχ20y*FracFromN + DP.DιDχ20y*FracFromP
		  DιDχ20z = DN.DιDχ20z*FracFromN + DP.DιDχ20z*FracFromP
		  
		  // Interpolate Dα derivatives
		  DαDV0 = DN.DαDV0*FracFromN + DP.DαDV0*FracFromP
		  DαDZ = DN.DαDZ*FracFromN + DP.DαDZ*FracFromP
		  DαDδ = DN.DαDδ*FracFromN + DP.DαDδ*FracFromP
		  DαDχ10x = DN.DαDχ10x*FracFromN + DP.DαDχ10x*FracFromP
		  DαDχ10y = DN.DαDχ10y*FracFromN + DP.DαDχ10y*FracFromP
		  DαDχ10z = DN.DαDχ10z*FracFromN + DP.DαDχ10z*FracFromP
		  DαDχ20x = DN.DαDχ20x*FracFromN + DP.DαDχ20x*FracFromP
		  DαDχ20y = DN.DαDχ20y*FracFromN + DP.DαDχ20y*FracFromP
		  DαDχ20z = DN.DαDχ20z*FracFromN + DP.DαDχ20z*FracFromP
		  
		  // Interpolate Dχax derivatives
		  DχaxDV0 = DN.DχaxDV0*FracFromN + DP.DvDV0*FracFromP
		  DχaxDZ = DN.DχaxDZ*FracFromN + DP.DχaxDZ*FracFromP
		  DχaxDδ = DN.DχaxDδ*FracFromN + DP.DχaxDδ*FracFromP
		  DχaxDχ10x = DN.DχaxDχ10x*FracFromN + DP.DχaxDχ10x*FracFromP
		  DχaxDχ10y = DN.DχaxDχ10y*FracFromN + DP.DχaxDχ10y*FracFromP
		  DχaxDχ10z = DN.DχaxDχ10z*FracFromN + DP.DχaxDχ10z*FracFromP
		  DχaxDχ20x = DN.DχaxDχ20x*FracFromN + DP.DχaxDχ20x*FracFromP
		  DχaxDχ20y = DN.DχaxDχ20y*FracFromN + DP.DχaxDχ20y*FracFromP
		  DχaxDχ20z = DN.DχaxDχ20z*FracFromN + DP.DχaxDχ20z*FracFromP
		  
		  // Interpolate Dχay derivatives
		  DχayDV0 = DN.DχayDV0*FracFromN + DP.DχayDV0*FracFromP
		  DχayDZ = DN.DχayDZ*FracFromN + DP.DχayDZ*FracFromP
		  DχayDδ = DN.DχayDδ*FracFromN + DP.DχayDδ*FracFromP
		  DχayDχ10x = DN.DχayDχ10x*FracFromN + DP.DχayDχ10x*FracFromP
		  DχayDχ10y = DN.DχayDχ10y*FracFromN + DP.DχayDχ10y*FracFromP
		  DχayDχ10z = DN.DχayDχ10z*FracFromN + DP.DχayDχ10z*FracFromP
		  DχayDχ20x = DN.DχayDχ20x*FracFromN + DP.DχayDχ20x*FracFromP
		  DχayDχ20y = DN.DχayDχ20y*FracFromN + DP.DχayDχ20y*FracFromP
		  DχayDχ20z = DN.DχayDχ20z*FracFromN + DP.DχayDχ20z*FracFromP
		  
		  // Interpolate Dχaz derivatives
		  DχazDV0 = DN.DχazDV0*FracFromN + DP.DχazDV0*FracFromP
		  DχazDZ = DN.DχazDZ*FracFromN + DP.DχazDZ*FracFromP
		  DχazDδ = DN.DχazDδ*FracFromN + DP.DχazDδ*FracFromP
		  DχazDχ10x = DN.DχazDχ10x*FracFromN + DP.DχazDχ10x*FracFromP
		  DχazDχ10y = DN.DχazDχ10y*FracFromN + DP.DχazDχ10y*FracFromP
		  DχazDχ10z = DN.DχazDχ10z*FracFromN + DP.DχazDχ10z*FracFromP
		  DχazDχ20x = DN.DχazDχ20x*FracFromN + DP.DχazDχ20x*FracFromP
		  DχazDχ20y = DN.DχazDχ20y*FracFromN + DP.DχazDχ20y*FracFromP
		  DχazDχ20z = DN.DχazDχ20z*FracFromN + DP.DχazDχ20z*FracFromP
		  
		  // Interpolate Dχsx derivatives
		  DχsxDV0 = DN.DχsxDV0*FracFromN + DP.DvDV0*FracFromP
		  DχsxDZ = DN.DχsxDZ*FracFromN + DP.DχsxDZ*FracFromP
		  DχsxDδ = DN.DχsxDδ*FracFromN + DP.DχsxDδ*FracFromP
		  DχsxDχ10x = DN.DχsxDχ10x*FracFromN + DP.DχsxDχ10x*FracFromP
		  DχsxDχ10y = DN.DχsxDχ10y*FracFromN + DP.DχsxDχ10y*FracFromP
		  DχsxDχ10z = DN.DχsxDχ10z*FracFromN + DP.DχsxDχ10z*FracFromP
		  DχsxDχ20x = DN.DχsxDχ20x*FracFromN + DP.DχsxDχ20x*FracFromP
		  DχsxDχ20y = DN.DχsxDχ20y*FracFromN + DP.DχsxDχ20y*FracFromP
		  DχsxDχ20z = DN.DχsxDχ20z*FracFromN + DP.DχsxDχ20z*FracFromP
		  
		  // Interpolate Dχsy derivatives
		  DχsyDV0 = DN.DχsyDV0*FracFromN + DP.DχsyDV0*FracFromP
		  DχsyDZ = DN.DχsyDZ*FracFromN + DP.DχsyDZ*FracFromP
		  DχsyDδ = DN.DχsyDδ*FracFromN + DP.DχsyDδ*FracFromP
		  DχsyDχ10x = DN.DχsyDχ10x*FracFromN + DP.DχsyDχ10x*FracFromP
		  DχsyDχ10y = DN.DχsyDχ10y*FracFromN + DP.DχsyDχ10y*FracFromP
		  DχsyDχ10z = DN.DχsyDχ10z*FracFromN + DP.DχsyDχ10z*FracFromP
		  DχsyDχ20x = DN.DχsyDχ20x*FracFromN + DP.DχsyDχ20x*FracFromP
		  DχsyDχ20y = DN.DχsyDχ20y*FracFromN + DP.DχsyDχ20y*FracFromP
		  DχsyDχ20z = DN.DχsyDχ20z*FracFromN + DP.DχsyDχ20z*FracFromP
		  
		  // Interpolate Dχsz derivatives
		  DχszDV0 = DN.DχszDV0*FracFromN + DP.DχszDV0*FracFromP
		  DχszDZ = DN.DχszDZ*FracFromN + DP.DχszDZ*FracFromP
		  DχszDδ = DN.DχszDδ*FracFromN + DP.DχszDδ*FracFromP
		  DχszDχ10x = DN.DχszDχ10x*FracFromN + DP.DχszDχ10x*FracFromP
		  DχszDχ10y = DN.DχszDχ10y*FracFromN + DP.DχszDχ10y*FracFromP
		  DχszDχ10z = DN.DχszDχ10z*FracFromN + DP.DχszDχ10z*FracFromP
		  DχszDχ20x = DN.DχszDχ20x*FracFromN + DP.DχszDχ20x*FracFromP
		  DχszDχ20y = DN.DχszDχ20y*FracFromN + DP.DχszDχ20y*FracFromP
		  DχszDχ20z = DN.DχszDχ20z*FracFromN + DP.DχszDχ20z*FracFromP
		  
		  // Interpolate Dψr derivatives
		  DΨrDV0 = DN.DΨrDV0*FracFromN + DP.DΨrDV0*FracFromP
		  DΨrDZ = DN.DΨrDZ*FracFromN + DP.DΨrDZ*FracFromP
		  DΨrDδ = DN.DΨrDδ*FracFromN + DP.DΨrDδ*FracFromP
		  DΨrDχ10x = DN.DΨrDχ10x*FracFromN + DP.DΨrDχ10x*FracFromP
		  DΨrDχ10y = DN.DΨrDχ10y*FracFromN + DP.DΨrDχ10y*FracFromP
		  DΨrDχ10z = DN.DΨrDχ10z*FracFromN + DP.DΨrDχ10z*FracFromP
		  DΨrDχ20x = DN.DΨrDχ20x*FracFromN + DP.DΨrDχ20x*FracFromP
		  DΨrDχ20y = DN.DΨrDχ20y*FracFromN + DP.DΨrDχ20y*FracFromP
		  DΨrDχ20z = DN.DΨrDχ20z*FracFromN + DP.DΨrDχ20z*FracFromP
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetAsCopyOf(DN As CurrentDerivativesClass)
		  // We use this method to make the past reference a copy of the current reference
		  
		  // Copy Dv derivatives
		  DvDV0 = DN.DvDV0
		  DvDZ = DN.DvDZ
		  DvDδ = DN.DvDδ
		  DvDχ10x = DN.DvDχ10x
		  DvDχ10y = DN.DvDχ10y
		  DvDχ10z = DN.DvDχ10z
		  DvDχ20x = DN.DvDχ20x
		  DvDχ20y = DN.DvDχ20y
		  DvDχ20z = DN.DvDχ20z
		  
		  // Copy Dι derivatives
		  DιDV0 = DN.DιDV0
		  DιDZ = DN.DιDZ
		  DιDδ = DN.DιDδ
		  DιDχ10x = DN.DιDχ10x
		  DιDχ10y = DN.DιDχ10y
		  DιDχ10z = DN.DιDχ10z
		  DιDχ20x = DN.DιDχ20x
		  DιDχ20y = DN.DιDχ20y
		  DιDχ20z = DN.DιDχ20z
		  
		  // Copy Dα derivatives
		  DαDV0 = DN.DαDV0
		  DαDZ = DN.DαDZ
		  DαDδ = DN.DαDδ
		  DαDχ10x = DN.DαDχ10x
		  DαDχ10y = DN.DαDχ10y
		  DαDχ10z = DN.DαDχ10z
		  DαDχ20x = DN.DαDχ20x
		  DαDχ20y = DN.DαDχ20y
		  DαDχ20z = DN.DαDχ20z
		  
		  // Copy Dχax derivatives
		  DχaxDV0 = DN.DχaxDV0
		  DχaxDZ = DN.DχaxDZ
		  DχaxDδ = DN.DχaxDδ
		  DχaxDχ10x = DN.DχaxDχ10x
		  DχaxDχ10y = DN.DχaxDχ10y
		  DχaxDχ10z = DN.DχaxDχ10z
		  DχaxDχ20x = DN.DχaxDχ20x
		  DχaxDχ20y = DN.DχaxDχ20y
		  DχaxDχ20z = DN.DχaxDχ20z
		  
		  // Copy Dχay derivatives
		  DχayDV0 = DN.DχayDV0
		  DχayDZ = DN.DχayDZ
		  DχayDδ = DN.DχayDδ
		  DχayDχ10x = DN.DχayDχ10x
		  DχayDχ10y = DN.DχayDχ10y
		  DχayDχ10z = DN.DχayDχ10z
		  DχayDχ20x = DN.DχayDχ20x
		  DχayDχ20y = DN.DχayDχ20y
		  DχayDχ20z = DN.DχayDχ20z
		  
		  // Copy Dχaz derivatives
		  DχazDV0 = DN.DχazDV0
		  DχazDZ = DN.DχazDZ
		  DχazDδ = DN.DχazDδ
		  DχazDχ10x = DN.DχazDχ10x
		  DχazDχ10y = DN.DχazDχ10y
		  DχazDχ10z = DN.DχazDχ10z
		  DχazDχ20x = DN.DχazDχ20x
		  DχazDχ20y = DN.DχazDχ20y
		  DχazDχ20z = DN.DχazDχ20z
		  
		  // Copy Dχsx derivatives
		  DχsxDV0 = DN.DχsxDV0
		  DχsxDZ = DN.DχsxDZ
		  DχsxDδ = DN.DχsxDδ
		  DχsxDχ10x = DN.DχsxDχ10x
		  DχsxDχ10y = DN.DχsxDχ10y
		  DχsxDχ10z = DN.DχsxDχ10z
		  DχsxDχ20x = DN.DχsxDχ20x
		  DχsxDχ20y = DN.DχsxDχ20y
		  DχsxDχ20z = DN.DχsxDχ20z
		  
		  // Copy Dχsy derivatives
		  DχsyDV0 = DN.DχsyDV0
		  DχsyDZ = DN.DχsyDZ
		  DχsyDδ = DN.DχsyDδ
		  DχsyDχ10x = DN.DχsyDχ10x
		  DχsyDχ10y = DN.DχsyDχ10y
		  DχsyDχ10z = DN.DχsyDχ10z
		  DχsyDχ20x = DN.DχsyDχ20x
		  DχsyDχ20y = DN.DχsyDχ20y
		  DχsyDχ20z = DN.DχsyDχ20z
		  
		  // Copy Dχsz derivatives
		  DχszDV0 = DN.DχszDV0
		  DχszDZ = DN.DχszDZ
		  DχszDδ = DN.DχszDδ
		  DχszDχ10x = DN.DχszDχ10x
		  DχszDχ10y = DN.DχszDχ10y
		  DχszDχ10z = DN.DχszDχ10z
		  DχszDχ20x = DN.DχszDχ20x
		  DχszDχ20y = DN.DχszDχ20y
		  DχszDχ20z = DN.DχszDχ20z
		  
		  // ICopy Dψr derivatives
		  DΨrDV0 = DN.DΨrDV0
		  DΨrDZ = DN.DΨrDZ
		  DΨrDδ = DN.DΨrDδ
		  DΨrDχ10x = DN.DΨrDχ10x
		  DΨrDχ10y = DN.DΨrDχ10y
		  DΨrDχ10z = DN.DΨrDχ10z
		  DΨrDχ20x = DN.DΨrDχ20x
		  DΨrDχ20y = DN.DΨrDχ20y
		  DΨrDχ20z = DN.DΨrDχ20z
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		DvDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DιDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20z As Double
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
			Name="DΨrDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
