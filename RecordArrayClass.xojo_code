#tag Class
Protected Class RecordArrayClass
	#tag Method, Flags = &h0
		Sub AddRecord(DR As DataRecord)
		  Records.Add(DR)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // The Names array must parallel the enumeration list. That list must end with "NItems."
		  Names.ResizeTo(Integer(Item.NItems))
		  Names(0) = "DhDh0"
		  Names(1) = "DhDδ"
		  Names(2) = "DhDv0"
		  Names(3) = "DhDz"
		  Names(4) = "DhDβ"
		  Names(5) = "DhDψ"
		  Names(6) = "DhDλ0"
		  Names(7) = "DhDΘ"
		  Names(8) = "DhDΦ"
		  Names(9) = "DhDχ10x"
		  Names(10) = "DhDχ10y"
		  Names(11) = "DhDχ10z"
		  Names(12) = "DhDχ20x"
		  Names(13) = "DhDχ20y"
		  Names(14) = "DhDχ20z"
		  Names(15) = "h"
		  Names(16) = "hp"
		  Names(17) = "hx"
		  Names(18) = "v"
		  Names(19) = "Ψr"
		  Names(20) = "α"
		  Names(21) = "𝓁x"
		  Names(22) = "𝓁y"
		  Names(23) = "𝓁z"
		  Names(24) = "χ1Hatx"
		  Names(25) = "χ1Haty"
		  Names(26) = "χ1Hatz"
		  Names(27) = "χ2Hatx"
		  Names(28) = "χ2Haty"
		  Names(29) = "χ2Hatz"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNewRecord() As DataRecord
		  Return New DataRecord(Integer(Item.NItems))
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Names() As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Records() As DataRecord
	#tag EndProperty


	#tag Enum, Name = Item, Type = Integer, Flags = &h0
		DhDh0
		  DhDδ
		  DhDv0
		  DhDz
		  DhDβ
		  DhDψ
		  DhDλ0
		  DhDΘ
		  DhDΦ
		  DhDχ10x
		  DhDχ10y
		  DhDχ10z
		  DhDχ20x
		  DhDχ20y
		  DhDχ20z
		  h
		  hp
		  hx
		  v
		  Ψr
		  α
		  Lx
		  Ly
		  Lz
		  χ1Hatx
		  χ1Haty
		  χ1Hatz
		  χ2Hatx
		  χ2Haty
		  χ2Hatz
		NItems
	#tag EndEnum


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
End Class
#tag EndClass
