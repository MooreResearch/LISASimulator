#tag DesktopWindow
Begin DesktopWindow MainWindow
   Backdrop        =   0
   BackgroundColor =   &cFFFFFF
   Composite       =   False
   DefaultLocation =   2
   FullScreen      =   False
   HasBackgroundColor=   False
   HasCloseButton  =   True
   HasFullScreenButton=   False
   HasMaximizeButton=   True
   HasMinimizeButton=   True
   Height          =   800
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   1095792639
   MenuBarVisible  =   False
   MinimumHeight   =   650
   MinimumWidth    =   1000
   Resizeable      =   True
   Title           =   "LISA Simulator"
   Type            =   0
   Visible         =   True
   Width           =   1000
   Begin Timer InterfaceUpdateTimer
      Enabled         =   True
      Index           =   -2147483648
      LockedInPosition=   False
      Period          =   500
      RunMode         =   0
      Scope           =   0
      TabPanelIndex   =   0
   End
   Begin DesktopTabPanel MainTabPanel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   800
      Index           =   -2147483648
      Italic          =   False
      Left            =   0
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      Panels          =   ""
      Scope           =   0
      SmallTabs       =   False
      TabDefinition   =   "Run\rAnalyze\rGraph"
      TabIndex        =   30
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   0
      Transparent     =   False
      Underline       =   False
      Value           =   0
      Visible         =   True
      Width           =   1000
      Begin DesktopListBox ParamNameListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   False
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "80"
         DefaultRowHeight=   26
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   3
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   False
         HasVerticalScrollbar=   False
         HeadingIndex    =   -1
         Height          =   596
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nM (sols)\nδ\nTorb (s)\nTc (y)\nR (ly)\nβ (°)	\nψ (°)\nλ0\nΘ (°)\nΦ(°)	\nΩ\nχ10x	\nχ10y	\nχ10z	\nχ20x\nχ20y\nx20z\nρ0\nPN Order\nDetectors\nΔT (s)\nDuration (y)"
         Italic          =   False
         Left            =   31
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel CaptionForGraphChoiceLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   0
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Graph of:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   47
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   64
      End
      Begin DesktopPopupMenu GraphChoicePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   26
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "H\nHP\nHX\nV\nι\nα\ndhDM\ndhDψ\ndhDλ0\ndhDΘ\ndhDΦ\ndhDβ\ndhDR\ndhDV0\ndhDδ\ndhDχ10x\ndhDχ10y\ndhDχ10z\ndhDχ20x\ndhDχ20y\ndhDχ20z\nαDotN"
         Italic          =   False
         Left            =   96
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   1
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   42
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   151
      End
      Begin DesktopLabel CaptionForStartTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   287
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   9
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Starting Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   47
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   104
      End
      Begin DesktopTextField StartTimeTextField
         AllowAutoDeactivate=   True
         AllowFocusRing  =   True
         AllowSpellChecking=   False
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Format          =   ""
         HasBorder       =   True
         Height          =   26
         Hint            =   ""
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   397
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumCharactersAllowed=   0
         Password        =   False
         ReadOnly        =   False
         Scope           =   0
         TabIndex        =   10
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "0"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   46
         Transparent     =   False
         Underline       =   False
         ValidationMask  =   ""
         Visible         =   True
         Width           =   110
      End
      Begin DesktopLabel CaptionForEndTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   287
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   11
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Ending Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   79
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   104
      End
      Begin DesktopTextField EndTimeTextField
         AllowAutoDeactivate=   True
         AllowFocusRing  =   True
         AllowSpellChecking=   False
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Format          =   ""
         HasBorder       =   True
         Height          =   26
         Hint            =   ""
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   398
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumCharactersAllowed=   0
         Password        =   False
         ReadOnly        =   False
         Scope           =   0
         TabIndex        =   12
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Max"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   79
         Transparent     =   False
         Underline       =   False
         ValidationMask  =   ""
         Visible         =   True
         Width           =   110
      End
      Begin DesktopListBox CasesListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   True
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "80"
         DefaultRowHeight=   26
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   3
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   False
         HasVerticalScrollbar=   False
         HeadingIndex    =   -1
         Height          =   596
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Case 1\n10000\n0.1\n500\n\n1.0e7\n39\n24\n0\n5\n268.5\n---\nx 0\nx 0\nx 0\nx 0\nx 0\nx 0\n0\n0\n2\n50\n1.0"
         Italic          =   False
         Left            =   115
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   7
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   82
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel ValueOfAnalyzeCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   129
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   22
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   48
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
      End
      Begin DesktopListBox MatrixListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   16
         ColumnWidths    =   "24, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80"
         DefaultRowHeight=   21
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   3
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   True
         HasVerticalScrollbar=   True
         HeadingIndex    =   -1
         Height          =   357
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "\n1	+0.00e+00\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   26
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   80
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   960
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopPopupMenu MatrixChoicePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   24
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "ATA\nY Original\nY Inverted\nYNormalized Original\nYNormalized Inverted\nY^-1 x Y\nY0 Normalized Unchanged\n"
         Italic          =   False
         Left            =   144
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   29
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   37
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   207
      End
      Begin DesktopLabel CaptionForMatrixLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   30
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Matrix To Display:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   136
      End
      Begin DesktopLabel XExplanationLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   39
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   31
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   True
         Scope           =   0
         Selectable      =   False
         TabIndex        =   8
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Typing 'x' in a cell toggles whether that variable's uncertainty is calculated ('x' = 'not calculated')."
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   639
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   327
      End
      Begin DesktopButton CopyMatrixButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Copy Matrix"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   372
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   33
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   97
      End
      Begin DesktopLabel CaptionForConditionLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   703
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   34
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Condition Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   123
      End
      Begin DesktopLabel ValueOfConditionLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   829
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   35
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   151
      End
      Begin DesktopLabel CaptionForStatusLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   9
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Status:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   49
      End
      Begin DesktopLabel CaptionForRunTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   12
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Computation Time (s):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   134
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfRunTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   13
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   134
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
      End
      Begin DesktopLabel CaptionForStepNumLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   14
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Current Step Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   164
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfStepNumberLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   15
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   164
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForSimTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   16
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Simulation Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   194
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfSimTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   17
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   194
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForTcLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   18
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Time to Coalescence (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   232
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfTcLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   19
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   232
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForVLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   20
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Current PN Factor v:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   262
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfVLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   21
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   262
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForStepRatioLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   22
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Current Step Ratio:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   292
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfStepRatioLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   23
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   292
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForSNRLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   24
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Current Signal-to-Noise:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   322
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfSNRLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   569
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   25
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   322
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel ValueOfStatusLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   465
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   26
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Not Started"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   164
      End
      Begin DesktopLabel CaptionForStopReasonLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   202
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   27
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Reason For Stopping:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   501
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin ProgressBar CaseProgressBar
         AllowAutoDeactivate=   True
         Enabled         =   True
         Height          =   14
         Indeterminate   =   False
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumValue    =   100
         Scope           =   0
         TabIndex        =   28
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   94
         Transparent     =   True
         Value           =   0.0
         Visible         =   True
         Width           =   259
      End
      Begin DesktopButton StartStopButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Run"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   706
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   29
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   96
      End
      Begin DesktopListBox UncertaintyListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   False
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "160"
         DefaultRowHeight=   26
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   1
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   False
         HasVerticalScrollbar=   False
         HeadingIndex    =   -1
         Height          =   465
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Uncertainty\n?\n?\n---\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?"
         Italic          =   False
         Left            =   202
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   30
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   160
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel ValueOfStopReasonLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   47
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   202
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   True
         Scope           =   0
         Selectable      =   False
         TabIndex        =   31
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   509
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   160
      End
      Begin DesktopCheckBox RunFileCheckBox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Run Case File"
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   860
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   32
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   120
      End
      Begin DesktopLabel CaptionForProgressLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   401
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   33
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Case Progress:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   70
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   99
      End
      Begin DesktopLabel CaptionForFProgLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   706
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   34
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "File Progress:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   70
         Transparent     =   False
         Underline       =   False
         Visible         =   False
         Width           =   99
      End
      Begin ProgressBar FileProgressBar
         AllowAutoDeactivate=   True
         Enabled         =   True
         Height          =   14
         Indeterminate   =   False
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   706
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumValue    =   100
         Scope           =   0
         TabIndex        =   35
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   94
         Transparent     =   True
         Value           =   0.0
         Visible         =   False
         Width           =   259
      End
      Begin DesktopLabel CaptionForActualTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   204
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   36
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Actual Duration (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   557
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfActualTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   204
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   37
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   582
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopSlider StartSlider
         AllowAutoDeactivate=   True
         AllowLiveScrolling=   True
         Enabled         =   True
         Height          =   30
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   589
         LineStep        =   1
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MaximumValue    =   100
         MinimumValue    =   0
         PageStep        =   20
         Scope           =   0
         TabIndex        =   13
         TabPanelIndex   =   3
         TabStop         =   True
         TickMarkStyle   =   0
         Tooltip         =   ""
         Top             =   47
         Transparent     =   False
         Value           =   0
         Visible         =   True
         Width           =   391
      End
      Begin DesktopSlider DurationSlider
         AllowAutoDeactivate=   True
         AllowLiveScrolling=   True
         Enabled         =   True
         Height          =   30
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   589
         LineStep        =   1
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MaximumValue    =   100
         MinimumValue    =   0
         PageStep        =   20
         Scope           =   0
         TabIndex        =   14
         TabPanelIndex   =   3
         TabStop         =   True
         TickMarkStyle   =   0
         Tooltip         =   ""
         Top             =   79
         Transparent     =   False
         Value           =   100
         Visible         =   True
         Width           =   391
      End
      Begin DesktopLabel CaptionForStartSliderLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   527
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   15
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Start:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   47
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   50
      End
      Begin DesktopLabel CaptionForLengthSliderLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   527
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   16
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Length:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   79
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   50
      End
      Begin DesktopButton GraphButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Graph"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   167
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   17
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   79
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
      End
      Begin Graph GraphingCanvas
         AllowAutoDeactivate=   True
         AllowFocus      =   False
         AllowFocusRing  =   True
         AllowTabs       =   False
         Backdrop        =   0
         CBlack          =   &c00000000
         CGrid           =   &c00000000
         CWhite          =   &c00000000
         DoubleBuffer    =   False
         Enabled         =   True
         FDepth          =   0
         FHeight         =   0
         FWidth          =   0
         GHeight         =   0
         GLeft           =   0
         GTop            =   0
         GWidth          =   0
         GXP             =   0
         HasGrid         =   False
         Height          =   668
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InvLog10        =   0.0
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         NoGraph         =   False
         PrevCurvH       =   0
         PrevCurvV       =   0
         Printing        =   False
         RErr            =   False
         Scope           =   0
         ScPixH          =   0
         ScPixV          =   0
         TabIndex        =   18
         TabPanelIndex   =   3
         TabStop         =   True
         TickFont        =   ""
         TickFSize       =   0
         Title           =   ""
         TitleH          =   0
         TitleV          =   0
         Tooltip         =   ""
         Top             =   111
         Transparent     =   True
         Visible         =   True
         Width           =   960
         XColor          =   &c00000000
         XFlags          =   0
         XLabel          =   ""
         XLabelH         =   0
         XLabelV         =   0
         XMax            =   0.0
         XMin            =   0.0
         XTickN          =   0
         XTPower         =   0
         XVtoPix         =   0.0
         YColor          =   &c00000000
         YFlags          =   0
         YLabel          =   ""
         YLabelH         =   0
         YLabelV         =   0
         YMax            =   0.0
         YMin            =   0.0
         YTickN          =   0
         YTPower         =   0
         YVToPix         =   0.0
      End
   End
   Begin MainThreadClass MainThread
      DebugIdentifier =   ""
      Index           =   -2147483648
      LockedInPosition=   False
      Priority        =   5
      Scope           =   0
      StackSize       =   0
      TabPanelIndex   =   0
      ThreadID        =   0
      ThreadState     =   0
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Event
		Sub Opening()
		  'SetUpMainLB()
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub AddToCasesList(inputString() As String)
		  Var rows,cols as integer
		  
		  rows = CasesList.LastIndex
		  cols = 19 
		  
		  Redim CasesList(rows+1, cols) //Adds a row to CasesList 
		  
		  Var j as integer 
		  
		  for j = 0 to cols 
		    CasesList(rows+1,j) = inputString(j) //Populates the new row with data from InputSting
		  next 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ConvertToDegrees(Value As Double) As Double
		  Var degFromRad As Double = 180.0/3.14159265358979
		  If Value.IsNotANumber Then
		    Return Value
		  Else
		    Return Value*degFromRad
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisplayMatrix(TheMatrix As Matrix)
		  Var n As Integer = TheMatrix.pDim-1
		  For j as Integer = 0 to 14
		    for k as Integer = 0 to 14
		      If j > n or k > n Then
		        MatrixListBox.CellTextAt(j,k+1) = ""
		      Else
		        MatrixListBox.CellTextAt(j,k+1) = Format(TheMatrix.pData(j,k), "+0.00e+00")
		      End If
		    Next
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisplayUncertainties(Params As CaseParametersClass, UV As UncertaintyValuesClass)
		  UncertaintyListBox.CellTextAt(0) = GetUncertaintyString(UV.OfM)
		  UncertaintyListBox.CellTextAt(1) = GetUncertaintyString(UV.Ofδ)
		  UncertaintyListBox.CellTextAt(2) = GetUncertaintyString(UV.OfT0)
		  UncertaintyListBox.CellTextAt(3) = GetUncertaintyString(UV.OfR)
		  UncertaintyListBox.CellTextAt(4) = GetUncertaintyString(UV.Ofβ)
		  UncertaintyListBox.CellTextAt(5) = GetUncertaintyString(UV.Ofψ)
		  UncertaintyListBox.CellTextAt(6) = GetUncertaintyString(UV.Ofλ0)
		  UncertaintyListBox.CellTextAt(7) = GetUncertaintyString(UV.OfΘ)
		  UncertaintyListBox.CellTextAt(8) = GetUncertaintyString(UV.OfΦ)
		  UncertaintyListBox.CellTextAt(9) = GetUncertaintyString(UV.OfΩ)
		  UncertaintyListBox.CellTextAt(10) = GetUncertaintyString(UV.Ofχ10x)
		  UncertaintyListBox.CellTextAt(11) = GetUncertaintyString(UV.Ofχ10y)
		  UncertaintyListBox.CellTextAt(12) = GetUncertaintyString(UV.Ofχ10z)
		  UncertaintyListBox.CellTextAt(13) = GetUncertaintyString(UV.Ofχ20x)
		  UncertaintyListBox.CellTextAt(14) = GetUncertaintyString(UV.Ofχ20y)
		  UncertaintyListBox.CellTextAt(15) = GetUncertaintyString(UV.Ofχ20z)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStart()
		  // Start running a case or cases
		  Var TheCases() As CaseParametersClass
		  Var Flag As Boolean
		  if RunFileCheckBox.Value then
		    
		    AllCasesDone = False 
		    
		    Var TxtType As New FileType
		    TxtType.Name = "Text  file"
		    TxtType.Extensions = ".txt"
		    OutputFile = FolderItem.ShowSaveFileDialog(TxtType, "Create Example.txt")
		    
		    // Case information is loaded into CasesList when box is checked 
		    Var N As Double = CasesList.LastIndex 
		    Var ThisCase As New CaseParametersClass
		    Var i as integer
		    
		    for i = 0 to N 
		      Var AddCase As New CaseParametersClass // Create new parameter list
		      AddCase.M = CasesList(i,0).ToDouble
		      AddCase.δ = CasesList(i,1).ToDouble
		      AddCase.T0 = CasesList(i,2).ToDouble
		      AddCase.R = CasesList(i,3).ToDouble
		      
		      AddCase.β = GetValueAndSolveFlag(Flag, CasesList(i,4))
		      AddCase.SolveForβ = Flag
		      AddCase.ψ = GetValueAndSolveFlag(Flag, CasesList(i,5))
		      AddCase.SolveForψ = Flag
		      AddCase.λ0 = GetValueAndSolveFlag(Flag, CasesList(i,6))
		      AddCase.SolveForλ0 = Flag
		      AddCase.Θ = GetValueAndSolveFlag(Flag, CasesList(i,7))
		      AddCase.SolveForΘ = Flag
		      AddCase.Φ = GetValueAndSolveFlag(Flag, CasesList(i,8))
		      AddCase.SolveForΦ = Flag
		      AddCase.χ10x = GetValueAndSolveFlag(Flag, CasesList(i,9))
		      AddCase.SolveForχ10x = Flag
		      AddCase.χ10y = GetValueAndSolveFlag(Flag, CasesList(i,10))
		      AddCase.SolveForχ10y = Flag
		      AddCase.χ10z = GetValueAndSolveFlag(Flag, CasesList(i,11))
		      AddCase.SolveForχ10z = Flag
		      AddCase.χ20x = GetValueAndSolveFlag(Flag, CasesList(i,12))
		      AddCase.SolveForχ20x = Flag
		      AddCase.χ20y = GetValueAndSolveFlag(Flag, CasesList(i,13))
		      AddCase.SolveForχ20y = Flag
		      AddCase.χ20z = GetValueAndSolveFlag(Flag, CasesList(i,14))
		      AddCase.SolveForχ20z = Flag
		      
		      AddCase.ρ0 = CasesList(i,15).ToDouble
		      AddCase.PNOrder = CasesList(i,16).ToDouble
		      AddCase.Detectors = CasesList(i,17).ToDouble
		      AddCase.ΔT = CasesList(i,18).ToDouble
		      AddCase.RunDuration = CasesList(i,19).ToDouble
		      
		      AddCase.FinishConstruction
		      TheCases.Add(AddCase)
		      
		    next
		    
		    
		    
		    ValueOfStatusLabel.Text = "Running"
		    ValueOfStopReasonLabel.Text = ""
		    ValueOfTcLabel.Text = ""
		    MainThread.LoadCases(TheCases)
		    MainThread.Priority = Thread.HighPriority
		    MainThread.Start
		    InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		    
		  Else
		    Var ThisCase As New CaseParametersClass // Create new parameter list
		    // Set the first four parameters
		    ThisCase.M = CasesListBox.CellTextAt(0).ToDouble
		    ThisCase.δ = CasesListBox.CellTextAt(1).ToDouble
		    ThisCase.T0 = CasesListBox.CellTextAt(2).ToDouble
		    ThisCase.R = CasesListBox.CellTextAt(3).ToDouble
		    // Specially handle cases we might not solve for
		    ThisCase.β = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(4))
		    ThisCase.SolveForβ = Flag
		    ThisCase.ψ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(5))
		    ThisCase.SolveForψ = Flag
		    ThisCase.λ0 = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(6))
		    ThisCase.SolveForλ0 = Flag
		    ThisCase.Θ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(7))
		    ThisCase.SolveForΘ = Flag
		    ThisCase.Φ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(8))
		    ThisCase.SolveForΦ = Flag
		    ThisCase.χ10x = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(10))
		    ThisCase.SolveForχ10x = Flag
		    ThisCase.χ10y = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(11))
		    ThisCase.SolveForχ10y = Flag
		    ThisCase.χ10z = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(12))
		    ThisCase.SolveForχ10z = Flag
		    ThisCase.χ20x = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(13))
		    ThisCase.SolveForχ20x = Flag
		    ThisCase.χ20y = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(14))
		    ThisCase.SolveForχ20y = Flag
		    ThisCase.χ20z = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(15))
		    ThisCase.SolveForχ20z = Flag
		    // Handle the remaining case items
		    ThisCase.ρ0 = CasesListBox.CellTextAt(16).ToDouble
		    ThisCase.PNOrder = CasesListBox.CellTextAt(17).ToDouble
		    ThisCase.Detectors = CasesListBox.CellTextAt(18).ToDouble
		    ThisCase.ΔT = CasesListBox.CellTextAt(19).ToDouble
		    ThisCase.RunDuration = CasesListBox.CellTextAt(20).ToDouble
		    // Finish setting up the case
		    ThisCase.FinishConstruction
		    // Add the case to the list
		    TheCases.Add(ThisCase)
		    ValueOfStatusLabel.Text = "Running"
		    ValueOfStopReasonLabel.Text = ""
		    ValueOfTcLabel.Text = ""
		    MainThread.LoadCases(TheCases)
		    MainThread.Priority = Thread.HighPriority
		    MainThread.Start
		    InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		  End if
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStop()
		  // Perform a manual stop of the current run
		  MainThread.Stop
		  InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		  ValueOfStatusLabel.Text = "Stopped"
		  ValueOfStopReasonLabel.Text = "Manual Stop"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetHighestValue(row as integer) As Double
		  
		  
		  Var n as integer = TotSteps - 1
		  
		  Var StartValue As Integer = (StartSlider.Value*n) \100
		  Var EndValue As Integer = StartValue + (DurationSlider.Value *n) \ 100
		  
		  If EndValue > n then 
		    EndValue = n 
		  end if 
		  
		  Var HighestValue as double = ChartArray(row,StartValue)
		  
		  for i as integer =  StartValue to EndValue
		    If ChartArray(row,i) > HighestValue then 
		      HighestValue = ChartArray(row,i)
		    end if 
		  next 
		  
		  return HighestValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLowestValue(row as integer) As Double
		  Var n as integer = TotSteps - 1
		  
		  Var StartValue As Integer = (StartSlider.Value*n) \ 100
		  Var EndValue As Integer = StartValue + (DurationSlider.Value *n) \ 100
		  
		  If EndValue > n then 
		    EndValue = n 
		  end if 
		  
		  Var LowestValue as double = ChartArray(row,StartValue)
		  
		  for i as integer = StartValue to EndValue
		    If ChartArray(row,i) < LowestValue then 
		      LowestValue = ChartArray(row,i)
		    end if 
		  next 
		  
		  return LowestValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTimeToCoalescence(TheSuper As CaseSupervisorClass) As Double
		  Var parameters As CaseParametersClass = TheSuper.CaseParameters
		  Var δ As Double = parameters.δ
		  Var η As Double = 0.25*(1.0 - δ*δ)
		  Var χa𝓁 As Double = TheSuper.WaveBuilder.SourceEvolverBase.χaL
		  Var χs𝓁 As Double = TheSuper.WaveBuilder.SourceEvolverBase.χsL
		  Var v0 As Double = parameters.V0
		  Var Threepi As Double = 3.0*parameters.π
		  Var c As Double = 743.0/2688.0 + (11.0/32.0)*η 
		  Var Term2 As Double = (32.0/3.0)*c
		  Var Term3 As Double = (64.0/3.0)*((47.0/40.0)*χs𝓁 + (15.0/32.0)*δ*χa𝓁 - Threepi/10.0)
		  Var Term4 As Double = 64.0*c*c/9.0 + (128.0/3.0)*(1855099.0/14450688.0 + (56975.0/258048.0)*η + (371.0/2048.0)*η*η)
		  Var A As Double = 5.0*Parameters.GM/(256.0*η*parameters.Year*v0^8)
		  Return A*(1 + Term2*v0*v0 + Term3*v0*v0*v0 + Term4*v0*v0*v0*v0)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetUncertaintyString(uc As Double) As String
		  If uc.IsNotANumber then
		    Return "(Imaginary)"
		  ElseIf uc.IsInfinite Then
		    Return "(Not Solved For)"
		  Else
		    Return "± " + uc.ToString
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetValueAndSolveFlag(ByRef Solve as Boolean, Source as String) As Double
		  If Source.BeginsWith("x ") Then
		    Solve = False
		    Return Source.Middle(2).ToDouble
		  Else
		    Solve = True
		    Return Source.ToDouble
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Graph()
		  GraphingCanvas.ClearFrame
		  GraphingCanvas.SetTitle("Graph of "+GraphChoicePopupMenu.SelectedRowText+" vs time")
		  GraphingCanvas.SetXLabel("t (time in s)")
		  GraphingCanvas.SetYLabel(GraphChoicePopupMenu.SelectedRowText)
		  
		  
		  if GraphChoicePopupMenu.SelectedRowText = "αDotN" then
		    
		    Var m as integer = AlphaList.LastIndex
		    
		    GraphingCanvas.SetGrid(true)
		    GraphingCanvas.DefineGraph(0,m,-.001,.001)
		    GraphingCanvas.GetContent
		    
		    
		  else 
		    
		    Var n as integer = TotSteps - 1
		    
		    Var StartValue As Integer = (StartSlider.Value*n) \100
		    Var EndValue As Integer = StartValue + (DurationSlider.Value *n) \ 100
		    
		    If EndValue > n then 
		      EndValue = n 
		    end if 
		    
		    Var j as integer = GraphChoicePopupMenu.SelectedRowIndex + 1
		    
		    GraphingCanvas.SetGrid(true)
		    GraphingCanvas.DefineGraph(ChartArray(0,StartValue),ChartArray(0,EndValue), GetLowestValue(j), GetHighestValue(j))
		    GraphingCanvas.GetContent
		    
		  end if 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub oldCreateArrays()
		  '//This method does the majority of the work in the main window. It feeds the necessary parameters
		  '//to a new instance of the EvolverClass class in order to perform the DoStep method enough times to make
		  '//sufficiently long arrays of v and each derivative of v, as well as the spin variables.
		  '
		  'τ.ResizeTo(-1)  // reset the time axis
		  '
		  'myMain = New Main(M, δ, f0, R, β, ψangle, λ0, Θ, Φ, χ1Initial, χ2Initial, PNOrder, Detectors, dτ0, K)  // instantiate the Main class
		  '
		  '// these lines are remnants from the spin-only program. The program now runs whether there's spin evolution or not
		  ''GraphAllowed = myMainWaveBuilder.ReadyToGo
		  ''if GraphAllowed then      // Checks to see if we are ready to go (i.e. if ι0 and its derivatives are nonzero)
		  '
		  'GraphArray = New ArrayClass  // set up a new array to help with graphing
		  '
		  'Var NewValues(30) As Double  // create the array to load into GraphArray
		  '
		  '//Stores the initial value of v. These values are set up in the EvolverClass constructor
		  'v = myMain.v0
		  '
		  '//Add the initial values of τ to the τ array so that the y- and x-axes match
		  'Var τnew As Double = 0
		  'τ.Add(τnew)
		  '
		  '// Perform the adaptive stepping technique to determine the time step
		  '
		  'While v < .2 And (τnew*M) < 31536000
		  'myMain.DoMainStep
		  '
		  'v = myMain.vF       //This updates the "current" value of v 
		  'τnew = τnew + dτ0          //This performs a step in τ . . .
		  'τ.Add(τnew)               //. . . and this adds it to the τ array
		  '
		  '// Add the new values to the graphing array
		  'For i As Integer = 0 To 14
		  'NewValues(i) = myMain.dzd(i)
		  'Next
		  '
		  'NewValues(15) = myMain.h
		  'NewValues(16) = myMain.hp
		  'NewValues(17) = myMain.hc
		  'NewValues(18) = myMainWaveBuilder.αAcc
		  'NewValues(19) = myMain.ιF
		  'NewValues(20) = myMain.ζ
		  'NewValues(21) = myMainWaveBuilder.LNhatF.x
		  'NewValues(22) = myMainWaveBuilder.LNhatF.y
		  'NewValues(23) = myMainWaveBuilder.LNhatF.z
		  'NewValues(24) = myMainWaveBuilder.χ1hatF.x
		  'NewValues(25) = myMainWaveBuilder.χ1hatF.y
		  'NewValues(26) = myMainWaveBuilder.χ1hatF.z
		  'NewValues(27) = myMainWaveBuilder.χ2hatF.x
		  'NewValues(28) = myMainWaveBuilder.χ2hatF.y
		  'NewValues(29) = myMainWaveBuilder.χ2hatF.z
		  'NewValues(30) = myMain.sn2
		  '
		  'GraphArray.AddAll(NewValues)
		  'Wend
		  ''end if 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetHighestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  //Parameters: InputArray() an array of double values
		  '//Return: CurrentHighest as double
		  '
		  '//This method takes an array and finds the largest value within it
		  '//It does this by comparing each item in the array to the current highest number, and if it is larger setting that number to the new current highest
		  '
		  '//Defines the current highest value and sets it equal to the first value in the input array
		  '
		  'Var CurrentHighest As Double = InputArray(Min)
		  '
		  '//For the rest of the values we check if they are higher than the current highest, if they are they become the current highest.
		  'Var i As Integer
		  'For i = Min + 1 to Max
		  'if CurrentHighest < InputArray(i) then
		  'CurrentHighest = InputArray(i)
		  'else 
		  'continue
		  'end if 
		  'next 
		  '
		  'return CurrentHighest
		  '
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetIndexOfClosest(InputValue As String) As Integer
		  '// This method helps with graphing (GetLowerBound and GetUpperBound in particular) by returning the index of the value closest to the
		  '// input value.
		  '
		  'If InputValue = "Min" then
		  'return 0
		  'elseif InputValue = "Max" then
		  'return τ.LastIndex - 1
		  'else
		  '
		  'var i As integer 
		  '
		  'Var CurrentClosest As integer = 0 
		  '
		  'Var TargetValue As Double = InputValue.ToDouble
		  '
		  'Var CurrentSmallestDifference As Double = Abs(τ(0) - TargetValue)
		  '
		  '
		  '
		  '
		  'For i = 1 to τ.LastIndex 
		  'Var Difference as Double = Abs(τ(i) - TargetValue)
		  'if Difference < CurrentSmallestDifference then
		  'CurrentSmallestDifference = Difference
		  'CurrentClosest = i
		  'else
		  'continue
		  'end if
		  'next 
		  '
		  '
		  'return CurrentClosest
		  '
		  '
		  'end if 
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetLowerBound() As Integer
		  '// Returns the lower bound based on user input
		  '
		  'Var LowerBound As Integer = GetIndexOfClosest(StartTimeTextField.Text)
		  '
		  'Return LowerBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetLowestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  '//Parameters: InputArray() an array of double values
		  '//Return: CurrentLowest as double
		  '
		  '//This method takes an array and finds the lowest value within it
		  '//It does this by comparing each item in the array to the current lowest number, and if it is lower setting that number to the new current lowest
		  '
		  '//Defines the current lowest value and sets it equal to the first value in the input array
		  'Var CurrentLowest As Double = InputArray(Min)
		  '
		  '//For the rest of the values we check if they are lower than the current lowest, if they are they become the current lowest.
		  '
		  'Var i As Integer
		  'For i = Min + 1 to Max
		  'if CurrentLowest > InputArray(i) then
		  'CurrentLowest = InputArray(i)
		  'else 
		  'continue
		  'end if 
		  'next 
		  '
		  'return CurrentLowest
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetUpperBound() As Integer
		  '// Returns the upper bound based on user input
		  '
		  'Var UpperBound As Integer = GetIndexOfClosest(EndTimeTextField.Text)
		  '
		  'Return UpperBound
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		AllCasesDone As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		AlphaList() As double
	#tag EndProperty

	#tag Property, Flags = &h0
		CasesList(-1,-1) As String
	#tag EndProperty

	#tag Property, Flags = &h0
		ChartArray(-1,-1) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OutputFile As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		TotSteps As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		UncertaintyList(-1,-1) As Double
	#tag EndProperty


#tag EndWindowCode

#tag Events InterfaceUpdateTimer
	#tag Event
		Sub Action()
		  Var TheSuper As CaseSupervisorClass = MainThread.CaseSupervisor  // Get a reference to the supervisor
		  // Whether the thread is running or not, update these values
		  ValueOfSimTimeLabel.Text = Format(TheSuper.τr*TheSuper.CaseParameters.GM/TheSuper.CaseParameters.Year, "0.0000000")
		  ValueOfVLabel.Text = Format(TheSuper.WaveBuilder.VDN,"0.000000")
		  ValueOfRunTimeLabel.Text = Format((System.Ticks - TheSuper.StartTicks)/60.0, "###0.00")
		  ValueOfStepNumberLabel.Text = TheSuper.N.ToString
		  If ValueOfTcLabel.Text = "" Then ValueOfTcLabel.Text = GetTimeToCoalescence(TheSuper).ToString
		  Var theStepPower As Integer = TheSuper.WaveBuilder.StepPowerP
		  Var theFactor as Integer
		  If theStepPower < 0 Then
		    theFactor = 2^(-theStepPower)
		    ValueOfStepRatioLabel.Text = "1/" + theFactor.ToString
		  Else
		    theFactor = 2^theStepPower
		    ValueOfStepRatioLabel.Text = theFactor.ToString
		  End if
		  // Var SNR As Double = 0.5*TheSuper.HCalculator.H0V2/TheSuper.HCalculator.Sn2
		  // ValueOfSNRLabel.Text = Format(SNR, "0.000e+00")
		  If MainThread.State = Thread.Running then  // if the thread is running
		    CaseProgressBar.Value = Round(TheSuper.N*100/TheSuper.NSteps)  // update the progress bar
		  Else // the thread has stopped, meaning that this case is done
		    CaseProgressBar.Value = 0  // set
		    StartStopButton.Caption = "Run Cases"
		    me.RunMode = Timer.RunModes.Off // and we need no more updates
		    ValueOfStatusLabel.Text = "Stopped"
		    ValueOfStopReasonLabel.Text = TheSuper.TerminationMessage
		    If TheSuper.Uncertainty <> Nil Then
		      DisplayUncertainties(TheSuper.CaseParameters, TheSuper.Uncertainty)
		      MatrixChoicePopupMenu.SelectedRowIndex = 0
		      DisplayMatrix(TheSuper.ATAMatrix)
		      ValueOfConditionLabel.Text = Format(TheSuper.UncertaintyCalculator.Condition, "0.000e-0##")
		    End if
		    
		    
		    
		    if RunFileCheckBox.Value and AllCasesDone then
		      Var t As TextOutputStream = TextOutputStream.Create(OutputFile)
		      t.WriteLine("M"+chr(9)+"δ"+chr(9)+"T0"+chr(9)+"R"+chr(9)+"β"+chr(9)+"ψ"+chr(9)+"λ0"+chr(9)+"Θ"_
		      +chr(9)+"Φ"+chr(9)+"Ω"+chr(9)+"χ10x"+chr(9)+"χ10y"+chr(9)+"χ10z"+chr(9)+"χ20x"+chr(9)+"χ20y"+chr(9)+"χ20z")
		      For i as integer = 0 to UncertaintyList.LastIndex
		        for j as integer = 0 to 15 
		          t.Write(UncertaintyList(i,j).ToString+chr(9))
		        next
		        t.write(EndOfLine)
		      next
		      t.close
		    end if 
		    
		  End if		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events GraphChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  //Add flag 
		  
		  Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CasesListBox
	#tag Event
		Function CellKeyDown(row as Integer, column as Integer, key as String) As Boolean
		  If key = "x" And row > 3 And row < 15 Then
		    If me.CellTextAt(row,column).BeginsWith("x ") Then
		      me.ActiveTextControl.Text = me.ActiveTextControl.Text.Middle(2)
		      Return True
		    Else
		      me.ActiveTextControl.Text = "x " + me.ActiveTextControl.Text
		      Return True
		    End If
		  Else
		    Return False
		  End If
		End Function
	#tag EndEvent
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  me.EditCellAt(row, column)
		  
		End Function
	#tag EndEvent
	#tag Event
		Sub Opening()
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextField
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events MatrixChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  Var theATA As Matrix = MainThread.CaseSupervisor.ATAMatrix
		  Var theY As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y
		  Var theY0 As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y0
		  Var theYNormalized As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.YNormalized
		  Var theY0Normalized As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y0Normalized
		  Var theProd As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.YInvXY
		  Var theY0NormalizedUnchanged As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y0NormUnchanged
		  
		  
		  If me.SelectedRowValue = "ATA" Then
		    DisplayMatrix(theATA)
		  ElseIf me.SelectedRowValue = "Y Original" Then
		    DisplayMatrix(theY0)
		  ElseIf me.SelectedRowValue = "YNormalized Inverted" Then
		    DisplayMatrix(theYNormalized)
		  ElseIf me.SelectedRowValue = "YNormalized Original" Then
		    DisplayMatrix(theY0Normalized)
		  ElseIf me.SelectedRowValue = "Y Inverted" Then
		    DisplayMatrix(theY)
		  ElseIf me.SelectedRowValue = "Y0 Normalized Unchanged" Then
		    DisplayMatrix(theY0NormalizedUnchanged)
		  Else
		    DisplayMatrix(theProd)
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CopyMatrixButton
	#tag Event
		Sub Pressed()
		  Var c As New Clipboard
		  Var s As String
		  Var delim As String
		  For j As Integer = 0 to MatrixListBox.LastRowIndex
		    For k As Integer = 1 to MatrixListBox.LastColumnIndex
		      s = s+ delim + MatrixListBox.CellTextAt(j,k)
		      delim = ", "
		    Next
		    delim = EndOfLine 
		  Next
		  c.Text = s
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CaseProgressBar
	#tag Event
		Sub Open()
		  me.MaximumValue = 100
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartStopButton
	#tag Event
		Sub Pressed()
		  If me.Caption = "Run" Then
		    DoStart
		    me.Caption = "Stop"
		  Else
		    me.Caption = "Run"
		    DoStop
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events RunFileCheckBox
	#tag Event
		Sub ValueChanged()
		  //To make a txt file that can be read by this code:
		  //Rows should be parameters, in the order shown, separated by tabs,
		  //All parameters need values, parameters not to be solved for should have x in front of them 
		  //Each case is on a new row 
		  
		  //Do not write data in text file with commas 
		  
		  Var TxtType As New FileType
		  TxtType.Name = "Text  file"
		  TxtType.Extensions = ".txt"
		  
		  Var f As FolderItem
		  Var textInput As TextInputStream
		  Var rowFromFile As String
		  
		  f = FolderItem.ShowOpenFileDialog(TxtType)
		  
		  If f <> Nil Then
		    textInput = TextInputStream.Open(f)
		    textInput.Encoding = Encodings.UTF8
		    
		    Do
		      rowFromFile = textInput.ReadLine
		      Var values() As String = rowFromFile.ToArray(String.Chr(9))
		      AddToCasesList(values())
		    Loop Until textInput.EndOfFile
		    
		    textInput.Close
		  End If
		  
		  
		  
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events FileProgressBar
	#tag Event
		Sub Open()
		  me.MaximumValue = 100
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartSlider
	#tag Event
		Sub ValueChanged()
		  //Add flag
		  Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events DurationSlider
	#tag Event
		Sub ValueChanged()
		  //Add flag here 
		  Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events GraphButton
	#tag Event
		Sub Pressed()
		  Graph
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events GraphingCanvas
	#tag Event
		Sub Open()
		  GraphingCanvas.SetTitle("Graph of "+GraphChoicePopupMenu.SelectedRowText+" vs time")
		  GraphingCanvas.SetXLabel("t (time in s)")
		  GraphingCanvas.SetYLabel(GraphChoicePopupMenu.SelectedRowText)
		End Sub
	#tag EndEvent
	#tag Event
		Sub DrawContent()
		  If GraphChoicePopupMenu.SelectedRowText = "αDotN" then 
		    Var m as integer = AlphaList.LastIndex
		    
		    GraphingCanvas.CurveStart(0, AlphaList(0))
		    Var k as integer
		    
		    For k = 1 To m
		      GraphingCanvas.CurveTo(k, AlphaList(k))
		    Next 
		    
		    
		  else
		    
		    
		    Var n as integer = TotSteps - 1
		    Var j as integer = GraphChoicePopupMenu.SelectedRowIndex + 1
		    
		    GraphingCanvas.CurveStart(ChartArray(0,0), ChartArray(j,0))
		    
		    For i As Integer = 1 To n
		      GraphingCanvas.CurveTo(ChartArray(0,i), ChartArray(j,i))
		    Next 
		    
		  end if 
		  
		End Sub
	#tag EndEvent
#tag EndEvents
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
		Name="Interfaces"
		Visible=true
		Group="ID"
		InitialValue=""
		Type="String"
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
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimumWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimumHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximumWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximumHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Type"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Types"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasCloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasMaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasMinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasFullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Visible=false
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Visible=false
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Visible=false
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="DefaultLocation"
		Visible=true
		Group="Behavior"
		InitialValue="2"
		Type="Locations"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Windows Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackgroundColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackgroundColor"
		Visible=true
		Group="Background"
		InitialValue="&cFFFFFF"
		Type="ColorGroup"
		EditorType="ColorGroup"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		InitialValue=""
		Type="Picture"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		InitialValue=""
		Type="DesktopMenuBar"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="AllCasesDone"
		Visible=false
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="TotSteps"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
#tag EndViewBehavior
