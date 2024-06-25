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
         Height          =   595
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nM (sols)\nδ\nTorb (s)\ntc (y)\nR (ly)\nβ (°)	\nψ (°)\nλ0\nΘ (°)\nΦ (°)	\nΩ (/sky)\nχ1	\nθ1 (°)\nφ1 (°)\nχ2\nθ2 (°)\nφ2 (°)\nρ0 (°)\nPN Order\nDetectors\nΔT (s)\nDuration (y)"
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
         InitialValue    =   ""
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
         LockLeft        =   True
         LockRight       =   False
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
         LockLeft        =   True
         LockRight       =   False
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
         LockLeft        =   True
         LockRight       =   False
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
         LockLeft        =   True
         LockRight       =   False
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
      Begin DesktopListBox CaseListBox
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
         Height          =   595
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Case 1\n10000\n0.1\n500\n\n1.0e7\n39\n24\n0\n5\n268.5\n\nx 0\nx 0\nx 0\nx 0\nx 0\nx 0\n0\n0\n2\n50\n1.0"
         Italic          =   False
         Left            =   109
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
         Width           =   83
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
      Begin DesktopLabel XExplanationLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   58
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
         Text            =   "Typing 'x' in the cells for β through φ2 will toggle whether that parameter's uncertainty is calculate or not (a displayed 'x' indicates 'not')."
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   645
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   306
      End
      Begin DesktopLabel CaptionForMatrixToDisplayLabel
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
         Width           =   112
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
         Top             =   132
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
         Top             =   166
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
         Top             =   225
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
         Top             =   226
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
         Text            =   "Source/Display Step Ratio:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   254
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   169
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
         Left            =   573
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
         Top             =   254
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
         Top             =   509
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
         Height          =   466
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Uncertainty\n?\n?\n\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?"
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
         Top             =   532
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
         Top             =   609
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
         Top             =   582
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
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
		  UpdateTc
		  
		  // Initialize the GraphChoicePopupMenu items
		  'Var tempCase As New CaseInfoClass
		  'Setting = True
		  'GraphChoicePopupMenu.RemoveAllRows
		  'Var theNames() As String = tempCase.GetPlotNames
		  'For i As Integer = 0 To theNames.LastIndex
		  'GraphChoicePopupMenu.AddRow(theNames(i))
		  'Next
		  'Setting = False
		End Sub
	#tag EndEvent


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
		Sub DisplayUncertainties(CaseInfo As CaseInfoClass)
		  'UncertaintyListBox.CellTextAt(0) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.M)))
		  'UncertaintyListBox.CellTextAt(1) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.delta)))
		  'UncertaintyListBox.CellTextAt(2) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.V0)))
		  'UncertaintyListBox.CellTextAt(3) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.R)))
		  'UncertaintyListBox.CellTextAt(4) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.beta)))
		  'UncertaintyListBox.CellTextAt(5) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.psi)))
		  'UncertaintyListBox.CellTextAt(6) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.lambda0)))
		  'Var uTheta As Double = CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.theta))
		  'Var uPhi As Double = CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.phi))
		  'UncertaintyListBox.CellTextAt(7) = GetUncertaintyString(uTheta)
		  'UncertaintyListBox.CellTextAt(8) = GetUncertaintyString(uPhi)
		  'Var d2r As Double = CaseInfo.π/180.0
		  'Var omega As Double = Sin(CaseInfo.Θ)*uTheta*uPhi/(4*CaseInfo.π)*d2r*d2r
		  'UncertaintyListBox.CellTextAt(9) = GetUncertaintyString(omega)
		  'UncertaintyListBox.CellTextAt(10) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10x)))
		  'UncertaintyListBox.CellTextAt(11) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10y)))
		  'UncertaintyListBox.CellTextAt(12) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10z)))
		  'UncertaintyListBox.CellTextAt(13) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20x)))
		  'UncertaintyListBox.CellTextAt(14) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20y)))
		  'UncertaintyListBox.CellTextAt(15) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20z)))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStart()
		  // Start running a case or cases
		  TheCases.ResizeTo(-1)  // Clear out any pre-existing cases
		  if RunFileCheckBox.Value then // if we are running cases from a file
		    GetCasesFromFile // get the cases from the file
		    OutputFile = FolderItem.ShowSaveFileDialog(FileTypeGroup1.Text, "Untitled.txt") // define an output file
		    If OutputFile = Nil Then Return // bail out if the user has cancelled
		  Else
		    TheCases.Add(GetDisplayCase)
		  End if
		  AllCasesDone = False 
		  ValueOfStatusLabel.Text = "Running"
		  ValueOfStopReasonLabel.Text = ""
		  MainThread.LoadCases(TheCases)
		  MainThread.Priority = Thread.HighPriority
		  MainThread.Start
		  InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		  
		  
		  
		  
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
		Function GetCaseFromValues(Values() As String) As CaseInfoClass
		  Var thisCase As New CaseInfoClass
		  Var Flag As Boolean
		  thisCase.SolveFor.ResizeTo(Integer(CaseInfoClass.Param.NItems) - 1)
		  thisCase.M = GetValueAndSolveFlag(Flag, Values(0))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.M)) = Flag
		  thisCase.δ = GetValueAndSolveFlag(Flag, Values(1))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.delta)) = Flag
		  thisCase.T0 = GetValueAndSolveFlag(Flag, Values(2))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.tauc)) = Flag
		  thisCase.R = GetValueAndSolveFlag(Flag, Values(3))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.R)) = Flag
		  thisCase.β = GetValueAndSolveFlag(Flag, Values(4))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.beta)) = Flag
		  thisCase.ψ = GetValueAndSolveFlag(Flag, Values(5))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.psi)) = Flag
		  thisCase.λ0 = GetValueAndSolveFlag(Flag, Values(6))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.lambda0)) = Flag
		  thisCase.Θ = GetValueAndSolveFlag(Flag, Values(7))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.theta)) = Flag
		  thisCase.Φ = GetValueAndSolveFlag(Flag, Values(8))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.phi)) = Flag
		  thisCase.χ1 = GetValueAndSolveFlag(Flag, Values(9))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.chi1)) = Flag
		  thisCase.θ1 = GetValueAndSolveFlag(Flag, Values(10))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.theta1)) = Flag
		  thisCase.φ1 = GetValueAndSolveFlag(Flag, Values(11))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.phi1)) = Flag
		  thisCase.χ2 = GetValueAndSolveFlag(Flag, Values(12))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.chi2)) = Flag
		  thisCase.θ2 = GetValueAndSolveFlag(Flag, Values(13))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.theta2)) = Flag
		  thisCase.φ2 = GetValueAndSolveFlag(Flag, Values(14))
		  thisCase.SolveFor(Integer(CaseInfoClass.Param.phi2)) = Flag
		  thisCase.ρ0 = values(15).ToDouble
		  thisCase.PNOrder = values(16).ToInteger
		  thisCase.Detectors = values(17).ToInteger
		  thisCase.ΔT = values(18).ToDouble
		  thisCase.RunDuration = values(19).ToDouble
		  thisCase.FinishConstruction
		  Return thisCase
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetCasesFromFile()
		  // To make a txt file that can be read by this code:
		  //    Rows should be parameters, in the order shown in the CaseListBox (ignoring Ω), separated by tabs.
		  //    All parameters need values, parameters not to be solved for should have x in front of them.
		  //    Do not write data in text file with commas.
		  
		  Var f As FolderItem
		  Var textInput As TextInputStream
		  Var rowFromFile As String
		  
		  f = FolderItem.ShowOpenFileDialog(FileTypeGroup1.Text)
		  
		  If f <> Nil Then
		    textInput = TextInputStream.Open(f)
		    textInput.Encoding = Encodings.UTF8
		    Do
		      rowFromFile = textInput.ReadLine
		      Var values() As String = rowFromFile.ToArray(String.Chr(9))
		      Var thisCase As CaseInfoClass = GetCaseFromValues(values)
		      TheCases.Add(thisCase)
		    Loop Until textInput.EndOfFile
		    textInput.Close
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDisplayCase() As CaseInfoClass
		  Var theValues() As String
		  Var theValue As String
		  For i As Integer = 0 to CaseListBox.LastRowIndex // for all entries in the list box
		    theValue = CaseListBox.CellTextAt(i) // get the text entered in the list box
		    if i <> 3 And i <> 10 Then theValues.Add(theValue) // skip over an entry corresponding to Ω or Tc
		  Next
		  Return GetCaseFromValues(theValues)
		  
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
		Function SultanUncertainty(value As Double, uncertainty As Double) As String
		  'Number Formatting:
		  
		  'Start the program.
		  'In the first input field or prompt, enter the value that you want to measure the uncertainty for. This can be done via a pop-up window, a slider, a number picker, or any other user interface element depending on your website design.
		  'This should be a numerical value. If any non-numeric characters are entered, a message box will appear saying "Invalid value entered!".
		  'In the second input field or prompt, enter the associated uncertainty of the value you have just entered. This can also be done via a pop-up window, a slider, a number picker, or any other user interface element.
		  'This should be a positive numerical value. The program considers uncertainty as a positive value. If you enter a negative number or zero for uncertainty, you will get a message box saying "Uncertainty must be greater than zero!".
		  'After you've entered the value and its uncertainty, submit your data. This can be done by clicking the 'Submit' button, pressing the 'Enter' key, or another action based on your website design.
		  
		  'Calculation Process:
		  
		  'Once the values are inputted correctly, the program will calculate the base-10 logarithm for both the value and uncertainty. It will then round the value and uncertainty according to the computed exponents.
		  'The program will display a message box with the computed exponents and rounded uncertainty.
		  
		  'Result Output:
		  
		  'The program will finally adjust the rounded uncertainty for the final display and determine the number of spaces needed for proper alignment.
		  'It checks whether the number is negative, and accordingly adds spaces or a minus sign before the number for proper formatting.
		  'Depending on the exponent of the value, the program will generate the final output in two formats: normal and scientific notation.
		  'If the exponent of the value is within the range from -3 to 6, both the value and the uncertainty will be displayed in normal notation.
		  'Otherwise, both will be displayed in scientific notation.
		  
		  'Special Case:
		  
		  'There is a special case where the exponent of the value is greater than 0 and the exponent of the uncertainty is less than 0. In this case, the program will prepare the value with all decimal places (0 to valueExponent) and uncertainty to 8 decimal places.
		  'Remember, this program is designed to handle and display uncertainty values correctly, but it is your responsibility to ensure the accuracy and correctness of the input data.
		  
		  If uncertainty.IsNotANumber Then
		    Return value.ToString + EndOfLine + "(Imaginary)"
		  Elseif uncertainty.IsInfinite Then
		    Return value.ToString + EndOfLine + "(Not Solved For)"
		  End If
		  
		  ' Ensure the uncertainty is a positive value. If it's not, raise an exception and display a message box
		  Try
		    If uncertainty <= 0 Then
		      Raise New RuntimeException("Uncertainty must be greater than zero!")
		    End If
		  Catch e As RuntimeException
		    MsgBox(e.Message)
		    Return ""
		  End Try
		  
		  ' Calculate the exponent of the value and uncertainty, which is the integer part of their base-10 logarithm
		  Var valueExponent As Integer = Floor(Log(Abs(value)) / Log(10))
		  Var uncertaintyExponent As Integer = Floor(Log(uncertainty) / Log(10))
		  
		  ' Compute the rounded value mantissa, which is the absolute value divided by 10 raised to its exponent, then rounded to 3 decimal places
		  Var valueMantissa As Double = abs(value / (10 ^ valueExponent))
		  Var roundedValueMantissa As Double = Round(valueMantissa * 1000) / 1000
		  
		  ' Compute the rounded uncertainty in a similar way, but round to the nearest integer
		  Var uncertaintyMantissa As Double = uncertainty / (10 ^ uncertaintyExponent)
		  Var roundedUncertainty As Double = Round(uncertaintyMantissa * 10)
		  
		  
		  ' Adjust the rounded uncertainty for final display by multiplying by 10^-3 and rounding to 3 decimal places
		  Var uncertaintyForDisplay As Double = roundedUncertainty * 10 ^ (-3)
		  
		  ' Determine whether a space or a minus sign is needed before the value
		  Var BeforeValue As String = "  "
		  if value < 0 then BeforeValue = "- "
		  
		  ' Determine the number of spaces needed for proper alignment in the final display
		  Var spacesNeeded As Integer
		  Var spaceString As String = ""
		  
		  ' Set the format string
		  Dim formattedValue, formattedUncertainty As String 
		  formattedValue = Format(value, "0.00000000") 
		  
		  ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		  if uncertaintyExponent < -7 Then
		    formattedUncertainty = "0.00000000*"
		  else
		    formattedUncertainty = Format(uncertainty, "0.00000000")
		  End If
		  
		  ' Determine the number of spaces needed to align the uncertainty under the last digit of the value
		  spacesNeeded = Len(formattedValue) - Len(formattedUncertainty)
		  
		  ' Generate the string of spaces needed for alignment
		  For i As Integer = 1 To spacesNeeded
		    spaceString = spaceString + " "
		  Next
		  
		  
		  
		  ' Check for the special case where the exponent of the value is greater than 0 and the exponent of the uncertainty is less than 0.
		  If valueExponent > 0 And uncertaintyExponent < 0 Then
		    ' Prepare the value with all decimal places (0 to valueExponent) and uncertainty to 8 decimal places
		    Var formatStr As String = "0."
		    For i As Integer = 1 To valueExponent
		      formatStr = formatStr + "0"
		    Next
		    
		    Var valueStr As String = Format(value, formatStr)
		    Var uncertaintyStr As String
		    ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		    if uncertaintyExponent < -7 Then
		      uncertaintyStr = "0.00000000*"
		    else
		      uncertaintyStr = Format(uncertainty, "0.00000000")
		    End If
		    
		    ' Determine the number of spaces needed to align the uncertainty under the last digit of the value
		    spacesNeeded = valueStr.Length - uncertaintyStr.Length + 2  ' add two more for the space and "±"
		    
		    ' Generate the string of spaces needed for alignment
		    spaceString = ""
		    For i As Integer = 1 To spacesNeeded
		      spaceString = spaceString + " "
		    Next
		    
		    ' Return the final formatted string
		    Return BeforeValue + valueStr + EndOfLine + "±" + spaceString + uncertaintyStr
		  End If
		  
		  ' Check if the value exponent is within the range -3 to 6. If it is, format the value and uncertainty accordingly
		  If valueExponent >= -3 And valueExponent <= 6 Then
		    ' Format value and uncertainty
		    Var valueStr As String
		    Var uncertaintyStr As String
		    
		    ' Find number of decimal places in uncertainty
		    Var numDecimalPlacesUncertainty As Integer = Len(Uncertainty.ToString) - InStr(Uncertainty.ToString, ".")
		    
		    ' Select correct format based on number of decimal places in uncertainty
		    If numDecimalPlacesUncertainty = 1 Then
		      valueStr = Format(value, "0.0")
		      uncertaintyStr = Format(uncertainty, "0.0")
		    Else
		      valueStr = Format(value, "0.00")
		      If valueExponent > -4 Then
		        uncertaintyStr = Format(uncertainty, "0.0") ' Round uncertainty to one decimal place
		      Else
		        uncertaintyStr = Format(uncertainty, "0.00")
		      End If
		    End If
		    
		    ' Determine the number of spaces needed to align the uncertainty under the value
		    spacesNeeded = valueStr.Length - uncertaintyStr.Length
		    
		    ' Generate the string of spaces needed for alignment
		    spaceString = ""
		    For i As Integer = 1 To spacesNeeded
		      spaceString = spaceString + " "
		    Next
		    
		    ' Return the final formatted string
		    Return BeforeValue + valueStr + EndOfLine + spaceString + "± " + uncertaintyStr
		  Else
		    ' If the value exponent is outside the range -3 to 6, use scientific notation for the value and uncertainty
		    Var plusMinusLine As String
		    ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		    if uncertaintyExponent < -7 Then
		      plusMinusLine = spaceString + "± 0.00000000* e" + Str(valueExponent)
		    else
		      plusMinusLine = spaceString + "± " + Str(uncertaintyForDisplay) + "e" + Str(valueExponent)
		    End If
		    Return BeforeValue + Str(roundedValueMantissa) + "e" + Str(valueExponent) + EndOfLine + plusMinusLine
		  End If
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateTc()
		  Var displayCase As CaseInfoClass = GetDisplayCase
		  Var tc As Double = displayCase.τc*displayCase.GM/displayCase.Year
		  Var tcString As String = Format(tc, "0.0000e#")
		  CaseListBox.CellTextAt(3,0) = tcString
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		AllCasesDone As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		OutputFile As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		Setting As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		TheCases() As CaseInfoClass
	#tag EndProperty


#tag EndWindowCode

#tag Events InterfaceUpdateTimer
	#tag Event
		Sub Action()
		  Var TheSuper As CaseSupervisorClass = MainThread.CaseSupervisor  // Get a reference to the supervisor
		  // Whether the thread is running or not, update these values
		  ValueOfSimTimeLabel.Text = Format(TheSuper.τr*TheSuper.CaseInfo.GM/TheSuper.CaseInfo.Year, "0.0000000")
		  ValueOfVLabel.Text = Format(TheSuper.WaveBuilder.VDN,"0.000000")
		  ValueOfRunTimeLabel.Text = Format((System.Ticks - TheSuper.StartTicks)/60.0, "###0.00")
		  ValueOfStepNumberLabel.Text = TheSuper.N.ToString
		  If MainThread.State = Thread.Running then  // if the thread is running
		    CaseProgressBar.Value = Round(TheSuper.N*100/TheSuper.NSteps)  // update the progress bar
		  Else // the thread has stopped, meaning that this case is done
		    CaseProgressBar.Value = 0  // set
		    StartStopButton.Caption = "Run Cases"
		    me.RunMode = Timer.RunModes.Off // and we need no more updates
		    ValueOfStatusLabel.Text = "Stopped"
		    ValueOfStopReasonLabel.Text = TheSuper.TerminationMessage
		    If TheSuper.CaseInfo.Uncertainties <> Nil Then
		      DisplayUncertainties(TheSuper.CaseInfo)
		      MatrixChoicePopupMenu.SelectedRowIndex = 0
		      DisplayMatrix(TheSuper.ATAMatrix)
		      ValueOfConditionLabel.Text = Format(TheSuper.UncertaintyCalculator.Condition, "0.000e-0##")
		    End if
		    
		    if RunFileCheckBox.Value and AllCasesDone then
		      Var t As TextOutputStream = TextOutputStream.Create(OutputFile)
		      t.WriteLine("M"+chr(9)+"δ"+chr(9)+"T0"+chr(9)+"R"+chr(9)+"β"+chr(9)+"ψ"+chr(9)+"λ0"+chr(9)+"Θ"_
		      +chr(9)+"Φ"+chr(9)+"Ω"+chr(9)+"χ10x"+chr(9)+"χ10y"+chr(9)+"χ10z"+chr(9)+"χ20x"+chr(9)+"χ20y"+chr(9)+"χ20z")
		      For Each caseItem As CaseInfoClass In TheCases
		        Var theUncertainties() As String
		        For Each unc As Double In caseItem.Uncertainties
		          theUncertainties.Add(unc.ToString)
		        Next
		        Var theString As String = String.FromArray(theUncertainties, chr(9))
		        t.WriteLine(theString)
		      Next
		      t.Close
		    end if 
		    
		  End if		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ParamNameListBox
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True  // do not allow sorting
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events GraphChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  If not Setting Then
		    
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CaseListBox
	#tag Event
		Function CellKeyDown(row as Integer, column as Integer, key as String) As Boolean
		  If key = "x" And row > 4 And row < 16 Then
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
		  If row <> 3 And row <> 10 Then
		    me.EditCellAt(row, column)
		  End If
		  Return True
		  
		End Function
	#tag EndEvent
	#tag Event
		Sub Opening()
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextField
		End Sub
	#tag EndEvent
	#tag Event
		Function PaintCellBackground(g As Graphics, row As Integer, column As Integer) As Boolean
		  If row = 10 Then
		    g.DrawingColor = Color.RGB(230,230,230)
		    g.FillRectangle(0,0,g.Width,g.Height)
		  End If
		End Function
	#tag EndEvent
	#tag Event
		Sub CellAction(row As Integer, column As Integer)
		  UpdateTc
		End Sub
	#tag EndEvent
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True // do not allow sorting
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events MatrixListBox
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True  // Do not allow sorting
		End Function
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
#tag Events UncertaintyListBox
	#tag Event
		Function PaintCellBackground(g As Graphics, row As Integer, column As Integer) As Boolean
		  If row = 2 Then
		    g.DrawingColor = Color.RGB(230,230,230)
		    g.FillRectangle(0,0, g.Width, g.Height)
		  End If
		End Function
	#tag EndEvent
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True  // do not allow sorting
		End Function
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
		  'Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events DurationSlider
	#tag Event
		Sub ValueChanged()
		  //Add flag here 
		  'Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events GraphButton
	#tag Event
		Sub Pressed()
		  'Graph
		  
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
		Name="Setting"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
#tag EndViewBehavior
