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
   Height          =   795
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   1095792639
   MenuBarVisible  =   False
   MinimumHeight   =   64
   MinimumWidth    =   64
   Resizeable      =   True
   Title           =   "LISA Simulator"
   Type            =   0
   Visible         =   True
   Width           =   1278
   Begin DesktopListBox LBVariableNamesMain
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "50%,50%"
      DefaultRowHeight=   34
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   False
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   633
      Index           =   -2147483648
      InitialValue    =   "case\nM (sols)\nδ\nf(mHz)\nR(ly)\nβ (°)\nψ (°)\nλ0\nΘ (°)\nΦ(°)\nχ10x \nχ10y\nχ10z\nχ20x\nχ20y\nχ20z\nPN Order\nDetectors\ndt(s)\nK"
      Italic          =   False
      Left            =   53
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   96
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   263
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopLabel LAOmega
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   328
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Omega:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   624
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   100
   End
   Begin DesktopListBox LBCurrentValues
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   7
      ColumnWidths    =   "10%,10%,10%,10%,10%,10%"
      DefaultRowHeight=   38
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   True
      HeadingIndex    =   -1
      Height          =   63
      Index           =   -2147483648
      InitialValue    =   "Case Time(s)	Time(y)	y^2 (~v/c)	r/GM	h	SNR	Stop b/c"
      Italic          =   False
      Left            =   40
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   4
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   28
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   1218
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopButton BUAdd10Cases
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Add 10 Cases"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   173
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   7
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   741
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   118
   End
   Begin DesktopButton BUAddColumn
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Add Column"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   40
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   8
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   741
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   97
   End
   Begin DesktopButton BUStop
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Stop"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   1178
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   10
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   122
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopPopupMenu PMExportMain
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      InitialValue    =   "Binary\n.csv\n"
      Italic          =   False
      Left            =   1086
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   11
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopButton BUExportMain
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Export"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   1178
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   12
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopPopupMenu PMGraphMain
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      InitialValue    =   "dh/dM\ndh/dδ\ndh/df0\ndh/dR\ndh/dβ\ndh/dψ\ndh/dλ0\ndh/dΘ\ndh/dΦ\ndh/dχ10x\ndh/dχ10y\ndh/dχ10z\ndh/dχ20x\ndh/dχ20y\ndh/dχ20z\nh\nhp\nhc\nα\nι\n ζ\nLNhatx\nLNhaty\nLNhatz\nχ1hatX\nχ1hatY\nχ1hatZ\nχ2hatX\nχ2hatY\nχ2hatZ\nnoise"
      Italic          =   False
      Left            =   556
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   13
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   180
   End
   Begin DesktopLabel LAGraphMain
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   495
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   14
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Graph of:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   100
   End
   Begin DesktopButton BUViewCases
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "View Cases"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   808
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   15
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   122
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   90
   End
   Begin DesktopListBox LBOmega
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   ""
      DefaultRowHeight=   -1
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   0
      HasBorder       =   True
      HasHeader       =   False
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   True
      HeadingIndex    =   -1
      Height          =   38
      Index           =   -2147483648
      InitialValue    =   ""
      Italic          =   False
      Left            =   328
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   16
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   643
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   128
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopButton BUStart
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Start"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   495
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   9
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   122
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopCheckBox CBWriteCases
      AllowAutoDeactivate=   True
      Bold            =   False
      Caption         =   "Untitled"
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   1063
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      Scope           =   0
      TabIndex        =   17
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Value           =   False
      Visible         =   True
      VisualState     =   0
      Width           =   18
   End
   Begin DesktopLabel LAExportCheckBox
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   873
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   18
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Check to write all cases to file:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   189
   End
   Begin Graph CAGraphMain
      AllowAutoDeactivate=   False
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
      Height          =   483
      Index           =   -2147483648
      InitialParent   =   ""
      InvLog10        =   0.0
      Left            =   495
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
      TabIndex        =   19
      TabPanelIndex   =   0
      TabStop         =   True
      TickFont        =   ""
      TickFSize       =   0
      Title           =   ""
      TitleH          =   0
      TitleV          =   0
      Tooltip         =   ""
      Top             =   292
      Transparent     =   True
      Visible         =   True
      Width           =   763
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
   Begin DesktopButton BUGraphMain
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Graph"
      Default         =   False
      Enabled         =   False
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   748
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   20
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   260
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopLabel LAEndTime
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   407
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   21
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Ending τ:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   721
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   76
   End
   Begin DesktopLabel LAStartTime
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   315
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   22
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Starting τ:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   721
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopTextField EndτField
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
      Height          =   22
      Hint            =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   403
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      MaximumCharactersAllowed=   0
      Password        =   False
      ReadOnly        =   False
      Scope           =   0
      TabIndex        =   23
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Max"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   741
      Transparent     =   False
      Underline       =   False
      ValidationMask  =   ""
      Visible         =   True
      Width           =   80
   End
   Begin DesktopTextField StartτField
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
      Height          =   22
      Hint            =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   315
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   False
      MaximumCharactersAllowed=   0
      Password        =   False
      ReadOnly        =   False
      Scope           =   0
      TabIndex        =   24
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Min"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   741
      Transparent     =   False
      Underline       =   False
      ValidationMask  =   ""
      Visible         =   True
      Width           =   80
   End
   Begin Thread myThread
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
   Begin DesktopButton BUMatrix
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Matrices"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   955
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   False
      LockRight       =   True
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   25
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   122
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopProgressWheel ProgressWheel1
      Active          =   False
      AllowAutoDeactivate=   True
      AllowTabStop    =   True
      Enabled         =   True
      Height          =   37
      Index           =   -2147483648
      InitialParent   =   ""
      Left            =   587
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      PanelIndex      =   0
      Scope           =   0
      TabIndex        =   26
      TabPanelIndex   =   0
      Tooltip         =   ""
      Top             =   113
      Transparent     =   False
      Visible         =   False
      Width           =   33
      _mIndex         =   0
      _mInitialParent =   ""
      _mName          =   ""
      _mPanelIndex    =   0
   End
   Begin Timer TMUpdate
      Enabled         =   True
      Index           =   -2147483648
      LockedInPosition=   False
      Period          =   500
      RunMode         =   0
      Scope           =   0
      TabPanelIndex   =   0
   End
   Begin ProgressBar PGBProgress
      AllowAutoDeactivate=   True
      Enabled         =   True
      Height          =   14
      Indeterminate   =   False
      Index           =   -2147483648
      InitialParent   =   ""
      Left            =   643
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      MaximumValue    =   100
      Scope           =   0
      TabIndex        =   27
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   122
      Transparent     =   True
      Value           =   0.0
      Visible         =   True
      Width           =   134
   End
   Begin DesktopListBox LBMain
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   ""
      DefaultRowHeight=   -1
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   True
      HeadingIndex    =   -1
      Height          =   281
      Index           =   -2147483648
      InitialValue    =   ""
      Italic          =   False
      Left            =   513
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   29
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   222
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   378
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopListBox LBOutputValues
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "50%,50%"
      DefaultRowHeight=   34
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   False
      HasHorizontalScrollbar=   True
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   494
      Index           =   -2147483648
      InitialParent   =   ""
      InitialValue    =   "Uncertainties\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-\n-"
      Italic          =   False
      Left            =   328
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   True
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   30
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   96
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   173
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopListBox LBInputValues
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   True
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "50%,50%"
      DefaultRowHeight=   34
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   False
      HasHorizontalScrollbar=   True
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   633
      Index           =   -2147483648
      InitialParent   =   ""
      InitialValue    =   "1\n10000\n0\n2.000\n1000\n39\n24\n0\n5\n268.5\n*0.0\n*0.0\n*0.0\n*0.0\n*0.0\n*0.0\n4\n2\n50\n0"
      Italic          =   False
      Left            =   149
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   True
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   31
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   96
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   173
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
      Begin DesktopCheckBox CheckBox1
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   444
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox2
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   1
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   479
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox3
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   2
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   512
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox4
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   3
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   545
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox5
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   4
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   578
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox6
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   5
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   612
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox7
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   6
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   409
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox8
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   7
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   376
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox9
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   8
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   340
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox10
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   9
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   307
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox11
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   10
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   274
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox12
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   11
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   240
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox13
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   12
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   205
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
      Begin DesktopCheckBox CheckBox14
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   13
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   172
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   18
      End
      Begin DesktopCheckBox CheckBox15
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   ""
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   21
         Index           =   -2147483648
         InitialParent   =   "LBInputValues"
         Italic          =   False
         Left            =   301
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   14
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   138
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   19
      End
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Event
		Sub Opening()
		  str = LBInputValues.width.tostring
		  LBMain.ColumnCount = 4
		  LBMain.HeaderAt(0) = "Use"
		  LBMain.HeaderAt(1) = "Case"
		  LBMain.HeaderAt(2) = "Values"
		  LBMain.HeaderAt(3) = "Uncertainties"
		  
		  
		  LBMain.AddRow(Array("","M (sols)", "10000", "0"))
		  LBMain.AddRow(Array("","δ", "0", "0"))
		  LBMain.AddRow(Array("","f(mHz)", "2.000", "0"))
		  LBMain.AddRow(Array("","R(ly)", "1000", "0"))
		  LBMain.AddRow(Array("","β (°)", "39", "0"))
		  LBMain.AddRow(Array("","ψ (°)", "24", "0"))
		  LBMain.AddRow(Array("","λ0", "0", "0"))
		  LBMain.AddRow(Array("","Θ (°)", "5", "0"))
		  LBMain.AddRow(Array("","Φ(°)", "268.5.", "0"))
		  LBMain.AddRow(Array("","χ10x ", "*0.0", "0"))
		  LBMain.AddRow(Array("","χ10y", "*0.0", "0"))
		  LBMain.AddRow(Array("","χ10z", "*0.0", "0"))
		  LBMain.AddRow(Array("","χ20x ", "*0.0", "0"))
		  LBMain.AddRow(Array("","χ20y", "*0.0", "0"))
		  LBMain.AddRow(Array("","χ20z", "*0.0", "0"))
		  LBMain.AddRow(Array("","PN Order", "4", "0"))
		  LBMain.AddRow(Array("","Detectors", "2", "0"))
		  LBMain.AddRow(Array("","dt(s)", "50", "0"))
		  LBMain.AddRow(Array("","K", "0", "0"))
		  For i As Integer = 0 to 18
		    LBMain.CellTypeAt(i,0) = DesktopListBox.CellTypes.CheckBox
		  Next
		  LBMain.ColumnTypeAt(1) = DesktopListBox.CellTypes.TextField
		  LBMain.ColumnTypeAt(2) = DesktopListBox.CellTypes.TextField
		  LBMain.DefaultRowHeight = 35
		End Sub
	#tag EndEvent

	#tag Event
		Sub Resizing()
		  LBInputValues.DefaultRowHeight = LBInputValues.height \ 20
		  LBVariableNamesMain.DefaultRowHeight = LBInputValues.height \ 20
		  LBOutputValues.DefaultRowHeight = LBInputValues.height \ 20
		  
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub CreateArrays()
		  //This method does the majority of the work in the main window. It feeds the necessary parameters
		  //to a new instance of the Evolver class in order to perform the DoStep method enough times to make
		  //sufficiently long arrays of v and each derivative of v, as well as the spin variables.
		  
		  τ.ResizeTo(-1)  // reset the time axis
		  
		  myMain = New Main(M, δ, f0, R, β, ψangle, λ0, Θ, Φ, χ1Initial, χ2Initial, PNOrder, Detectors, dτ0, K)  // instantiate the Main class
		  
		  // these lines are remnants from the spin-only program. The program now runs whether there's spin evolution or not
		  'GraphAllowed = myMain.myEvolver.ReadyToGo
		  'if GraphAllowed then      // Checks to see if we are ready to go (i.e. if ι0 and its derivatives are nonzero)
		  
		  GraphArray = New ArrayClass  // set up a new array to help with graphing
		  
		  Var NewValues(30) As Double  // create the array to load into GraphArray
		  
		  //Stores the initial value of v. These values are set up in the Evolver constructor
		  v = myMain.v0
		  
		  //Add the initial values of τ to the τ array so that the y- and x-axes match
		  Var τnew As Double = 0
		  τ.Add(τnew)
		  
		  // Perform the adaptive stepping technique to determine the time step
		  
		  While v < .2 And (τnew*M) < 31536000
		    myMain.DoMainStep
		    
		    v = myMain.vF       //This updates the "current" value of v 
		    τnew = τnew + dτ0          //This performs a step in τ . . .
		    τ.Add(τnew)               //. . . and this adds it to the τ array
		    
		    // Add the new values to the graphing array
		    For i As Integer = 0 To 14
		      NewValues(i) = myMain.dzd(i)
		    Next
		    
		    NewValues(15) = myMain.h
		    NewValues(16) = myMain.hp
		    NewValues(17) = myMain.hc
		    NewValues(18) = myMain.myEvolver.αAcc
		    NewValues(19) = myMain.ιF
		    NewValues(20) = myMain.ζ
		    NewValues(21) = myMain.myEvolver.LNhatF.x
		    NewValues(22) = myMain.myEvolver.LNhatF.y
		    NewValues(23) = myMain.myEvolver.LNhatF.z
		    NewValues(24) = myMain.myEvolver.χ1hatF.x
		    NewValues(25) = myMain.myEvolver.χ1hatF.y
		    NewValues(26) = myMain.myEvolver.χ1hatF.z
		    NewValues(27) = myMain.myEvolver.χ2hatF.x
		    NewValues(28) = myMain.myEvolver.χ2hatF.y
		    NewValues(29) = myMain.myEvolver.χ2hatF.z
		    NewValues(30) = myMain.sn2
		    
		    GraphArray.AddAll(NewValues)
		  Wend
		  'end if 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Export()
		  // need to rewrite this file to work with the correct parameters
		  
		  'var OutputFile as folderitem
		  'var Stream as TextOutputStream 
		  '
		  'OutputFile = FolderItem.ShowSaveFileDialog("csv", "OutputFile.csv")
		  '
		  'if Outputfile <> nil Then
		  'Stream = TextOutputStream.Create(OutputFile)
		  'If ExportMenu.SelectedRowIndex = 0 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString +","+v(i).ToString + EndOfLine)
		  'Next 
		  'elseif ExportMenu.SelectedRowIndex = 1 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + dvdδ(i).ToString + EndOfLine)
		  'Next 
		  'elseif ExportMenu.SelectedRowIndex = 2 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + dvdχ1ℓ(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 3 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + dvdχ2ℓ(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 4 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + ι(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 5 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + α(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 6 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ1mag(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 7 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ1hatX(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 8 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ1hatY(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 9 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ1hatZ(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 10 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ2mag(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 11 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ2hatX(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 12 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ2hatY(i).ToString + EndOfLine)
		  'Next 
		  'elseif  ExportMenu.SelectedRowIndex = 13 Then
		  'For i As Integer = 0 To τ.Ubound
		  'stream.write(τ(i).ToString + "," + χ2hatZ(i).ToString + EndOfLine)
		  'Next 
		  'End If
		  'Stream.close
		  'end if 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetCurrentCaseValues() As Double()
		  // Returns an array with the current case values
		  
		  Var CurrentValues(19) As Double
		  
		  CurrentValues(0) = CaseCounter
		  
		  CurrentValues(1) = M
		  
		  CurrentValues(2) = δ
		  
		  CurrentValues(3) = f0
		  
		  CurrentValues(4) = R
		  
		  CurrentValues(5) = β
		  
		  CurrentValues(6) = ψangle
		  
		  CurrentValues(7) = λ0
		  
		  CurrentValues(8) = Θ
		  
		  CurrentValues(9) = Φ
		  
		  CurrentValues(10) = χ1Initial.x
		  
		  CurrentValues(11) = χ1Initial.y
		  
		  CurrentValues(12) = χ1Initial.z
		  
		  CurrentValues(13) = χ2Initial.x
		  
		  CurrentValues(14) = χ2Initial.y
		  
		  CurrentValues(15) = χ2Initial.z
		  
		  CurrentValues(16) = z0
		  
		  CurrentValues(17) = PNOrder
		  
		  CurrentValues(18) = Detectors
		  
		  CurrentValues(19) = dτ0
		  
		  
		  return CurrentValues
		  
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetHighestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  //Parameters: InputArray() an array of double values
		  //Return: CurrentHighest as double
		  
		  //This method takes an array and finds the largest value within it
		  //It does this by comparing each item in the array to the current highest number, and if it is larger setting that number to the new current highest
		  
		  //Defines the current highest value and sets it equal to the first value in the input array
		  
		  Var CurrentHighest As Double = InputArray(Min)
		  
		  //For the rest of the values we check if they are higher than the current highest, if they are they become the current highest.
		  Var i As Integer
		  For i = Min + 1 to Max
		    if CurrentHighest < InputArray(i) then
		      CurrentHighest = InputArray(i)
		    else 
		      continue
		    end if 
		  next 
		  
		  return CurrentHighest
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetIndexOfClosest(InputValue As String) As Integer
		  // This method helps with graphing (GetLowerBound and GetUpperBound in particular) by returning the index of the value closest to the
		  // input value.
		  
		  If InputValue = "Min" then
		    return 0
		  elseif InputValue = "Max" then
		    return τ.LastIndex - 1
		  else
		    
		    var i As integer 
		    
		    Var CurrentClosest As integer = 0 
		    
		    Var TargetValue As Double = InputValue.ToDouble
		    
		    Var CurrentSmallestDifference As Double = Abs(τ(0) - TargetValue)
		    
		    
		    
		    
		    For i = 1 to τ.LastIndex 
		      Var Difference as Double = Abs(τ(i) - TargetValue)
		      if Difference < CurrentSmallestDifference then
		        CurrentSmallestDifference = Difference
		        CurrentClosest = i
		      else
		        continue
		      end if
		    next 
		    
		    
		    return CurrentClosest
		    
		    
		  end if 
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLowerBound() As Integer
		  // Returns the lower bound based on user input
		  
		  Var LowerBound As Integer = GetIndexOfClosest(StartτField.Text)
		  
		  Return LowerBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLowestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  //Parameters: InputArray() an array of double values
		  //Return: CurrentLowest as double
		  
		  //This method takes an array and finds the lowest value within it
		  //It does this by comparing each item in the array to the current lowest number, and if it is lower setting that number to the new current lowest
		  
		  //Defines the current lowest value and sets it equal to the first value in the input array
		  Var CurrentLowest As Double = InputArray(Min)
		  
		  //For the rest of the values we check if they are lower than the current lowest, if they are they become the current lowest.
		  
		  Var i As Integer
		  For i = Min + 1 to Max
		    if CurrentLowest > InputArray(i) then
		      CurrentLowest = InputArray(i)
		    else 
		      continue
		    end if 
		  next 
		  
		  return CurrentLowest
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetUpperBound() As Integer
		  // Returns the upper bound based on user input
		  
		  Var UpperBound As Integer = GetIndexOfClosest(EndτField.Text)
		  
		  Return UpperBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Graph()
		  //Parameters: None 
		  //Return: None 
		  // This method populates the arrays y and τ with the correct values, and then plots them in the universeGraph canvas
		  
		  //Clearing y
		  y.ResizeTo(-1) 
		  
		  //Populating a with the correct values from our big array
		  y = GraphArray.GetAllValues(PMGraphMain.SelectedRowIndex)
		  
		  Var Min As Integer = GetLowerBound
		  Var Max As Integer = GetUpperBound 
		  
		  //Creating the graph
		  MainWindow.CAGraphMain.SetGrid(true)
		  CAGraphMain.SetTitle("Graph of " +PMGraphMain.RowValueAt(PMGraphMain.SelectedRowIndex) + " vs time " )
		  CAGraphMain.SetXLabel("τ")
		  CAGraphMain.SetYLabel(PMGraphMain.RowValueAt(PMGraphMain.SelectedRowIndex))
		  MainWindow.CAGraphMain.DefineGraph(τ(Min), τ(Max), GetLowestValue(y, Min, Max), GetHighestValue(y, Min, Max))
		  MainWindow.CAGraphMain.GetContent
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PopulateUncertainties(ATA(, ) As Double)
		  // This method takes the ATA matrix and calculates the uncertainties from it before displaying them on the UI.
		  
		  Var thisUncertaintyCalculator As New UncertaintyCalculator(ATA, solveList)  // set up an uncertainty calculator
		  
		  // normalize and arrange the matrix
		  thisUncertaintyCalculator.DiagNormalize
		  thisUncertaintyCalculator.Arrange
		  
		  // invert the matrix
		  Var invertCheck As Integer = thisUncertaintyCalculator.InvertY
		  
		  While invertCheck <> 0
		    thisUncertaintyCalculator.Y.RemoveInds(invertCheck, invertCheck)
		    MessageBox("Error in inverting matrix. Row: " + invertCheck.toString + ". Row was removed.")
		    thisUncertaintyCalculator.solveList(invertCheck - 1) = False
		    invertCheck = thisUncertaintyCalculator.InvertY
		  Wend
		  
		  // if the inversion was successful, unarrange the matrix, then calculate and populate the uncertainties
		  thisUncertaintyCalculator.Unarrange
		  thisUncertaintyCalculator.CalculateUncertainties
		  
		  // populate the uncertainties in the main window
		  For i As Integer = 0 to 14
		    If thisUncertaintyCalculator.σ(i) < 1e-98 Then
		      LBOutputValues.CellTextAt(i + 1, CaseCounter) = "-"
		    Else
		      LBOutputValues.CellTextAt(i + 1, CaseCounter) = Format(thisUncertaintyCalculator.σ(i), "#.##e")
		    End If
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetInitialValues(CaseCounter As Integer)
		  //This method takes the inputs from LBInputValues and sets the properties in the main window equal to them
		  
		  Var π As Double = 3.141592653589793238462
		  
		  // check if we are solving for the various parameters and create a list of booleans to store this data
		  Var values() As Double
		  For i As Integer = 1 To 19
		    If LBInputValues.CellTextAt(i, CaseCounter).Left(1) = "*" Then
		      values.Add(LBInputValues.CellTextAt(i, CaseCounter).Replace("*", "").ToDouble)
		      solveList.Add(False)
		    Else
		      values.Add(LBInputValues.CellTextAt(i, CaseCounter).ToDouble)
		      solveList.Add(True)
		    End If
		  Next
		  
		  M = values(0)*(4.927e-6) // total mass of the system (in seconds)
		  
		  δ = values(1)  // Defined to be (m1 − m2)/M , so characterizes the mass difference
		  
		  f0 = values(2)  //The initial value of the system’s orbital frequency in the system’s own reference frame 
		  
		  R = values(3) * 9.454254955488e15 / 2.99792458e8  // The system’s luminosity distance from our solar system (in seconds)
		  
		  β = values(4)*(π/180)  // angle between initial total angular momentum and line of sight
		  
		  ψangle = values(5)*(π/180)  // angle of the total angular momentum around line of sight
		  
		  λ0 = values(6)  // initial angle of the vector that points from the more massive to less massive star and the precessed x-axis
		  
		  Θ = values(7)*(π/180)  // altitude angle of the source in the sky
		  
		  Φ = values(8)*(π/180) // azimuth angle of the source around the ecliptic 
		  
		  χ1Initial = New Vector(values(9), values(10), values(11))  // the spin vector of star 1 (in units of (m1)^2
		  
		  χ2Initial = New Vector(values(12), values(13), values(14)) // the spin vector of star 2 (in units of (m2)^2
		  
		  PNOrder = values(15)  // post-Newtonian order
		  
		  Detectors = values(16)  // number of detectors
		  
		  dτ0 = values(17)/M  // initial unitless time step
		  
		  K = values(18)  // initial constant angle value needed for detector functions
		  
		  
		  
		  CaseCounter = CaseCounter + 1
		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		arrangedMatrix As String
	#tag EndProperty

	#tag Property, Flags = &h0
		CaseCounter As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h0
		Detectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		dτ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		f0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FullTurnsArray() As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GraphAllowed As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		GraphArray As ArrayClass
	#tag EndProperty

	#tag Property, Flags = &h0
		GraphedYet As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		invertedMatrix As String
	#tag EndProperty

	#tag Property, Flags = &h0
		K As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		myMain As Main
	#tag EndProperty

	#tag Property, Flags = &h0
		normalizedMatrix As String
	#tag EndProperty

	#tag Property, Flags = &h0
		originalMatrix As String
	#tag EndProperty

	#tag Property, Flags = &h0
		PNOrder As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		R As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		solveList() As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		str As String
	#tag EndProperty

	#tag Property, Flags = &h0
		timeStep() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		unarrangedMatrix As String
	#tag EndProperty

	#tag Property, Flags = &h0
		v As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		v0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		y() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τ() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τMainF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τMainN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1Initial As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2Initial As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψangle As Double
	#tag EndProperty


#tag EndWindowCode

#tag Events LBVariableNamesMain
	#tag Event
		Function MouseWheel(x As Integer, y As Integer, deltaX As Integer, deltaY As Integer) As Boolean
		  return True
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events BUAddColumn
	#tag Event
		Sub Pressed()
		  LBInputValues.ColumnCount = LBInputValues.ColumnCount + 1
		  LBOutputValues.ColumnCount = LBInputValues.ColumnCount + 1
		  
		  str = str +","+ LBInputValues.Width.tostring
		  
		  LBInputValues.ColumnWidths = str 
		  LBOutputValues.ColumnWidths = str 
		  
		  LBInputValues.CellTextAt(0, LBInputValues.ColumnCount-1) = LBInputValues.ColumnCount.tostring
		  LBOutputValues.CellTextAt(0, LBOutputValues.ColumnCount-1) = LBOutputValues.ColumnCount.tostring
		  
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BUViewCases
	#tag Event
		Sub Pressed()
		  CasesWindow.show
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BUStart
	#tag Event
		Sub Pressed()
		  SetInitialValues(CaseCounter)
		  
		  BUGraphMain.Enabled = False
		  
		  ProgressWheel1.Visible = True
		  
		  TMUpdate.RunMode = Timer.RunModes.Multiple
		  
		  myThread.Priority = 10
		  myThread.run
		  
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CAGraphMain
	#tag Event
		Sub DrawContent()
		  me.CurveStart(τ(GetLowerBound), y(GetLowerBound))
		  For i As Integer = GetLowerBound + 1 To GetUpperBound
		    me.CurveTo(τ(i), y(i))
		  Next 
		End Sub
	#tag EndEvent
	#tag Event
		Sub Open()
		  Me.SetTitle("Graph of " +PMGraphMain.RowValueAt(PMGraphMain.SelectedRowIndex) + " vs time " )'
		  Me.SetXLabel("t (in seconds)")
		  Me.SetYLabel(PMGraphMain.RowValueAt(PMGraphMain.SelectedRowIndex))
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BUGraphMain
	#tag Event
		Sub Pressed()
		  Graph
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events myThread
	#tag Event
		Sub Run()
		  CreateArrays
		  
		  myThread.AddUserInterfaceUpdate
		End Sub
	#tag EndEvent
	#tag Event
		Sub UserInterfaceUpdate(data() as Dictionary)
		  PopulateUncertainties(myMain.ATA)
		  
		  MessageBox("All done")
		  
		  BUGraphMain.Enabled = True
		  
		  ProgressWheel1.Visible = False
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BUMatrix
	#tag Event
		Sub Pressed()
		  MatrixWindow.Show
		  MatrixWindow.originalMatrix = originalMatrix
		  MatrixWindow.normalizedMatrix = normalizedMatrix
		  MatrixWindow.arrangedMatrix = arrangedMatrix
		  MatrixWindow.invertedMatrix = invertedMatrix
		  MatrixWindow.unarrangedMatrix = unarrangedMatrix
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events TMUpdate
	#tag Event
		Sub Action()
		  if MyThread.State = Thread.Running then  // if the thread is running
		    PGBProgress.value = myMain.UpdateProgress  // show progress on current case
		  else // the thread has stopped, meaning all cases are done
		    PGBProgress.value = 0 //  so reset progress bar
		    me.RunMode = Timer.RunModes.Off // and we need no more updates
		  end if
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events LBOutputValues
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  LBInputValues.CellTypeAt(row,column) = DesktopListBox.CellTypes.TextField
		End Function
	#tag EndEvent
	#tag Event
		Function MouseWheel(x As Integer, y As Integer, deltaX As Integer, deltaY As Integer) As Boolean
		  return True
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events LBInputValues
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  LBInputValues.CellTypeAt(row,column) = DesktopListBox.CellTypes.TextField
		End Function
	#tag EndEvent
	#tag Event
		Function MouseWheel(x As Integer, y As Integer, deltaX As Integer, deltaY As Integer) As Boolean
		  return True
		End Function
	#tag EndEvent
	#tag Event
		Function CellKeyDown(row as Integer, column as Integer, key as String) As Boolean
		  If key = chr(13) then
		    LBInputValues.CellTypeAt(row+1,column) = DesktopListBox.CellTypes.TextField
		    LBInputValues.EditCellAt(row+1,column) 
		    return True
		  end if 
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox1
	#tag Event
		Sub ValueChanged()
		  If CheckBox1.Value Then
		    LBInputValues.CellTextAt(10, CaseCounter) = "*" + LBInputValues.CellTextAt(10, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(10, CaseCounter) = LBInputValues.CellTextAt(10, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox2
	#tag Event
		Sub ValueChanged()
		  If CheckBox2.Value Then
		    LBInputValues.CellTextAt(11, CaseCounter) = "*" + LBInputValues.CellTextAt(11, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(11, CaseCounter) = LBInputValues.CellTextAt(11, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox3
	#tag Event
		Sub ValueChanged()
		  If CheckBox3.Value Then
		    LBInputValues.CellTextAt(12, CaseCounter) = "*" + LBInputValues.CellTextAt(12, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(12, CaseCounter) = LBInputValues.CellTextAt(12, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox4
	#tag Event
		Sub ValueChanged()
		  If CheckBox4.Value Then
		    LBInputValues.CellTextAt(13, CaseCounter) = "*" + LBInputValues.CellTextAt(13, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(13, CaseCounter) = LBInputValues.CellTextAt(13, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox5
	#tag Event
		Sub ValueChanged()
		  If CheckBox5.Value Then
		    LBInputValues.CellTextAt(14, CaseCounter) = "*" + LBInputValues.CellTextAt(14, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(14, CaseCounter) = LBInputValues.CellTextAt(14, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox6
	#tag Event
		Sub ValueChanged()
		  If CheckBox6.Value Then
		    LBInputValues.CellTextAt(15, CaseCounter) = "*" + LBInputValues.CellTextAt(15, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(15, CaseCounter) = LBInputValues.CellTextAt(15, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox7
	#tag Event
		Sub ValueChanged()
		  If CheckBox7.Value Then
		    LBInputValues.CellTextAt(9, CaseCounter) = "*" + LBInputValues.CellTextAt(9, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(9, CaseCounter) = LBInputValues.CellTextAt(9, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox8
	#tag Event
		Sub ValueChanged()
		  If CheckBox8.Value Then
		    LBInputValues.CellTextAt(8, CaseCounter) = "*" + LBInputValues.CellTextAt(8, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(8, CaseCounter) = LBInputValues.CellTextAt(8, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox9
	#tag Event
		Sub ValueChanged()
		  If CheckBox9.Value Then
		    LBInputValues.CellTextAt(7, CaseCounter) = "*" + LBInputValues.CellTextAt(7, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(7, CaseCounter) = LBInputValues.CellTextAt(7, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox10
	#tag Event
		Sub ValueChanged()
		  If CheckBox10.Value Then
		    LBInputValues.CellTextAt(6, CaseCounter) = "*" + LBInputValues.CellTextAt(6, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(6, CaseCounter) = LBInputValues.CellTextAt(6, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox11
	#tag Event
		Sub ValueChanged()
		  If CheckBox11.Value Then
		    LBInputValues.CellTextAt(5, CaseCounter) = "*" + LBInputValues.CellTextAt(5, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(5, CaseCounter) = LBInputValues.CellTextAt(5, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox12
	#tag Event
		Sub ValueChanged()
		  If CheckBox12.Value Then
		    LBInputValues.CellTextAt(4, CaseCounter) = "*" + LBInputValues.CellTextAt(4, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(4, CaseCounter) = LBInputValues.CellTextAt(4, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox13
	#tag Event
		Sub ValueChanged()
		  If CheckBox13.Value Then
		    LBInputValues.CellTextAt(3, CaseCounter) = "*" + LBInputValues.CellTextAt(3, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(3, CaseCounter) = LBInputValues.CellTextAt(3, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox14
	#tag Event
		Sub ValueChanged()
		  If CheckBox14.Value Then
		    LBInputValues.CellTextAt(2, CaseCounter) = "*" + LBInputValues.CellTextAt(2, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(2, CaseCounter) = LBInputValues.CellTextAt(2, CaseCounter).Replace("*", "")
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CheckBox15
	#tag Event
		Sub ValueChanged()
		  If CheckBox15.Value Then
		    LBInputValues.CellTextAt(1, CaseCounter) = "*" + LBInputValues.CellTextAt(1, CaseCounter)
		  Else
		    LBInputValues.CellTextAt(1, CaseCounter) = LBInputValues.CellTextAt(1, CaseCounter).Replace("*", "")
		  End If
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
			"9 - Metal Window"
			"11 - Modeless Dialog"
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
		Name="M"
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
		Name="χ1ℓ"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="ψangle"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="χ2ℓ"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="CaseCounter"
		Visible=false
		Group="Behavior"
		InitialValue="0"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="v0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="β"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="λ0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Θ"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Φ"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="dτ0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="α0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
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
		Name="Detectors"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="str"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="z0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="τMainN"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="τMainF"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="GraphAllowed"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="GraphedYet"
		Visible=false
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="f0"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="R"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="v"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="K"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="arrangedMatrix"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="invertedMatrix"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="normalizedMatrix"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="originalMatrix"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="unarrangedMatrix"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
#tag EndViewBehavior
