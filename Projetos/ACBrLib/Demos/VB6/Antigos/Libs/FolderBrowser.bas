Attribute VB_Name = "FolderBrowser"
Option Explicit

Private Type BROWSEINFO
  hOwner As Long
  pidlRoot As Long
  pszDisplayName As String
  lpszTitle As String
  ulFlags As Long
  lpfn As Long
  lParam As Long
  iImage As Long
End Type

Private Declare Function SHGetPathFromIDList Lib "shell32.dll" Alias _
            "SHGetPathFromIDListA" (ByVal pidl As Long, _
            ByVal pszPath As String) As Long
            
Private Declare Function SHBrowseForFolder Lib "shell32.dll" Alias _
            "SHBrowseForFolderA" (lpBrowseInfo As BROWSEINFO) _
            As Long
            
Private Const BIF_RETURNONLYFSDIRS = &H1

Public Function BrowseFolder(szDialogTitle As String) As String
  Dim x As Long, bi As BROWSEINFO, dwIList As Long
  Dim szPath As String, wPos As Integer
  
    With bi
        .hOwner = 0
        .lpszTitle = szDialogTitle
        .ulFlags = BIF_RETURNONLYFSDIRS
    End With
    
    dwIList = SHBrowseForFolder(bi)
    szPath = Space$(512)
    x = SHGetPathFromIDList(ByVal dwIList, ByVal szPath)
    
    If x Then
        wPos = InStr(szPath, Chr(0))
        BrowseFolder = Left$(szPath, wPos - 1)
    Else
        BrowseFolder = ""
    End If
End Function

