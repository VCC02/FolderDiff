{
    Copyright (C) 2024 VCC
    creation date: 07 Jun 2024
    initial release date: 18 Jul 2024

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit FolderDiffMainForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, VirtualTrees, ImgList, Menus, AsyncProcess;

type
  TDiffStatus = (dsLeftOlderThanRight, dsSameAs, dsLeftNewerThanRight, dsUnknown, dsSameTimestampDiffContent);     //dsSameTimestampDiffContent is an unusual case were the files can be different, but some app modified the timestamp(s) to match.
  TExistenceStatus = (esLeftOnly, esBoth, esRightOnly);
  TFileFolderType = (ffFolder, ffFile);

  TNodeData = record
    Fnm: string;
    PathLeft, PathRight: string;
    SizeLeft, SizeRight: Int64;
    TimeStampLeft, TimeStampRight: Double;
    DiffStatus: TDiffStatus;     //////////////////////
    ExistenceStatus: TExistenceStatus;
    FileFolderType: TFileFolderType;
  end;

  PNodeData = ^TNodeData;

  TExistenceItem = record
    Fnm: string;
    ExistenceStatus: TExistenceStatus;
    FileFolderType: TFileFolderType;
  end;

  TExistenceItemArr = array of TExistenceItem;

  TSyncDir = (sdLeftToRight, sdRightToLeft);


  { TfrmFolderDiffMain }

  TfrmFolderDiffMain = class(TForm)
    AsyncProcess1: TAsyncProcess;
    chkFullFolderLoading: TCheckBox;
    chkSearchFilename: TCheckBox;
    cmbFilter: TComboBox;
    edtSearch: TEdit;
    edtLeft: TEdit;
    edtRight: TEdit;
    imglstVST: TImageList;
    lblFilter: TLabel;
    MenuItem_Recent: TMenuItem;
    Separator1: TMenuItem;
    MenuItem_RemoveCurrentPathsFromRecent: TMenuItem;
    MenuItem_AddToRecent: TMenuItem;
    MenuItem_ShowInExplorer: TMenuItem;
    MenuItem_OpenInEditMode: TMenuItem;
    MenuItem_CloseActiveTab: TMenuItem;
    PageControlMain: TPageControl;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pmTabs: TPopupMenu;
    pmRecent: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    spdbtnDiff: TSpeedButton;
    spdbtnLeft: TSpeedButton;
    spdbtnRight: TSpeedButton;
    spdbtnStopLoading: TSpeedButton;
    spdbtnSwitchLeftAndRight: TSpeedButton;
    spdbtnSyncToLeft: TSpeedButton;
    spdbtnSyncToRight: TSpeedButton;
    spdbtnStopSyncing: TSpeedButton;
    StatusBar1: TStatusBar;
    TabSheetFolderDiff: TTabSheet;
    tmrSearch: TTimer;
    tmrStartup: TTimer;
    vstDiff: TVirtualStringTree;
    procedure chkSearchFilenameChange(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
    procedure edtLeftKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edtRightKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_AddToRecentClick(Sender: TObject);
    procedure MenuItem_CloseActiveTabClick(Sender: TObject);
    procedure MenuItem_OpenInEditModeClick(Sender: TObject);
    procedure MenuItem_RemoveCurrentPathsFromRecentClick(Sender: TObject);
    procedure MenuItem_ShowInExplorerClick(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
    procedure spdbtnDiffClick(Sender: TObject);
    procedure spdbtnLeftClick(Sender: TObject);
    procedure spdbtnRightClick(Sender: TObject);
    procedure spdbtnStopLoadingClick(Sender: TObject);
    procedure spdbtnStopSyncingClick(Sender: TObject);
    procedure spdbtnSwitchLeftAndRightClick(Sender: TObject);
    procedure spdbtnSyncToLeftClick(Sender: TObject);
    procedure spdbtnSyncToRightClick(Sender: TObject);
    procedure TabSheetFolderDiffResize(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstDiffAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure vstDiffBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstDiffClick(Sender: TObject);
    procedure vstDiffDblClick(Sender: TObject);
    procedure vstDiffDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: string;
      const CellRect: TRect; var DefaultDraw: boolean);
    procedure vstDiffExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstDiffGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDiffGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstDiffKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstDiffMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstDiffPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FManuallyStopLoading: Boolean;
    FStopSyncing: Boolean;
    FFormatSettings: TFormatSettings;
    FActionsHitInfo: THitInfo;

    FRecentPathsLeft, FRecentPathsRight: TStringList;

    procedure AddAllFoldersAndFilesFromFolder(ABaseNode: PVirtualNode; ABasePathLeft, ABasePathRight: string; ALoadFileDetails, AFullLoading: Boolean; ARecursionLevel: Integer);
    procedure Compare;
    procedure FilterComparison;
    procedure SyncFiles(ASyncDir: TSyncDir);
    procedure CreateNewDiffTab;
    procedure CreateNewTextEditorTab(ALeftFileName, ARightFileName: string);

    procedure FillInRecent;
    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure HandleOnCreateNewTextEditorFrame(ALeftFileName, ARightFileName: string);
    procedure DisplayFirstSelectedFileInStatusBar;
    procedure HandleOnRecentClick(Sender: TObject);
  public

  end;


const
  CDiffStatusStr: array[TDiffStatus] of string = ('<', '=', '>', '?', '<>');
  CSyncDirStr: array[TSyncDir] of string = ('left to right', 'right to left');
  CLeftRightMenuSpace = '  ';

var
  frmFolderDiffMain: TfrmFolderDiffMain;

{ToDo
- Add log
[in work] - Add splitters
- bug??? - after searching for a file and clearing the editbox, the files are not displayed anymore, only the folders
- MenuItem_OpenInEditModeClick should open based on what is the focued tab (main folder diff, or a file diff)
}


implementation

{$R *.frm}


uses
  FileDiffFrame, FileDiffUtils, FileEditFrame,
  IniFiles, Math;


{ TfrmFolderDiffMain }


procedure GetFolderContentLists(APath: string; ADestFoldersList, ADestFilesList: TStringList);
var
  Res: Integer;
  SrcRec: TSearchRec;
begin
  Res := FindFirst(APath + PathDelim + '*', faAnyFile, SrcRec);
  try
    if Res = 0 then
      if (SrcRec.Name <> '.') and (SrcRec.Name <> '..') then
        if SrcRec.IsDirectory then
          ADestFoldersList.Add(SrcRec.Name)
        else
          ADestFilesList.Add(SrcRec.Name);

    while Res = 0 do
    begin
      Res := FindNext(SrcRec);

      if Res = 0 then
        if (SrcRec.Name <> '.') and (SrcRec.Name <> '..') then
          if SrcRec.IsDirectory then
            ADestFoldersList.Add(SrcRec.Name)
          else
            ADestFilesList.Add(SrcRec.Name);
    end;
  finally
    FindClose(SrcRec);
  end;

  ADestFoldersList.Sort;
  ADestFilesList.Sort;
end;


procedure GetFolderLists(ALeftPath, ARightPath: string; ADestLeftFoldersList, ADestRightFoldersList, ADestLeftFilesList, ADestRightFilesList: TStringList);
begin
  GetFolderContentLists(ALeftPath, ADestLeftFoldersList, ADestLeftFilesList);
  GetFolderContentLists(ARightPath, ADestRightFoldersList, ADestRightFilesList);
end;


function IndexOfFileInExistenceItem(var ADestExistenceItems: TExistenceItemArr; AFnm: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(ADestExistenceItems) - 1 downto 0 do
    if ADestExistenceItems[i].Fnm = AFnm then
    begin
      Result := i;
      Exit;
    end;
end;


procedure BuildArrayOfFoldersAndFiles(ALeftFoldersList, ARightFoldersList, ALeftFilesList, ARightFilesList: TStringList; var ADestExistenceItems: TExistenceItemArr);
var
  i, n: Integer;
begin
  ALeftFoldersList.Sort;
  ARightFoldersList.Sort;
  ALeftFilesList.Sort;
  ARightFilesList.Sort;

  //Folders
  for i := 0 to ALeftFoldersList.Count - 1 do
    if IndexOfFileInExistenceItem(ADestExistenceItems, ALeftFoldersList.Strings[i]) = -1 then
    begin
      n := Length(ADestExistenceItems);
      SetLength(ADestExistenceItems, n + 1);

      ADestExistenceItems[n].Fnm := ALeftFoldersList.Strings[i];
      ADestExistenceItems[n].FileFolderType := ffFolder;         //Folder

      if ARightFoldersList.IndexOf(ADestExistenceItems[n].Fnm) = -1 then  ///////////////////////// replace <List>.IndexOf with a temp list of uppercase  - windows only
        ADestExistenceItems[n].ExistenceStatus := esLeftOnly
      else
        ADestExistenceItems[n].ExistenceStatus := esBoth;
    end;

  for i := 0 to ARightFoldersList.Count - 1 do
    if IndexOfFileInExistenceItem(ADestExistenceItems, ARightFoldersList.Strings[i]) = -1 then
    begin
      n := Length(ADestExistenceItems);
      SetLength(ADestExistenceItems, n + 1);

      ADestExistenceItems[n].Fnm := ARightFoldersList.Strings[i];
      ADestExistenceItems[n].FileFolderType := ffFolder;         //Folder

      if ALeftFoldersList.IndexOf(ADestExistenceItems[n].Fnm) = -1 then  ///////////////////////// replace <List>.IndexOf with a temp list of uppercase  - windows only
        ADestExistenceItems[n].ExistenceStatus := esRightOnly
      else
        ADestExistenceItems[n].ExistenceStatus := esBoth;
    end;

  //Files
  for i := 0 to ALeftFilesList.Count - 1 do
    if IndexOfFileInExistenceItem(ADestExistenceItems, ALeftFilesList.Strings[i]) = -1 then
    begin
      n := Length(ADestExistenceItems);
      SetLength(ADestExistenceItems, n + 1);

      ADestExistenceItems[n].Fnm := ALeftFilesList.Strings[i];
      ADestExistenceItems[n].FileFolderType := ffFile;         //File

      if ARightFilesList.IndexOf(ADestExistenceItems[n].Fnm) = -1 then  ///////////////////////// replace <List>.IndexOf with a temp list of uppercase  - windows only
        ADestExistenceItems[n].ExistenceStatus := esLeftOnly
      else
        ADestExistenceItems[n].ExistenceStatus := esBoth;
    end;

  for i := 0 to ARightFilesList.Count - 1 do
    if IndexOfFileInExistenceItem(ADestExistenceItems, ARightFilesList.Strings[i]) = -1 then
    begin
      n := Length(ADestExistenceItems);
      SetLength(ADestExistenceItems, n + 1);

      ADestExistenceItems[n].Fnm := ARightFilesList.Strings[i];
      ADestExistenceItems[n].FileFolderType := ffFile;         //File

      if ALeftFilesList.IndexOf(ADestExistenceItems[n].Fnm) = -1 then  ///////////////////////// replace <List>.IndexOf with a temp list of uppercase  - windows only
        ADestExistenceItems[n].ExistenceStatus := esRightOnly
      else
        ADestExistenceItems[n].ExistenceStatus := esBoth;
    end;
end;


function GetFileSize(AFileName: string): Int64;
var
  TempStream: TFileStream;
begin
  try
    TempStream := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyNone);
    try
      Result := TempStream.Size;
    finally
      TempStream.Free;
    end;
  except
    Result := -1;
  end;
end;


procedure TfrmFolderDiffMain.FillInRecent;
var
  i: Integer;
  TempMenuItem: TMenuItem;
begin
  MenuItem_Recent.Clear;

  for i := 0 to FRecentPathsLeft.Count - 1 do
  begin
    TempMenuItem := TMenuItem.Create(Self);
    TempMenuItem.Caption := FRecentPathsLeft.Strings[i] + CLeftRightMenuSpace + FRecentPathsRight.Strings[i];
    TempMenuItem.OnClick := @HandleOnRecentClick;

    MenuItem_Recent.Add(TempMenuItem);
  end;
end;


procedure TfrmFolderDiffMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
  i, n: Integer;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FolderDiff.ini');
  try
    Left := Ini.ReadInteger('MainWindow', 'Left', Left);
    Top := Ini.ReadInteger('MainWindow', 'Top', Top);
    Width := Ini.ReadInteger('MainWindow', 'Width', Width);
    Height := Ini.ReadInteger('MainWindow', 'Height', Height);

    cmbFilter.ItemIndex := Max(0, Min(Ini.ReadInteger('Settings', 'DiffFilter', 0), cmbFilter.Items.Count - 1));

    n := Ini.ReadInteger('Settings', 'RecentCount', 0);
    for i := 0 to n - 1 do
    begin
      FRecentPathsLeft.Add(Ini.ReadString('Settings', 'RecentLeft_' + IntToStr(i), ''));
      FRecentPathsRight.Add(Ini.ReadString('Settings', 'RecentRight_' + IntToStr(i), ''));
    end;

    FillInRecent;
  finally
    Ini.Free;
  end;
end;


procedure TfrmFolderDiffMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
  i: Integer;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FolderDiff.ini');
  try
    Ini.WriteInteger('MainWindow', 'Left', Left);
    Ini.WriteInteger('MainWindow', 'Top', Top);
    Ini.WriteInteger('MainWindow', 'Width', Width);
    Ini.WriteInteger('MainWindow', 'Height', Height);

    Ini.WriteInteger('Settings', 'DiffFilter', cmbFilter.ItemIndex);

    Ini.WriteInteger('Settings', 'RecentCount', FRecentPathsLeft.Count);
    for i := 0 to FRecentPathsLeft.Count - 1 do
    begin
      Ini.WriteString('Settings', 'RecentLeft_' + IntToStr(i), FRecentPathsLeft.Strings[i]);
      Ini.WriteString('Settings', 'RecentRight_' + IntToStr(i), FRecentPathsRight.Strings[i]);
    end;

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmFolderDiffMain.AddAllFoldersAndFilesFromFolder(ABaseNode: PVirtualNode; ABasePathLeft, ABasePathRight: string; ALoadFileDetails, AFullLoading: Boolean; ARecursionLevel: Integer);
var
  LeftFoldersList, RightFoldersList, LeftFilesList, RightFilesList: TStringList;
  i: Integer;

  NodeData: PNodeData;
  ExistenceItems: TExistenceItemArr;

  RecentNode: PVirtualNode;
begin
  if (ABasePathLeft = '') or (ABasePathRight = '') then
    Exit;

  LeftFoldersList := TStringList.Create;
  RightFoldersList := TStringList.Create;
  LeftFilesList := TStringList.Create;
  RightFilesList := TStringList.Create;
  try
    GetFolderLists(ABasePathLeft, ABasePathRight, LeftFoldersList, RightFoldersList, LeftFilesList, RightFilesList);
    BuildArrayOfFoldersAndFiles(LeftFoldersList, RightFoldersList, LeftFilesList, RightFilesList, ExistenceItems);
  finally
    LeftFoldersList.Free;
    RightFoldersList.Free;
    LeftFilesList.Free;
    RightFilesList.Free;
  end;

  for i := 0 to Length(ExistenceItems) - 1 do
  begin
    RecentNode := vstDiff.AddChild(ABaseNode);

    NodeData := vstDiff.GetNodeData(RecentNode);
    NodeData^.Fnm := ExistenceItems[i].Fnm;

    NodeData^.PathLeft := ABasePathLeft + PathDelim + NodeData^.Fnm;
    NodeData^.PathRight := ABasePathRight + PathDelim + NodeData^.Fnm;

    if ALoadFileDetails then
    begin
      NodeData^.SizeLeft := GetFileSize(NodeData^.PathLeft);
      NodeData^.SizeRight := GetFileSize(NodeData^.PathRight);
    end
    else
    begin
      NodeData^.SizeLeft := -1;
      NodeData^.SizeRight := -1;
    end;

    NodeData^.FileFolderType := ExistenceItems[i].FileFolderType;
    NodeData^.ExistenceStatus := ExistenceItems[i].ExistenceStatus;
    NodeData^.DiffStatus := dsUnknown;

    try
      if FileExists(NodeData^.PathLeft) then
      begin
        if NodeData^.FileFolderType = ffFile then
          NodeData^.TimeStampLeft := FileDateToDateTime(FileAge(NodeData^.PathLeft))
        else
          NodeData^.TimeStampLeft := MaxInt;
      end
      else
        NodeData^.TimeStampLeft := 0;
    except
      NodeData^.TimeStampLeft := 0;
    end;

    try
      if FileExists(NodeData^.PathRight) then
      begin
        if NodeData^.FileFolderType = ffFile then
          NodeData^.TimeStampRight := FileDateToDateTime(FileAge(NodeData^.PathRight))
        else
          NodeData^.TimeStampRight := MaxInt;
      end
      else
        NodeData^.TimeStampRight := 0;
    except
      NodeData^.TimeStampRight := 0;
    end;

    if FileExists(NodeData^.PathLeft) and FileExists(NodeData^.PathRight) then
    begin
      if NodeData^.TimeStampLeft = NodeData^.TimeStampRight then
        NodeData^.DiffStatus := dsSameAs
      else
        if NodeData^.TimeStampLeft < NodeData^.TimeStampRight then
          NodeData^.DiffStatus := dsLeftOlderThanRight
        else
          NodeData^.DiffStatus := dsLeftNewerThanRight;
    end;
  end;

  if not AFullLoading then
    if ARecursionLevel > 0 then
      Exit;

  RecentNode := vstDiff.GetFirstChild(ABaseNode);
  if RecentNode = nil then
    Exit;

  FManuallyStopLoading := False;
  vstDiff.Color := clDefault;
  vstDiff.Hint := '';
  repeat
    Application.ProcessMessages;
    if FManuallyStopLoading then
    begin
      vstDiff.Color := $EEEEFF;
      vstDiff.Hint := 'Manually stopped loading.';
      Break;
    end;

    NodeData := vstDiff.GetNodeData(RecentNode);
    if NodeData^.FileFolderType = ffFolder then
      AddAllFoldersAndFilesFromFolder(RecentNode, NodeData^.PathLeft, NodeData^.PathRight, False, AFullLoading, ARecursionLevel + 1);

    RecentNode := RecentNode^.NextSibling;
  until RecentNode = nil;
end;


procedure TfrmFolderDiffMain.Compare;
var
  BasePathLeft, BasePathRight: string;
begin
  edtLeft.Enabled := False;
  edtRight.Enabled := False;
  spdbtnStopLoading.Show;
  try
    vstDiff.Clear;

    BasePathLeft := edtLeft.Text;
    BasePathRight := edtRight.Text;

    if BasePathLeft <> '' then
      if BasePathLeft[Length(BasePathLeft)] = PathDelim then
        Delete(BasePathLeft, Length(BasePathLeft), 1);

    if BasePathRight <> '' then
      if BasePathRight[Length(BasePathRight)] = PathDelim then
        Delete(BasePathRight, Length(BasePathRight), 1);

    Application.ProcessMessages;
    AddAllFoldersAndFilesFromFolder(vstDiff.RootNode, BasePathLeft, BasePathRight, True, chkFullFolderLoading.Checked, 0);
    vstDiff.Repaint;
  finally
    edtLeft.Enabled := True;
    edtRight.Enabled := True;
    spdbtnStopLoading.Hide;
  end;
end;


procedure TfrmFolderDiffMain.FilterComparison;
var
  Node, WorkNode: PVirtualNode;
  NodeData: PNodeData;
  NodeIsVisible: Boolean;
  AllVisibleFromSelection: Boolean;
  NodeVisibleFromSearchText, UsingSearchText: Boolean;
  SearchText: string;
begin
  Node := vstDiff.GetFirst;
  if Node = nil then
    Exit;

  AllVisibleFromSelection := cmbFilter.ItemIndex = 0;

  UsingSearchText := chkSearchFilename.Checked;
  SearchText := UpperCase(edtSearch.Text);

  repeat
    NodeData := vstDiff.GetNodeData(Node);
    if NodeData = nil then
      Exit;

    NodeVisibleFromSearchText := ((SearchText = '') or (Pos(SearchText, UpperCase(NodeData^.Fnm)) > 0)) or not UsingSearchText;
    NodeIsVisible := (AllVisibleFromSelection or (NodeData^.DiffStatus <> dsSameAs)) and NodeVisibleFromSearchText;

    vstDiff.IsVisible[Node] := NodeIsVisible;

    Node := vstDiff.GetNext(Node);
  until Node = nil;

  vstDiff.RootNode^.Parent := nil;
  Node := vstDiff.GetLast;
  repeat
    WorkNode := Node;
    while (WorkNode^.Parent <> nil) and vstDiff.IsVisible[WorkNode] do
    begin
      vstDiff.IsVisible[WorkNode^.Parent] := True;
      WorkNode := WorkNode^.Parent;
    end;

    Node := vstDiff.GetPrevious(Node);
  until Node = nil;

  vstDiff.Repaint;
end;


procedure TfrmFolderDiffMain.vstDiffGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: integer);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if not (Column in [0, 4]) then
    Exit;

  if ((Column = 0) and (NodeData^.ExistenceStatus in [esLeftOnly, esBoth])) or
    ((Column = 4) and (NodeData^.ExistenceStatus in [esRightOnly, esBoth])) then
  begin
    if NodeData^.FileFolderType = ffFolder then
      ImageIndex := 0
    else
      ImageIndex := 1;


    if Column = 4 then
    begin
      ImageIndex := -1;

    end;
  end;
end;


procedure TfrmFolderDiffMain.vstDiffKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Compare;

  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    DisplayFirstSelectedFileInStatusBar;
end;


procedure TfrmFolderDiffMain.vstDiffMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstDiff.GetHitTestInfoAt(X, Y, True, FActionsHitInfo);
end;


procedure TfrmFolderDiffMain.DisplayFirstSelectedFileInStatusBar;
var
  Node: PVirtualNode;
  NodeData: PNodeData;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstDiff.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  StatusBar1.Panels.Items[0].Text := NodeData^.PathLeft;
  StatusBar1.Panels.Items[1].Text := NodeData^.PathRight;
end;


procedure TfrmFolderDiffMain.vstDiffPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  //if ((Column = 0) {and (NodeData^.ExistenceStatus in [esLeftOnly, esBoth])}) or
  //  ((Column = 4) {and (NodeData^.ExistenceStatus in [esRightOnly, esBoth])}) then

  case NodeData^.DiffStatus of
    dsLeftNewerThanRight: //if NodeData^.TimeStampLeft > NodeData^.TimeStampRight then
    begin
      if Column = 0 then
        TargetCanvas.Font.Color := clTeal;

      if Column = 4 then
        TargetCanvas.Font.Color := clRed;
    end;

    dsLeftOlderThanRight: //if NodeData^.TimeStampLeft < NodeData^.TimeStampRight then
    begin
      if Column = 0 then
        TargetCanvas.Font.Color := clRed;

      if Column = 4 then
        TargetCanvas.Font.Color := clTeal;
    end;

    dsSameTimestampDiffContent:
    begin
      if Column = 0 then
        TargetCanvas.Font.Color := clRed;

      if Column = 4 then
        TargetCanvas.Font.Color := clRed;
    end;

    dsUnknown:
    begin
      if Column = 0 then
        TargetCanvas.Font.Color := clGray;

      if Column = 4 then
        TargetCanvas.Font.Color := clGray;
    end;

    dsSameAs:
    begin
      //if Column = 0 then
      //  TargetCanvas.Font.Color := clWindowText;
      //
      //if Column = 4 then
      //  TargetCanvas.Font.Color := clWindowText;
    end;

  end; //case
end;


procedure TfrmFolderDiffMain.edtLeftKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Compare;
end;


procedure TfrmFolderDiffMain.cmbFilterChange(Sender: TObject);
begin
  FilterComparison;
end;


procedure TfrmFolderDiffMain.chkSearchFilenameChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;

  if chkSearchFilename.Checked then
    vstDiff.Height := edtSearch.Top - edtLeft.Top - edtLeft.Height - 56
  else
    vstDiff.Height := edtSearch.Top - edtLeft.Top {- edtLeft.Height} - 52;
end;


procedure TfrmFolderDiffMain.edtRightKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Compare;
end;


procedure TfrmFolderDiffMain.edtSearchChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;
end;


procedure TfrmFolderDiffMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    SaveSettingsToIni;
  except
  end;

  FRecentPathsLeft.Free;
  FRecentPathsRight.Free;
end;


procedure TfrmFolderDiffMain.FormCreate(Sender: TObject);
begin
  vstDiff.NodeDataSize := SizeOf(TNodeData);
  FRecentPathsLeft := TStringList.Create;
  FRecentPathsRight := TStringList.Create;

  tmrStartup.Enabled := True;

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.LongDateFormat := 'YYYY.MM.DD';
  FFormatSettings.LongTimeFormat := 'HH:MM:SS';

  FActionsHitInfo.HitColumn := -1;
end;


procedure TfrmFolderDiffMain.HandleOnRecentClick(Sender: TObject);
var
  LeftPath, RightPath, s: string;
begin
  s := (Sender as TMenuItem).Caption;
  s := StringReplace(s, '&', '', [rfReplaceAll]);

  LeftPath := Copy(s, 1, Pos(CLeftRightMenuSpace, s) - 1);
  RightPath := Copy(s, Pos(CLeftRightMenuSpace, s) + Length(CLeftRightMenuSpace), MaxInt);
  edtLeft.Text := LeftPath;
  edtRight.Text := RightPath;
  Compare;
end;


procedure TfrmFolderDiffMain.MenuItem_AddToRecentClick(Sender: TObject);
begin
  FRecentPathsLeft.Add(edtLeft.Text);
  FRecentPathsRight.Add(edtRight.Text);

  FillInRecent;
end;


procedure TfrmFolderDiffMain.MenuItem_RemoveCurrentPathsFromRecentClick(
  Sender: TObject);
var
  i, Idx: Integer;
  s: string;
begin
  Idx := -1;
  for i := 0 to FRecentPathsLeft.Count - 1 do
    if (FRecentPathsLeft.Strings[i] = edtLeft.Text) and
       (FRecentPathsRight.Strings[i] = edtRight.Text) then
    begin
      Idx := i;
      Break;
    end;

  if Idx = -1 then
  begin
    MessageBox(Handle, 'The current paths are not in the list of recent paths.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  for i := 0 to MenuItem_Recent.Count - 1 do
  begin
    s := MenuItem_Recent.Items[i].Caption;
    s := StringReplace(s, '&', '', [rfReplaceAll]);

    if s = edtLeft.Text + CLeftRightMenuSpace + edtRight.Text then
    begin
      MenuItem_Recent.Delete(i);
      FRecentPathsLeft.Delete(Idx);
      FRecentPathsRight.Delete(Idx);

      Break;
    end;
  end;

  FillInRecent;
end;


procedure TfrmFolderDiffMain.MenuItem_CloseActiveTabClick(Sender: TObject);
begin
  if PageControlMain.ActivePageIndex <= 0 then
    Exit;

  PageControlMain.Pages[PageControlMain.ActivePageIndex].Destroy;
end;


procedure TfrmFolderDiffMain.MenuItem_OpenInEditModeClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PNodeData;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstDiff.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.FileFolderType = ffFolder then
    Exit;

  CreateNewTextEditorTab(NodeData^.PathLeft, NodeData^.PathRight);
end;


procedure TfrmFolderDiffMain.MenuItem_ShowInExplorerClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PNodeData;
  PathToFileToBeSelected: string;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstDiff.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  PathToFileToBeSelected := '';

  if FActionsHitInfo.HitColumn in [0, 1, 2] then
    PathToFileToBeSelected := NodeData^.PathLeft
  else
    PathToFileToBeSelected := NodeData^.PathRight;

  {$IFDEF Windows}
    PathToFileToBeSelected := '"' + PathToFileToBeSelected + '"';

    AsyncProcess1.Executable := 'Explorer.exe';
    AsyncProcess1.Parameters.Add('/select,' + PathToFileToBeSelected);
  {$ELSE}
    //PathToFileToBeSelected := '"' + PathToFileToBeSelected + '"';//it doesn't want double quotes on Linux

    AsyncProcess1.Executable := 'dolphin';
    AsyncProcess1.Parameters.Add('--select');
    AsyncProcess1.Parameters.Add(PathToFileToBeSelected);
  {$ENDIF}

  AsyncProcess1.Execute;
end;


procedure TfrmFolderDiffMain.PageControlMainChange(Sender: TObject);
begin

end;


function GetFileComparisonStatus(ALeftFileName, ARightFileName: string): TDiffStatus;
  procedure SetResultByTimestamp;
  begin
    if FileAge(ALeftFileName) < FileAge(ARightFileName) then
      Result := dsLeftOlderThanRight
    else
      if FileAge(ALeftFileName) > FileAge(ARightFileName) then
        Result := dsLeftNewerThanRight
      else
        if FileAge(ALeftFileName) = FileAge(ARightFileName) then
          Result := dsSameTimestampDiffContent;
  end;

var
  LeftStream, RightStream: TMemoryStream;
begin
  Result := dsUnknown;

  if not FileExists(ALeftFileName) or not FileExists(ARightFileName) then
    Exit;

  LeftStream := TMemoryStream.Create;
  RightStream := TMemoryStream.Create;
  try
    LeftStream.LoadFromFile(ALeftFileName);
    RightStream.LoadFromFile(ARightFileName);

    if LeftStream.Size <> RightStream.Size then
    begin
      SetResultByTimestamp;
      Exit;
    end
    else
    begin
      if CompareMem(LeftStream.Memory, RightStream.Memory, LeftStream.Size) then
        Result := dsSameAs
      else
        SetResultByTimestamp;
    end;
  finally
    LeftStream.Free;
    RightStream.Free;
  end;
end;


procedure TfrmFolderDiffMain.spdbtnDiffClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PNodeData;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
    Exit;

  repeat
    if vstDiff.Selected[Node] then
    begin
      NodeData := vstDiff.GetNodeData(Node);
      if NodeData^.FileFolderType = ffFile then
      begin
        NodeData^.DiffStatus := GetFileComparisonStatus(NodeData^.PathLeft, NodeData^.PathRight);
        vstDiff.RepaintNode(Node);
      end;
    end;

    Node := vstDiff.GetNext(Node);
  until Node = nil;

  FilterComparison;
end;


procedure TfrmFolderDiffMain.spdbtnLeftClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := edtLeft.Text;
  if not SelectDirectoryDialog1.Execute then
    Exit;

  edtLeft.Text := SelectDirectoryDialog1.FileName;

  if DirectoryExists(edtRight.Text) then
    Compare;
end;


procedure TfrmFolderDiffMain.spdbtnRightClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := edtRight.Text;
  if not SelectDirectoryDialog1.Execute then
    Exit;

  edtRight.Text := SelectDirectoryDialog1.FileName;

  if DirectoryExists(edtLeft.Text) then
    Compare;
end;


procedure TfrmFolderDiffMain.spdbtnStopLoadingClick(Sender: TObject);
begin
  FManuallyStopLoading := True;
end;


procedure TfrmFolderDiffMain.spdbtnStopSyncingClick(Sender: TObject);
begin
  FStopSyncing := True;
end;


procedure TfrmFolderDiffMain.spdbtnSwitchLeftAndRightClick(Sender: TObject);
var
  TmpPath: string;
begin
  TmpPath := edtLeft.Text;
  edtLeft.Text := edtRight.Text;
  edtRight.Text := TmpPath;

  Compare;
end;


function CreateDirWithSubDirs(ADir: string): Boolean;   //requires absolute paths
var
  ADirTemp, PrevTemp: string;
begin
  Result := True; ///success
  if DirectoryExists(ADir) then
    Exit;

  ADirTemp := ADir;
  repeat
    if ADirTemp = '' then
    begin
      Result := False;
      Exit;
    end;

    if not CreateDir(ADirTemp) then
    begin
      PrevTemp := ADirTemp;
      ADirTemp := ExtractFileDir(ADirTemp);
      if PrevTemp = ADirTemp then
      begin
        Result := False;
        Exit;
      end;
    end
    else
      ADirTemp := ADir;
  until DirectoryExists(ADir);
end;


function CopyFile(ASrcPath, ADstPath: string): Boolean;
var
  SrcStream, DstStream: TFileStream;
begin
  Result := True;
  try
    CreateDirWithSubDirs(ExtractFileDir(ADstPath));

    SrcStream := TFileStream.Create(ASrcPath, fmOpenRead);

    if FileExists(ADstPath) then
    begin
      DstStream := TFileStream.Create(ADstPath, fmOpenWrite);
      try
        DstStream.Size := 0;
      except
      end;
    end
    else
      DstStream := TFileStream.Create(ADstPath, fmCreate or fmOpenWrite);

    try
      SrcStream.Position := 0;
      DstStream.CopyFrom(SrcStream, SrcStream.Size);
    finally
      SrcStream.Free;
      DstStream.Free;
    end;

    FileSetDate(ADstPath, FileAge(ASrcPath));
  except
    //maybe file permission errors or disk rw access errors
    Result := False;
  end;
end;


procedure TfrmFolderDiffMain.SyncFiles(ASyncDir: TSyncDir);
var
  SrcPath, DstPath: string;
  Node: PVirtualNode;
  NodeData: PNodeData;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
  begin
    MessageBoxFunction('No item selected.', PChar(Application.Title), 0);
    Exit;
  end;

  if MessageBox(Handle, PChar('Sync files from ' + CSyncDirStr[ASyncDir] + '?'), PChar(Application.Title), MB_ICONQUESTION) = IDNO then
    Exit;

  FStopSyncing := False;
  spdbtnStopSyncing.Show;
  try
    repeat
      if vstDiff.Selected[Node] then
      begin
        NodeData := vstDiff.GetNodeData(Node);
        if NodeData = nil then
        begin
          MessageBoxFunction('Empty NodeData.', PChar(Application.Title), 0);
          Exit;
        end;

        if ASyncDir = sdLeftToRight then
        begin
          SrcPath := NodeData^.PathLeft;
          DstPath := NodeData^.PathRight;
        end
        else
        begin
          SrcPath := NodeData^.PathRight;
          DstPath := NodeData^.PathLeft;
        end;

        if NodeData^.FileFolderType = ffFile then
        begin
          if CopyFile(SrcPath, DstPath) then  //returns True for success
          begin
            if ASyncDir = sdLeftToRight then
            begin
              NodeData^.TimeStampRight := NodeData^.TimeStampLeft;
              NodeData^.SizeRight := NodeData^.SizeLeft;
            end
            else
            begin
              NodeData^.TimeStampLeft := NodeData^.TimeStampRight;
              NodeData^.SizeLeft := NodeData^.SizeRight;
            end;

            NodeData^.DiffStatus := dsSameAs;
            NodeData^.ExistenceStatus := esBoth;
          end;
        end;
      end;

      Node := Node^.NextSibling;
      Application.ProcessMessages;

      if FStopSyncing then
        Break;
    until Node = nil;
  finally
    spdbtnStopSyncing.Hide;
  end;

  //Compare;  //ideally, Compare should not be called
end;


procedure TfrmFolderDiffMain.spdbtnSyncToLeftClick(Sender: TObject);
begin
  SyncFiles(sdRightToLeft);
end;


procedure TfrmFolderDiffMain.spdbtnSyncToRightClick(Sender: TObject);
begin
  SyncFiles(sdLeftToRight);
end;


procedure TfrmFolderDiffMain.TabSheetFolderDiffResize(Sender: TObject);
begin
  pnlLeft.Width := TabSheetFolderDiff.Width shr 1 - 4;
  pnlRight.Width := pnlLeft.Width;
  pnlRight.Left := pnlLeft.Left + pnlLeft.Width + 8;
end;


procedure TfrmFolderDiffMain.tmrSearchTimer(Sender: TObject);
begin
  tmrSearch.Enabled := False;
  FilterComparison;
end;


procedure TfrmFolderDiffMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  LoadSettingsFromIni;
end;


procedure TfrmFolderDiffMain.vstDiffAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  NodeData: PNodeData;
  TempBmp: TBitmap;
  Idx: Integer;
begin
  if Column = 4 then
  begin
    NodeData := Sender.GetNodeData(Node);
    if NodeData = nil then
      Exit;

    if (NodeData^.ExistenceStatus in [esRightOnly, esBoth]) then
    begin
      TempBmp := TBitmap.Create;
      try
        TempBmp.Width := 16;
        TempBmp.Height := 16;
        TempBmp.PixelFormat := pf24bit;

        TempBmp.Canvas.Pen.Color := clWhite;
        TempBmp.Canvas.Brush.Color := clWhite;
        TempBmp.Canvas.Rectangle(0, 0, 16, 16);

        if NodeData^.FileFolderType = ffFolder then
          Idx := 0
        else
          Idx := 1;

        imglstVST.Draw(TempBmp.Canvas, 0, 0, Idx, dsTransparent, itMask);
        TargetCanvas.Draw(CellRect.Left + Integer(vstDiff.Indent) * Integer(vstDiff.GetNodeLevel(Node)), CellRect.Top, TempBmp);
      finally
        TempBmp.Free;
      end;
    end;
  end;
end;


procedure TfrmFolderDiffMain.vstDiffBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Node^.Index and 1 = 0 then
  begin
    TargetCanvas.Brush.Color := $F8F8F8;
    TargetCanvas.Pen.Color := $F8F8F8;
    TargetCanvas.Rectangle(CellRect);
  end;
end;


procedure TfrmFolderDiffMain.vstDiffClick(Sender: TObject);
begin
  DisplayFirstSelectedFileInStatusBar;
end;


procedure TfrmFolderDiffMain.CreateNewDiffTab;
var
  Node: PVirtualNode;
  LastTabsheet: TTabSheet;
  NodeData: PNodeData;
  NewFrame: TfrFileDiff;
  TempPanel: TPanel;
begin
  Node := vstDiff.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstDiff.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.FileFolderType = ffFolder then
    Exit;

  //if UpperCase(ExtractFileExt(NodeData^.Fnm)) = '.EXE' then
  //  if MessageBoxFunction('Diffing an executable might freeze the application. Continue?', PChar(Application.Title), 0) = IDNO then
  //    Exit;

  LastTabsheet := PageControlMain.AddTabSheet;
  LastTabsheet.Caption := NodeData^.Fnm;
  LastTabsheet.Width := PageControlMain.Width;
  LastTabsheet.ImageIndex := 2;
  Application.ProcessMessages;
  PageControlMain.ActivePageIndex := PageControlMain.PageCount - 1;

  TempPanel := TPanel.Create(LastTabsheet);
  TempPanel.Name := 'Panel_' + IntToStr(GetTickCount64);
  TempPanel.Parent := LastTabsheet;
  TempPanel.Left := 0;
  TempPanel.Top := 0;
  TempPanel.Width := LastTabsheet.Width;
  TempPanel.Height := LastTabsheet.Height;
  TempPanel.Anchors := [akLeft, akTop, akRight, akBottom];

  NewFrame := TfrFileDiff.Create(LastTabsheet);
  NewFrame.Name := 'Frame_' + IntToStr(GetTickCount64);
  NewFrame.Parent := TempPanel;
  NewFrame.Left := 0;
  NewFrame.Top := 0;
  NewFrame.Width := TempPanel.Width;
  NewFrame.Height := TempPanel.Height;
  NewFrame.Anchors := [akLeft, akTop, akRight, akBottom];
  NewFrame.OnCreateNewTextEditorFrame := @HandleOnCreateNewTextEditorFrame;

  NewFrame.edtLeftPath.Text := NodeData^.PathLeft;
  NewFrame.edtRightPath.Text := NodeData^.PathRight;

  //if FileExists(NodeData^.PathLeft) then
  //  NewFrame.synedtLeft.Lines.LoadFromFile(NodeData^.PathLeft);
  //
  //if FileExists(NodeData^.PathRight) then
  //  NewFrame.synedtRight.Lines.LoadFromFile(NodeData^.PathRight);

  SelectSynEditHighlighter(ExtractFileExt(NodeData^.PathLeft), NewFrame.synedtLeft);
  SelectSynEditHighlighter(ExtractFileExt(NodeData^.PathRight), NewFrame.synedtRight);

  NewFrame.LoadContentIntoSynEdits;
  NewFrame.LoadContentIntoMiniMap;
end;


procedure TfrmFolderDiffMain.CreateNewTextEditorTab(ALeftFileName, ARightFileName: string);
var
  LastTabsheet: TTabSheet;
  NewFrame: TfrFileEdit;
  TempPanel: TPanel;
begin
  LastTabsheet := PageControlMain.AddTabSheet;

  if ALeftFileName <> '' then
    LastTabsheet.Caption := ExtractFileName(ALeftFileName)
  else
    LastTabsheet.Caption := ExtractFileName(ARightFileName);

  LastTabsheet.Width := PageControlMain.Width;
  LastTabsheet.ImageIndex := 1;
  Application.ProcessMessages;
  PageControlMain.ActivePageIndex := PageControlMain.PageCount - 1;

  TempPanel := TPanel.Create(LastTabsheet);
  TempPanel.Name := 'Panel_' + IntToStr(GetTickCount64);
  TempPanel.Parent := LastTabsheet;
  TempPanel.Left := 0;
  TempPanel.Top := 0;
  TempPanel.Width := LastTabsheet.Width;
  TempPanel.Height := LastTabsheet.Height;
  TempPanel.Anchors := [akLeft, akTop, akRight, akBottom];

  NewFrame := TfrFileEdit.Create(LastTabsheet);
  NewFrame.Name := 'Frame_' + IntToStr(GetTickCount64);
  NewFrame.Parent := TempPanel;
  NewFrame.Left := 0;
  NewFrame.Top := 0;
  NewFrame.Width := TempPanel.Width;
  NewFrame.Height := TempPanel.Height;
  NewFrame.Anchors := [akLeft, akTop, akRight, akBottom];

  NewFrame.edtLeftPath.Text := ALeftFileName;
  NewFrame.edtRightPath.Text := ARightFileName;

  if FileExists(ALeftFileName) then
    NewFrame.synedtLeft.Lines.LoadFromFile(ALeftFileName)
  else
    NewFrame.synedtLeft.Lines.Text := 'File not found.';

  if FileExists(ARightFileName) then
    NewFrame.synedtRight.Lines.LoadFromFile(ARightFileName)
  else
    NewFrame.synedtRight.Lines.Text := 'File not found.';
end;


procedure TfrmFolderDiffMain.vstDiffDblClick(Sender: TObject);
begin
  CreateNewDiffTab;
end;


procedure TfrmFolderDiffMain.HandleOnCreateNewTextEditorFrame(ALeftFileName, ARightFileName: string);
begin
  CreateNewTextEditorTab(ALeftFileName, ARightFileName);
end;


procedure TfrmFolderDiffMain.vstDiffDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  NodeData: PNodeData;
begin
  if Column = 4 then
  begin
    NodeData := Sender.GetNodeData(Node);
    if NodeData = nil then
      Exit;

    if (NodeData^.ExistenceStatus in [esRightOnly, esBoth]) then
    begin
      DefaultDraw := False;
      TargetCanvas.TextOut(18 + Integer(CellRect.Left) + Integer(vstDiff.Indent) * Integer(vstDiff.GetNodeLevel(Node)), CellRect.Top, NodeData^.Fnm);
    end;
  end;
end;


procedure TfrmFolderDiffMain.vstDiffExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PNodeData;
begin
  if Node^.ChildCount = 0 then
    Exit;

  Node := Node^.FirstChild;

  //if chkFullFolderLoading.Checked then
  //begin
    //NodeData := vstDiff.GetNodeData(Node);
    //if NodeData = nil then
    //  Exit;
    //
    //AddAllFoldersAndFilesFromFolder(Node, ExtractFileDir(NodeData^.PathLeft), ExtractFileDir(NodeData^.PathRight), True, chkFullFolderLoading.Checked, 0);
    //vstDiff.Repaint;
  //end;

  repeat
    NodeData := vstDiff.GetNodeData(Node);
    if NodeData = nil then
      Continue;

    if NodeData^.FileFolderType = ffFile then
    begin
      NodeData^.SizeLeft := GetFileSize(NodeData^.PathLeft);
      NodeData^.SizeRight := GetFileSize(NodeData^.PathRight);
    end;

    vstDiff.RepaintNode(Node);
    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmFolderDiffMain.vstDiffGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PNodeData;
begin
  try
    NodeData := Sender.GetNodeData(Node);
    if NodeData = nil then
    begin
      CellText := '||| NoData |||';
      Exit;
    end;

    CellText := '';

    if (Column in [0..2]) and (NodeData^.ExistenceStatus in [esLeftOnly, esBoth]) then
    begin
      case Column of
        0:
          CellText := NodeData^.Fnm;

        1:
          if NodeData^.FileFolderType = ffFolder then
            CellText := ''
          else
          begin
            CellText := IntToStr(NodeData^.SizeLeft);
            if NodeData^.SizeLeft > 1000000 then
              CellText := CellText + '  (' + IntToStr(NodeData^.SizeLeft shr 20) + 'MB)';
          end;

        2:
          if NodeData^.FileFolderType = ffFile then
            CellText := DateTimeToStr(NodeData^.TimeStampLeft, FFormatSettings);
      end;
    end;

    if Column = 3 then  //diff
    begin
      CellText := CDiffStatusStr[NodeData^.DiffStatus];
    end;

    if (Column in [4..6]) and (NodeData^.ExistenceStatus in [esRightOnly, esBoth]) then
    begin
      case Column of
        4:
          CellText := NodeData^.Fnm;

        5:
          if NodeData^.FileFolderType = ffFolder then
            CellText := ''
          else
          begin
            CellText := IntToStr(NodeData^.SizeRight);
            if NodeData^.SizeRight > 1000000 then
              CellText := CellText + '  (' + IntToStr(NodeData^.SizeRight shr 20) + 'MB)';
          end;

        6:
          if NodeData^.FileFolderType = ffFile then
            CellText := DateTimeToStr(NodeData^.TimeStampRight, FFormatSettings);
      end;
    end;
  except
    on E: Exception do
      CellText := E.Message;
  end;
end;


end.
