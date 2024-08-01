{
    Copyright (C) 2024 VCC
    creation date: 16 Jun 2024
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


unit FileDiffFrame;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  Buttons, SynEdit, SynEditKeyCmds, SynEditMouseCmds, SynGutterLineNumber,
  SynHighlighterPas, SynHighlighterIni, SynHighlighterLFM, SynHighlighterXML,
  DiffMiniMap, Graphics, Menus, Dialogs, TextDiffLoader, FileDiffUtils,
  ClickerUtils;

type

  TOnCreateNewTextEditorFrame = procedure(ALeftFileName, ARightFileName: string) of object;

  { TfrFileDiff }

  TfrFileDiff = class(TFrame)
    edtLeftPath: TEdit;
    edtRightPath: TEdit;
    FindDialog1: TFindDialog;
    lblInfo: TLabel;
    MenuItem_OpenInTextEditor: TMenuItem;
    pnlLeft: TPanel;
    pnlMiniMap: TPanel;
    pnlRight: TPanel;
    pmHighlighter: TPopupMenu;
    spdbtnJumpToNextSection: TSpeedButton;
    spdbtnJumpToPrevSection: TSpeedButton;
    SynIniSyn1: TSynIniSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPasSyn1: TSynPasSyn;
    SynXMLSyn1: TSynXMLSyn;
    tmrSetRightFromLeftPosition: TTimer;
    tmrRepaintMinimapOnSelect: TTimer;
    tmrSetLeftFromRightPosition: TTimer;
    procedure edtLeftPathKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtRightPathKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FindDialog1Find(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MenuItem_OpenInTextEditorClick(Sender: TObject);
    procedure pnlMiniMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tmrRepaintMinimapOnSelectTimer(Sender: TObject);
    procedure tmrSetLeftFromRightPositionTimer(Sender: TObject);
    procedure tmrSetRightFromLeftPositionTimer(Sender: TObject);
  private
    FLeftSource: TStringList;
    FRightSource: TStringList;
    FDiffCount: Integer;

    FMiniMap: TDiffMiniMap;
    FDiffLoader: TDiffLoader;
    FSelectedSynEdit: TSynEdit;

    FOnCreateNewTextEditorFrame: TOnCreateNewTextEditorFrame;

    procedure MinimapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MinimapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure DoOnCreateNewTextEditorFrame;

    procedure CreateRemainingUIComponents;
    procedure ReloadFiles;

    procedure SynEdtLeftOnProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
    procedure SynEdtRightOnProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);

    procedure SynEdtLeftSynGutterLineNumberFormatLineNumber(Sender: TSynGutterLineNumber; ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
    procedure SynEdtRightSynGutterLineNumberFormatLineNumber(Sender: TSynGutterLineNumber; ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);

    //procedure SynEdtLeftOnScroll(Sender: TObject; ScrollBar: TScrollBarKind);
    //procedure SynEdtRightOnScroll(Sender: TObject; ScrollBar: TScrollBarKind);
    //procedure SynEdtLeftGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
    //procedure SynEdtRightGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
    procedure SynEdtLeftSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure SynEdtRightSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);

    procedure synedtLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure synedtRightMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure synedtLeftKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure synedtRightKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure synedtLeftClick(Sender: TObject);
    procedure synedtRightClick(Sender: TObject);

    procedure CreateMinimap;
    procedure FindNextText;
  public
    synedtLeft: TSynEdit;
    synedtRight: TSynEdit;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadContentIntoSynEdits;
    procedure LoadContentIntoMiniMap;

    property OnCreateNewTextEditorFrame: TOnCreateNewTextEditorFrame write FOnCreateNewTextEditorFrame;
  end;


function DisplayDiff(ALeftFile, ARightFile: string; AOnCreateNewTextEditorFrame: TOnCreateNewTextEditorFrame): Boolean;


implementation

{$R *.frm}

{$IFnDEF FPC}
  uses
    SynEditSearch; //some old version of SynEdit
{$ENDIF}


function DisplayDiff(ALeftFile, ARightFile: string; AOnCreateNewTextEditorFrame: TOnCreateNewTextEditorFrame): Boolean;
var
  frFileDiff: TfrFileDiff;
begin
  frFileDiff := TfrFileDiff.Create(nil);

  frFileDiff.edtLeftPath.Text := ALeftFile;
  frFileDiff.edtRightPath.Text := ARightFile;
  frFileDiff.FOnCreateNewTextEditorFrame := AOnCreateNewTextEditorFrame;

  SelectSynEditHighlighter(ExtractFileExt(ALeftFile), frFileDiff.synedtLeft);
  SelectSynEditHighlighter(ExtractFileExt(ARightFile), frFileDiff.synedtRight);

  frFileDiff.LoadContentIntoSynEdits;
  frFileDiff.LoadContentIntoMiniMap;

  //frFileDiff.ShowModal; ////////////////
  Result := True;
end;


{ TfrFileDiff }

//procedure TfrFileDiff.pnlMiniMapMouseDown(Sender: TObject;
//  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//begin
//  pnlMiniMapMouseMove(Sender, Shift, X, Y);
//end;
//
//procedure TfrFileDiff.FrameResize(Sender: TObject);
//begin
//  //
//end;
//
//
//procedure TfrFileDiff.pnlMiniMapMouseMove(Sender: TObject; Shift: TShiftState;
//  X, Y: Integer);
//var
//  SelIndex: Int64;
//begin
//  if ssLeft in Shift then
//  begin
//    SelIndex := Round(Int64(FDiffCount) * Int64(Y) / Int64(pnlMiniMap.Height));
//    if SelIndex > FDiffCount - 1 then
//      SelIndex := FDiffCount - 1;
//
//    try
//      synedtLeft.TopLine := SelIndex;
//      synedtRight.TopLine := SelIndex;
//      FMiniMap.SelectedEntryIndex := SelIndex - 1;
//    except
//      //bad index  /  out of bounds index
//    end;
//
//    tmrRepaintMinimapOnSelect.Enabled := True;
//  end;
//end;
//
//
//procedure TfrFileDiff.tmrRepaintMinimapOnSelectTimer(Sender: TObject);
//begin
//  tmrRepaintMinimapOnSelect.Enabled := False;
//  FMiniMap.DrawMiniMap;
//end;


constructor TfrFileDiff.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSelectedSynEdit := nil;
  CreateRemainingUIComponents;
  FLeftSource := TStringList.Create;
  FRightSource := TStringList.Create;
  FDiffLoader := TDiffLoader.Create;
  FOnCreateNewTextEditorFrame := nil;
end;


destructor TfrFileDiff.Destroy;
begin
  FreeAndNil(FDiffLoader);
  FLeftSource.Free;
  FRightSource.Free;

  inherited Destroy;
end;


procedure TfrFileDiff.DoOnCreateNewTextEditorFrame;
begin
  if Assigned(FOnCreateNewTextEditorFrame) then
    FOnCreateNewTextEditorFrame(edtLeftPath.Text, edtRightPath.Text)
  else
    raise Exception.Create('OnCreateNewTextEditorFrame not assigned.');
end;


procedure TfrFileDiff.LoadContentIntoSynEdits;
var
  i: Integer;
  LineIndex: Integer;
  LeftMem: TMemoryStream;
  RightMem: TMemoryStream;
begin
  try
    LeftMem := TMemoryStream.Create;
    RightMem := TMemoryStream.Create;
    try
      LeftMem.LoadFromFile(edtLeftPath.Text);
      RightMem.LoadFromFile(edtRightPath.Text);

      try
        LeftMem.Position := 0;
        FLeftSource.LoadFromStream(LeftMem);
        RightMem.Position := 0;
        FRightSource.LoadFromStream(RightMem);
      finally
        FDiffCount := FDiffLoader.LoadFilesFromMem(LeftMem.Memory, RightMem.Memory, LeftMem.Size, RightMem.Size);
      end;

      if LeftMem.Size = RightMem.Size then
        if CompareMem(LeftMem.Memory, RightMem.Memory, LeftMem.Size) then
          lblInfo.Caption := 'same content';

      lblInfo.Hint := 'Left Size: ' + IntToStr(LeftMem.Size) + 'B.   Right Size: ' + IntToStr(RightMem.Size) + 'B.';
    finally
      LeftMem.Free;
      RightMem.Free;
    end;

    synedtLeft.BeginUpdate;
    synedtRight.BeginUpdate;
    try
      synedtLeft.Clear;
      synedtRight.Clear;

      for i := 0 to FDiffCount - 1 do
      begin
        LineIndex := FDiffLoader.GetSourceLineIndexLeft(i);
        if LineIndex = -1 then
          synedtLeft.Lines.Add('')
        else
        begin
          try
            synedtLeft.Lines.Add(FLeftSource.Strings[LineIndex]);
          except
            synedtLeft.Lines.Add('====================  Diff Exception at index ' + IntToStr(i));
          end;
        end;

        LineIndex := FDiffLoader.GetSourceLineIndexRight(i);
        if LineIndex = -1 then
          synedtRight.Lines.Add('')
        else
        begin
          try
            synedtRight.Lines.Add(FRightSource.Strings[LineIndex]);
          except
            synedtRight.Lines.Add('====================  Diff Exception at index ' + IntToStr(i));
          end;
        end;
      end;  //for i
    finally
      synedtLeft.EndUpdate;
      synedtRight.EndUpdate;
    end;
  except
    on E: Exception do
    begin
      synedtLeft.Text := E.Message;
      synedtRight.Text := synedtLeft.Text;
    end;
  end;
end;


procedure TfrFileDiff.LoadContentIntoMiniMap;
var
  i: Integer;
begin
  FMiniMap.SetDiffLength(FDiffCount);
  for i := 0 to FDiffCount - 1 do
    FMiniMap.SetDiffAtIndex(i, TChangeKind(FDiffLoader.GetDiffAtIndex(i)));
end;


procedure TfrFileDiff.SynEdtLeftOnProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin                                                           //Application.MainForm.Caption := 'L' + IntToStr(synedtLeft.CaretY);
  if Command in [ecScrollUp, ecScrollDown] then
    tmrSetRightFromLeftPosition.Enabled := True;
end;


procedure TfrFileDiff.SynEdtRightOnProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin                                                          //Application.MainForm.Caption := 'R' + IntToStr(Random(10));
  if Command in [ecScrollUp, ecScrollDown] then
    tmrSetLeftFromRightPosition.Enabled := True;
end;


procedure TfrFileDiff.SynEdtLeftSynGutterLineNumberFormatLineNumber(Sender: TSynGutterLineNumber; ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
var
  LineIndex: Integer;
begin
  LineIndex := FDiffLoader.GetSourceLineIndexLeft(ALine - 1);
  if LineIndex = -1 then
    AText := ''
  else
    AText := IntToStr(LineIndex + 1);
end;


procedure TfrFileDiff.SynEdtRightSynGutterLineNumberFormatLineNumber(Sender: TSynGutterLineNumber; ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
var
  LineIndex: Integer;
begin
  LineIndex := FDiffLoader.GetSourceLineIndexRight(ALine - 1);
  if LineIndex = -1 then
    AText := ''
  else
    AText := IntToStr(LineIndex + 1);
end;


//procedure TfrFileDiff.SynEdtLeftGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
//var
//  LineIndex: Integer;
//begin
//  LineIndex := FDiffLoader.GetSourceLineIndexLeft(aLine - 1);
//  if LineIndex = -1 then
//    aText := ''
//  else
//    aText := IntToStr(LineIndex + 1);
//end;
//
//
//procedure TfrFileDiff.SynEdtRightGutterGetText(Sender: TObject; aLine: Integer; var aText: string);
//var
//  LineIndex: Integer;
//begin
//  LineIndex := FDiffLoader.GetSourceLineIndexRight(aLine - 1);
//  if LineIndex = -1 then
//    aText := ''
//  else
//    aText := IntToStr(LineIndex + 1);
//end;


//type
//  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

const
  CDiffGreen: TColor = $CCFFCC;
  CDiffRed  : TColor = $DDDDFF;
  CDiffGray : TColor = $E0E0E0;

procedure TfrFileDiff.SynEdtLeftSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  ChKind: TChangeKind;
begin
  Special := True;

  try
    if Line <= FDiffCount then   // "<=", because Line is 1-indexed in SynEdit
    begin
      ChKind := TChangeKind(FDiffLoader.GetDiffAtIndex(Line - 1));
      case ChKind of
        ckNone:
        begin
          BG := clWindow;
          Special := False;
        end;

        ckModify: BG := CDiffRed;
        ckDelete: BG := CDiffRed;
        ckAdd: BG := CDiffGray;
      end;

      if Line = (Sender as TSynEdit).CaretY then
      begin
        if Special then
          BG := ModifyBrightness(BG, 48, boDecB)
        else
        begin
          Special := True;
          BG := $00BBEEFF;
        end;
      end;
    end
    else
      if Line = (Sender as TSynEdit).CaretY then
        BG := $00BBEEFF;
  except
    FG := clWhite;
    BG := clRed;
    Special := True;
  end;
end;


procedure TfrFileDiff.SynEdtRightSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  ChKind: TChangeKind;
begin
  Special := True;

  try
    if Line <= FDiffCount then     // "<=", because Line is 1-indexed in SynEdit
    begin
      ChKind := TChangeKind(FDiffLoader.GetDiffAtIndex(Line - 1));
      case ChKind of
        ckNone:
        begin
          BG := clWindow;
          Special := False;
        end;

        ckModify: BG := CDiffGreen;
        ckDelete: BG := CDiffGray;
        ckAdd: BG := CDiffGreen;
      end;

      if Line = (Sender as TSynEdit).CaretY then
      begin
        if Special then
          BG := ModifyBrightness(BG, 48, boDecB)
        else
        begin
          Special := True;
          BG := $00BBEEFF;
        end;
      end;
    end
    else
      if Line = (Sender as TSynEdit).CaretY then
        BG := $00BBEEFF;
  except
    FG := clWhite;
    BG := clRed;
    Special := True;
  end;
end;


procedure TfrFileDiff.synedtLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  tmrSetRightFromLeftPosition.Enabled := True;
end;


procedure TfrFileDiff.synedtRightMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  tmrSetLeftFromRightPosition.Enabled := True;
end;


procedure TfrFileDiff.synedtLeftKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TempTopLine: Integer;
begin
  FSelectedSynEdit := Sender as TSynEdit;

  if Key = VK_F5 then
    ReloadFiles;

  if Key = Ord('F') then
    if ssCtrl in Shift then
      FindDialog1.Execute;

  if Key = VK_F3 then
    FindNextText;

  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    FSelectedSynEdit.Repaint;
end;


procedure TfrFileDiff.synedtRightKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TempTopLine: Integer;
begin
  FSelectedSynEdit := Sender as TSynEdit;

  if Key = VK_F5 then
  begin
    TempTopLine := synedtRight.TopLine;
    LoadContentIntoSynEdits;
    LoadContentIntoMiniMap;

    synedtLeft.TopLine := TempTopLine;
    synedtRight.TopLine := TempTopLine;

    FMiniMap.SelectedEntryIndex := synedtRight.TopLine - 1;
    tmrRepaintMinimapOnSelect.Enabled := True
  end;

  if Key = Ord('F') then
    if ssCtrl in Shift then
      FindDialog1.Execute;

  if Key = VK_F3 then
    FindNextText;

  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    FSelectedSynEdit.Repaint;
end;


procedure TfrFileDiff.synedtLeftClick(Sender: TObject);
begin
  FSelectedSynEdit := Sender as TSynEdit;
  FSelectedSynEdit.Repaint;
end;


procedure TfrFileDiff.synedtRightClick(Sender: TObject);
begin
  FSelectedSynEdit := Sender as TSynEdit;
  FSelectedSynEdit.Repaint;
end;


procedure TfrFileDiff.tmrRepaintMinimapOnSelectTimer(Sender: TObject);
begin
  tmrRepaintMinimapOnSelect.Enabled := False;
  FMiniMap.DrawMiniMap;
end;


procedure TfrFileDiff.tmrSetRightFromLeftPositionTimer(Sender: TObject);
begin
  tmrSetRightFromLeftPosition.Enabled := False;

  synedtRight.TopLine := synedtLeft.TopLine;
  FMiniMap.SelectedEntryIndex := synedtRight.TopLine - 1;
  tmrRepaintMinimapOnSelect.Enabled := True;
end;


procedure TfrFileDiff.tmrSetLeftFromRightPositionTimer(Sender: TObject);
begin
  tmrSetLeftFromRightPosition.Enabled := False;

  synedtLeft.TopLine := synedtRight.TopLine;
  FMiniMap.SelectedEntryIndex := synedtLeft.TopLine - 1;
  tmrRepaintMinimapOnSelect.Enabled := True;
end;


procedure TfrFileDiff.CreateRemainingUIComponents;
begin
  synedtLeft := TSynEdit.Create(Self);
  synedtLeft.Parent := pnlLeft;
  synedtLeft.Left := 0; //pnlMiniMap.Width;
  synedtLeft.Top := edtLeftPath.Height + 2;
  synedtLeft.Width := pnlLeft.Width; //(ClientWidth - pnlMiniMap.Width) shr 1;
  synedtLeft.Height := pnlLeft.Height - edtLeftPath.Height - 4; //ClientHeight - 30 - edtLeftPath.Height;
  synedtLeft.Hint := '';
  synedtLeft.Anchors := [akLeft, akTop, akRight, akBottom]; //[akLeft, akTop, akBottom];
  synedtLeft.Font.Charset := DEFAULT_CHARSET;
  synedtLeft.Font.Color := clWindowText;
  synedtLeft.Font.Height := -13;
  synedtLeft.Font.Name := 'Courier New';
  synedtLeft.Font.Style := [];
  synedtLeft.ParentShowHint := False;
  synedtLeft.PopupMenu := pmHighlighter;
  synedtLeft.ShowHint := False;
  synedtLeft.TabOrder := 0;
  synedtLeft.OnKeyDown := nil;
  synedtLeft.Gutter.AutoSize := True;
  synedtLeft.ReadOnly := True;
  synedtLeft.ScrollBars := ssBoth;

  //try
  //  synedtLeft.MouseActions.Add.Command := emcWheelScrollDown;
  //  synedtLeft.MouseActions.Add.Command := emcWheelScrollUp;         //AV (actually a crash) if using both
  //except
  //end;

  synedtLeft.OnProcessCommand := @SynEdtLeftOnProcessCommand;
  (synedtLeft.Gutter.Parts[1] as TSynGutterLineNumber).OnFormatLineNumber := @SynEdtLeftSynGutterLineNumberFormatLineNumber;
  //synedtLeft.OnScroll := SynEdtLeftOnScroll;
  //synedtLeft.OnGutterGetText := SynEdtLeftGutterGetText;
  synedtLeft.OnSpecialLineColors := @SynEdtLeftSpecialLineColors;
  synedtLeft.OnMouseWheel := @synedtLeftMouseWheel;
  synedtLeft.OnKeyDown := @synedtLeftKeyDown;
  synedtLeft.OnClick := @synedtLeftClick;
  {$IFNDEF FPC}
    synedtLeft.SearchEngine := TSynEditSearch.Create(Self);  //used in old SynEdit (v 2.0.3)
  {$ENDIF}

  synedtRight := TSynEdit.Create(Self);
  synedtRight.Parent := pnlRight;
  synedtRight.Left := 0; //(ClientWidth - pnlMiniMap.Width) shr 1 + pnlMiniMap.Width;
  synedtRight.Top := edtRightPath.Height + 2;
  synedtRight.Width := pnlRight.Width; //ClientWidth shr 1;
  synedtRight.Height := pnlRight.Height - edtRightPath.Height - 4; //ClientHeight - 30 - edtRightPath.Height;
  synedtRight.Hint := '';
  synedtRight.Anchors := [akLeft, akTop, akRight, akBottom]; //[akTop, akRight, akBottom];
  synedtRight.Font.Charset := DEFAULT_CHARSET;
  synedtRight.Font.Color := clWindowText;
  synedtRight.Font.Height := -13;
  synedtRight.Font.Name := 'Courier New';
  synedtRight.Font.Style := [];
  synedtRight.ParentShowHint := False;
  synedtRight.PopupMenu := pmHighlighter;
  synedtRight.ShowHint := False;
  synedtRight.TabOrder := 0;
  synedtRight.OnKeyDown := nil;
  synedtRight.Gutter.AutoSize := True;
  synedtRight.ReadOnly := True;
  synedtRight.ScrollBars := ssBoth;

  synedtRight.OnProcessCommand := @SynEdtRightOnProcessCommand;
  (synedtRight.Gutter.Parts[1] as TSynGutterLineNumber).OnFormatLineNumber := @SynEdtRightSynGutterLineNumberFormatLineNumber;
  //synedtRight.OnScroll := SynEdtRightOnScroll;
  //synedtRight.OnGutterGetText := SynEdtRightGutterGetText;
  synedtRight.OnSpecialLineColors := @SynEdtRightSpecialLineColors;
  synedtRight.OnMouseWheel := @synedtRightMouseWheel;
  synedtRight.OnKeyDown := @synedtRightKeyDown;
  synedtRight.OnClick := @synedtRightClick;
  {$IFNDEF FPC}
    synedtRight.SearchEngine := TSynEditSearch.Create(Self);  //used in old SynEdit (v 2.0.3)
  {$ENDIF}

  CreateMinimap;
end;


procedure TfrFileDiff.FrameResize(Sender: TObject);
begin
  //synedtLeft.Width := (ClientWidth - pnlMiniMap.Width) shr 1;
  //synedtRight.Width := (ClientWidth - pnlMiniMap.Width) shr 1;
  //synedtRight.Left := synedtRight.Width + pnlMiniMap.Width;
  //
  //edtLeftPath.Width := (ClientWidth - pnlMiniMap.Width) shr 1;
  //edtRightPath.Width := (ClientWidth - pnlMiniMap.Width) shr 1;
  //edtRightPath.Left := edtRightPath.Width + pnlMiniMap.Width;

  pnlLeft.Width := (ClientWidth - pnlMiniMap.Width) shr 1 - 4;
  pnlRight.Width := (ClientWidth - pnlMiniMap.Width) shr 1 - 4;
  pnlRight.Left := pnlRight.Width + pnlMiniMap.Width + 4;

  tmrRepaintMinimapOnSelect.Enabled := True;
end;


procedure TfrFileDiff.FindNextText;
begin
  FindNextSynEditText(FSelectedSynEdit, FindDialog1);
end;


procedure TfrFileDiff.FindDialog1Find(Sender: TObject);
begin
  FindNextText;
end;


procedure TfrFileDiff.ReloadFiles;
var
  TempTopLine: Integer;
begin
  //DisplayDiff(edtLeftPath.Text, edtRightPath.Text, FOnCreateNewTextEditorFrame);

  TempTopLine := synedtLeft.TopLine;
  LoadContentIntoSynEdits;
  LoadContentIntoMiniMap;

  synedtLeft.TopLine := TempTopLine;
  synedtRight.TopLine := TempTopLine;

  FMiniMap.SelectedEntryIndex := synedtLeft.TopLine - 1;
  tmrRepaintMinimapOnSelect.Enabled := True;
end;


procedure TfrFileDiff.edtLeftPathKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ReloadFiles;
end;


procedure TfrFileDiff.edtRightPathKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TempTopLine: Integer;
begin
  if Key = VK_RETURN then
    ReloadFiles;
end;


procedure TfrFileDiff.MenuItem_OpenInTextEditorClick(Sender: TObject);
begin
  DoOnCreateNewTextEditorFrame;
end;


procedure TfrFileDiff.MinimapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  pnlMiniMapMouseMove(Sender, Shift, X, Y);
end;


procedure TfrFileDiff.pnlMiniMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pnlMiniMapMouseMove(Sender, Shift, X, Y);
end;


procedure TfrFileDiff.pnlMiniMapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  SelIndex: Int64;
begin
  if ssLeft in Shift then
  begin
    SelIndex := Round(Int64(FDiffCount) * Int64(Y) / Int64(pnlMiniMap.Height));
    if SelIndex > Int64(FDiffCount) - 1 then
      SelIndex := Int64(FDiffCount) - 1;

    if SelIndex < 0 then
      SelIndex := 0;

    try
      synedtLeft.TopLine := SelIndex;
      synedtRight.TopLine := SelIndex;
      FMiniMap.SelectedEntryIndex := SelIndex - 1;
    except
      //bad index  /  out of bounds index
    end;

    tmrRepaintMinimapOnSelect.Enabled := True;
  end;
end;


procedure TfrFileDiff.MinimapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pnlMiniMapMouseDown(Sender, Button, Shift, X, Y);
end;


procedure TfrFileDiff.CreateMinimap;
begin
  FMiniMap := TDiffMiniMap.Create(pnlMiniMap);
  FMiniMap.Parent := pnlMiniMap;
  FMiniMap.Left := 0;
  FMiniMap.Top := 0;
  FMiniMap.Width := pnlMiniMap.Width;
  FMiniMap.Height := pnlMiniMap.Height;
  FMiniMap.Visible := True;
  FMiniMap.Anchors := [akLeft, akTop, akRight, akBottom];

  FMiniMap.OnMouseDown := @MinimapMouseDown;
  FMiniMap.OnMouseMove := @MinimapMouseMove;
  FMiniMap.PopupMenu := nil;
end;

end.
