{
    Copyright (C) 2024 VCC
    creation date: 23 Jun 2024
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


unit FileEditFrame;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  SynEdit, SynHighlighterPas, SynHighlighterIni, SynHighlighterLFM,
  SynHighlighterXML, Graphics, Dialogs, Menus, Types;

type

  { TfrFileEdit }

  TfrFileEdit = class(TFrame)
    cmbLineBreak: TComboBox;
    edtLeftPath: TEdit;
    edtRightPath: TEdit;
    FindDialog1: TFindDialog;
    lblLineSeparator: TLabel;
    lblModifiedLeft: TLabel;
    lblModifiedRight: TLabel;
    MenuItem_ShowSpecialChars: TMenuItem;
    pnlCurrentSection: TPanel;
    pnlHorizSplitter: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pmSynEdits: TPopupMenu;
    tmrSetLeftFromRightPosition: TTimer;
    tmrSetRightFromLeftPosition: TTimer;
    tmrStartup: TTimer;
    tmrRefreshHighlighter: TTimer;
    procedure edtLeftPathChange(Sender: TObject);
    procedure edtRightPathChange(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MenuItem_ShowSpecialCharsClick(Sender: TObject);
    procedure pnlHorizSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlHorizSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrRefreshHighlighterTimer(Sender: TObject);
    procedure tmrSetLeftFromRightPositionTimer(Sender: TObject);
    procedure tmrSetRightFromLeftPositionTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FHoldSplitter: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;
    FSelectedSynEdit: TSynEdit;

    procedure synedtLeftChange(Sender: TObject);
    procedure synedtRightChange(Sender: TObject);

    procedure synedtLeftKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure synedtRightKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure synedtLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure synedtRightMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure synedtLeftClick(Sender: TObject);
    procedure synedtRightClick(Sender: TObject);

    procedure SynEdtLeftSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure SynEdtRightSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);

    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);
    procedure CreateRemainingUIComponents;

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure ReloadFiles;
    procedure ShowCurrentIniSection;

    procedure FindNextText;
    procedure SetShowSpecialChars;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    synedtLeft: TSynEdit;
    synedtRight: TSynEdit;
  end;

implementation

{$R *.frm}


uses
  FileDiffUtils, IniFiles, ClickerUtils, SynEditTypes;

{ TfrFileEdit }


constructor TfrFileEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHoldSplitter := False;
  FSelectedSynEdit := nil;
  CreateRemainingUIComponents;
  tmrStartup.Enabled := True;
end;


destructor TfrFileEdit.Destroy;
begin
  SaveSettingsToIni;
  inherited Destroy;
end;


procedure TfrFileEdit.CreateRemainingUIComponents;
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
  //synedtLeft.PopupMenu := pmHighlighter;
  synedtLeft.ShowHint := False;
  synedtLeft.TabOrder := 0;
  synedtLeft.Gutter.AutoSize := True;
  synedtLeft.ReadOnly := False;
  synedtLeft.PopupMenu := pmSynEdits;
  synedtLeft.Options := synedtLeft.Options + [eoShowSpecialChars];
  synedtLeft.ScrollBars := ssBoth;
  synedtLeft.OnChange := @synedtLeftChange;
  synedtLeft.OnKeyDown := @synedtLeftKeyDown;
  synedtLeft.OnMouseWheel := @synedtLeftMouseWheel;
  synedtLeft.OnClick := @synedtLeftClick;
  synedtLeft.OnSpecialLineColors := @SynEdtLeftSpecialLineColors;

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
  //synedtRight.PopupMenu := pmHighlighter;
  synedtRight.ShowHint := False;
  synedtRight.TabOrder := 0;
  synedtRight.Gutter.AutoSize := True;
  synedtRight.ReadOnly := False;
  synedtRight.PopupMenu := pmSynEdits;
  synedtRight.Options := synedtRight.Options + [eoShowSpecialChars];
  synedtRight.ScrollBars := ssBoth;
  synedtRight.OnChange := @synedtRightChange;
  synedtRight.OnKeyDown := @synedtRightKeyDown;
  synedtRight.OnMouseWheel := @synedtRightMouseWheel;
  synedtRight.OnClick := @synedtRightClick;
  synedtRight.OnSpecialLineColors := @SynEdtRightSpecialLineColors;

  {$IFNDEF FPC}
    synedtRight.SearchEngine := TSynEditSearch.Create(Self);  //used in old SynEdit (v 2.0.3)
  {$ENDIF}

  {$IFDEF Linux} //GTK2
    cmbLineBreak.Top := 393;
  {$ENDIF}
end;


procedure TfrFileEdit.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FolderDiff.ini');
  try
    pnlHorizSplitter.Left := Ini.ReadInteger('TextEditor', 'SplitterLeft', 400);

    if pnlHorizSplitter.Left < pnlLeft.Constraints.MinWidth then
      pnlHorizSplitter.Left := pnlLeft.Constraints.MinWidth;

    if pnlHorizSplitter.Left > Width - pnlRight.Constraints.MinWidth then
      pnlHorizSplitter.Left := Width - pnlRight.Constraints.MinWidth;

    ResizeFrameSectionsBySplitter(pnlHorizSplitter.Left);

    cmbLineBreak.ItemIndex := Ini.ReadInteger('TextEditor', 'LineBreak', cmbLineBreak.ItemIndex);

    MenuItem_ShowSpecialChars.Checked := Ini.ReadBool('TextEditor', 'ShowSpecialChars', MenuItem_ShowSpecialChars.Checked);
    SetShowSpecialChars;
  finally
    Ini.Free;
  end;
end;


procedure TfrFileEdit.SaveSettingsToIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FolderDiff.ini');
  try
    Ini.WriteInteger('TextEditor', 'SplitterLeft', pnlHorizSplitter.Left);
    Ini.WriteInteger('TextEditor', 'LineBreak', cmbLineBreak.ItemIndex);
    Ini.WriteBool('TextEditor', 'ShowSpecialChars', MenuItem_ShowSpecialChars.Checked);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrFileEdit.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < pnlLeft.Constraints.MinWidth then
    NewLeft := pnlLeft.Constraints.MinWidth;

  if NewLeft > Width - 200 then
    NewLeft := Width - 200;

  pnlHorizSplitter.Left := NewLeft;

  pnlRight.Left := pnlHorizSplitter.Left + pnlHorizSplitter.Width;
  pnlRight.Width := Width - pnlRight.Left;
  pnlLeft.Width := pnlHorizSplitter.Left;
end;


procedure TfrFileEdit.pnlHorizSplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldSplitter then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlHorizSplitter.Left;
    FHoldSplitter := True;
  end;
end;


procedure TfrFileEdit.FrameResize(Sender: TObject);
begin
  ResizeFrameSectionsBySplitter(pnlHorizSplitter.Left);
end;


procedure TfrFileEdit.SetShowSpecialChars;
begin
  if MenuItem_ShowSpecialChars.Checked then
  begin
    synedtLeft.Options := synedtLeft.Options + [eoShowSpecialChars];
    synedtRight.Options := synedtRight.Options + [eoShowSpecialChars];
  end
  else
  begin
    synedtLeft.Options := synedtLeft.Options - [eoShowSpecialChars];
    synedtRight.Options := synedtRight.Options - [eoShowSpecialChars];
  end;
end;


procedure TfrFileEdit.MenuItem_ShowSpecialCharsClick(Sender: TObject);
begin
  SetShowSpecialChars;
end;


procedure TfrFileEdit.FindNextText;
begin
  FindNextSynEditText(FSelectedSynEdit, FindDialog1);
end;


procedure TfrFileEdit.FindDialog1Find(Sender: TObject);
begin
  FindNextText;
end;


procedure TfrFileEdit.edtLeftPathChange(Sender: TObject);
begin
  tmrRefreshHighlighter.Enabled := True;
end;


procedure TfrFileEdit.edtRightPathChange(Sender: TObject);
begin
  tmrRefreshHighlighter.Enabled := True;
end;


procedure TfrFileEdit.pnlHorizSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldSplitter then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrFileEdit.pnlHorizSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHoldSplitter := False;
end;


procedure TfrFileEdit.synedtLeftChange(Sender: TObject);
begin
  lblModifiedLeft.Show;
end;


procedure TfrFileEdit.synedtRightChange(Sender: TObject);
begin
  lblModifiedRight.Show;
end;


procedure TfrFileEdit.ReloadFiles;
begin
  if FileExists(edtLeftPath.Text) then
    synedtLeft.Lines.LoadFromFile(edtLeftPath.Text)
  else
    synedtLeft.Lines.Text := 'File not found.';

  if FileExists(edtRightPath.Text) then
    synedtRight.Lines.LoadFromFile(edtRightPath.Text)
  else
    synedtRight.Lines.Text := 'File not found.';
end;


procedure SaveFileWithFixedLineBreaks(AContent, AFnm: string; ALineBreakOption: Integer);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    {$IFDEF Windows}
      if ALineBreakOption = 1 then //set to LF
        AContent := StringReplace(AContent, #13#10, #10, [rfReplaceAll]);
    {$ELSE}
      if ALineBreakOption = 0 then //set to CRLF
        if (Pos(#13, AContent) = 0) and (Pos(#10, AContent) > 0) then  //there are returns in the file
          AContent := StringReplace(AContent, #10, #13#10, [rfReplaceAll]);
    {$ENDIF}

    TempStream.Write(AContent[1], Length(AContent));
    TempStream.Position := 0;
    TempStream.SaveToFile(AFnm);
  finally
    TempStream.Free;
  end;
end;


procedure TfrFileEdit.synedtLeftKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FSelectedSynEdit := Sender as TSynEdit;

  if Key = Ord('S') then
    if ssCtrl in Shift then
    begin
      SaveFileWithFixedLineBreaks(synedtLeft.Lines.Text, edtLeftPath.Text, cmbLineBreak.ItemIndex);
      lblModifiedLeft.Hide;
    end;

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


procedure TfrFileEdit.synedtRightKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FSelectedSynEdit := Sender as TSynEdit;

  if Key = Ord('S') then
    if ssCtrl in Shift then
    begin
      SaveFileWithFixedLineBreaks(synedtRight.Lines.Text, edtRightPath.Text, cmbLineBreak.ItemIndex);
      lblModifiedRight.Hide;
    end;

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


procedure TfrFileEdit.synedtLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  tmrSetRightFromLeftPosition.Enabled := True;
end;


procedure TfrFileEdit.synedtRightMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  tmrSetLeftFromRightPosition.Enabled := True;
end;


procedure TfrFileEdit.ShowCurrentIniSection;
var
  i: Integer;
  s: string;
begin
  pnlCurrentSection.Caption := '';

  if synedtLeft.Lines.Count > 0 then
    for i := synedtLeft.CaretY - 1 downto 0 do
    begin
      s := synedtLeft.Lines.Strings[i];

      if (Length(s) > 2) and (s[1] = '[') and (s[Length(s)] = ']') then
      begin
        pnlCurrentSection.Caption := 'L: ' + s;
        Break;
      end;
    end;


  if synedtRight.Lines.Count > 0 then
    for i := synedtRight.CaretY - 1 downto 0 do
    begin
      s := synedtRight.Lines.Strings[i];

      if (Length(s) > 2) and (s[1] = '[') and (s[Length(s)] = ']') then
      begin
        pnlCurrentSection.Caption := pnlCurrentSection.Caption + '   R: ' +  s;
        Break;
      end;
    end;
end;


procedure TfrFileEdit.synedtLeftClick(Sender: TObject);
begin
  FSelectedSynEdit := Sender as TSynEdit;
  ShowCurrentIniSection;
  FSelectedSynEdit.Repaint;
end;


procedure TfrFileEdit.synedtRightClick(Sender: TObject);
begin
  FSelectedSynEdit := Sender as TSynEdit;
  ShowCurrentIniSection;
  FSelectedSynEdit.Repaint;
end;


procedure TfrFileEdit.SynEdtLeftSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = (Sender as TSynEdit).CaretY then
  begin
    Special := True;
    BG := $00BBEEFF;
  end;
end;


procedure TfrFileEdit.SynEdtRightSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = (Sender as TSynEdit).CaretY then
  begin
    Special := True;
    BG := $00BBEEFF;
  end;
end;

procedure TfrFileEdit.tmrRefreshHighlighterTimer(Sender: TObject);
begin
  tmrRefreshHighlighter.Enabled := False;

  SelectSynEditHighlighter(ExtractFileExt(edtLeftPath.Text), synedtLeft);
  SelectSynEditHighlighter(ExtractFileExt(edtRightPath.Text), synedtRight);
end;


procedure TfrFileEdit.tmrSetRightFromLeftPositionTimer(Sender: TObject);
begin
  tmrSetRightFromLeftPosition.Enabled := False;
  synedtRight.TopLine := synedtLeft.TopLine;
end;


procedure TfrFileEdit.tmrSetLeftFromRightPositionTimer(Sender: TObject);
begin
  tmrSetLeftFromRightPosition.Enabled := False;
  synedtLeft.TopLine := synedtRight.TopLine;
end;


procedure TfrFileEdit.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  LoadSettingsFromIni;
end;

end.
