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


unit FileDiffUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, Dialogs;


procedure SelectSynEditHighlighter(Extension: string; ASynEdit: TSynEdit);
procedure FindNextSynEditText(ASelectedSynEdit: TSynEdit; AFindDialog: TFindDialog);


implementation


uses
  HighlightersDataModule;


procedure SelectSynEditHighlighter(Extension: string; ASynEdit: TSynEdit);
begin
  ASynEdit.Highlighter := nil;

  Extension := LowerCase(Extension);

  if (Extension = '.pas') or (Extension = '.mpas') or (Extension = '.ppr') or (Extension = '.dpr') or (Extension = '.inc') then
  begin
    ASynEdit.Highlighter := dmHighlighters.SynPasSyn1;
    Exit;
  end;

  if (Extension = '.lfm') or (Extension = '.frm') or (Extension = '.dfm') then
  begin
    ASynEdit.Highlighter := dmHighlighters.SynLFMSyn1;
    Exit;
  end;

  if (Extension = '.ini') or (Extension = '.clktmpl') or (Extension = '.dyntftcg') then
  begin
    ASynEdit.Highlighter := dmHighlighters.SynIniSyn1;
    Exit;
  end;

  if (Extension = '.xml') or (Extension = '.ctpr') or (Extension = '.ctps') or (Extension = '.lpi') or (Extension = '.lps') then
  begin
    ASynEdit.Highlighter := dmHighlighters.SynXMLSyn1;
    Exit;
  end;

  if Extension = '.py' then
  begin
    ASynEdit.Highlighter := dmHighlighters.SynPythonSyn1;
    Exit;
  end;
end;


procedure FindNextSynEditText(ASelectedSynEdit: TSynEdit; AFindDialog: TFindDialog);
var
  i, BotLine, VisibleLineCount: Integer;
  SearchedText: string;
  UsingMatchCase: Boolean;

  function SelectFoundLine(ALineIndex: Integer): Boolean;
  begin
    Result := False;

    if (UsingMatchCase and (Pos(SearchedText, ASelectedSynEdit.Lines.Strings[ALineIndex]) > 0)) or
       (not UsingMatchCase and (Pos(SearchedText, UpperCase(ASelectedSynEdit.Lines.Strings[ALineIndex])) > 0)) then
    begin
      ASelectedSynEdit.CaretY := ALineIndex + 1;

      VisibleLineCount := ASelectedSynEdit.Height div ASelectedSynEdit.LineHeight;
      BotLine := ASelectedSynEdit.TopLine + VisibleLineCount;

      if ALineIndex > BotLine then
        ASelectedSynEdit.TopLine := ALineIndex - 10 - VisibleLineCount
      else
        if ALineIndex < ASelectedSynEdit.TopLine then
          ASelectedSynEdit.TopLine := ALineIndex - 10;

      Result := True;
    end;
  end;
begin
  if ASelectedSynEdit = nil then
    Exit;

  UsingMatchCase := frMatchCase in AFindDialog.Options;

  if UsingMatchCase then
    SearchedText := AFindDialog.FindText
  else
    SearchedText := UpperCase(AFindDialog.FindText);

  if (frDown in AFindDialog.Options) then //top -> down
  begin
    for i := ASelectedSynEdit.CaretY - 1 + 1 to ASelectedSynEdit.Lines.Count - 1 do
      if SelectFoundLine(i) then
        Break;
  end
  else    //down -> top
  begin
    for i := ASelectedSynEdit.CaretY - 1 - 1 downto 0 do
      if SelectFoundLine(i) then
        Break;
  end;
end;

end.

