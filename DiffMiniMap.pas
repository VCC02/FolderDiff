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


unit DiffMiniMap;

interface


uses
  SysUtils, Classes, Graphics, Controls;

type
  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);
  TChangeKindArr = array of TChangeKind;

  TDiffMiniMap = class(TGraphicControl)
  private
    FSelectedEntryIndex: Integer;
    FChanges: TChangeKindArr;
    FHalfWidth: Integer;

    procedure SetSelectedEntryIndex(Value: Integer);
    procedure DrawSelectedLine(ACnv: TCanvas);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetDiffLength(ANewLength: Integer);
    procedure SetDiffAtIndex(AIndex: Integer; ADiff: TChangeKind);

    procedure DrawMiniMap;
    procedure SaveToFile(AFileName: TFileName);
    procedure CopyToClipboard;

    procedure Paint; override;

    property OnMouseDown;
    property OnMouseMove;
    property PopupMenu;

    property SelectedEntryIndex: Integer read FSelectedEntryIndex write SetSelectedEntryIndex;
  end;

implementation

uses
  Clipbrd;


const
//  CDiffGreen = $CCFFCC;
//  CDiffRed = $DDDDFF;
//  CDiffGray = $E0E0E0;
  CLightRed = $7777FF;
  CLightGreen = $77FF77;

//  CLeftDiffColors: array[TChangeKind] of TColor = (clWindow, CDiffGray, CDiffRed, CDiffRed);
//  CRightDiffColors: array[TChangeKind] of TColor = (clWindow, CDiffGreen, CDiffGray, CDiffGreen);

  CLeftDiffColors: array[TChangeKind] of TColor = (clWindow, clSilver, CLightRed, CLightRed);
  CRightDiffColors: array[TChangeKind] of TColor = (clWindow, CLightGreen, clSilver, CLightGreen);

  CSelLeftDiffColors: array[TChangeKind] of TColor = (clOlive, clGray, clMaroon, clMaroon);
  CSelRightDiffColors: array[TChangeKind] of TColor = (clOlive, clGreen, clGray, clGreen);

//  CSelLeftDiffColors: array[TChangeKind] of TColor = (clWindow, clGray, clRed, clRed);
//  CSelRightDiffColors: array[TChangeKind] of TColor = (clWindow, clLime, clGray, clLime);


constructor TDiffMiniMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FChanges, 0);
end;


procedure TDiffMiniMap.SetSelectedEntryIndex(Value: Integer);
begin
  if FSelectedEntryIndex <> Value then
  begin
    FSelectedEntryIndex := Value;
    //do not call DrawMiniMap here
  end;
end;


procedure TDiffMiniMap.SetDiffLength(ANewLength: Integer);
begin
  if ANewLength < 0 then
    raise Exception.Create('Invalid minimap diff length: ' + IntToStr(ANewLength));

  SetLength(FChanges, ANewLength);
  FHalfWidth := Width shr 1;  //the width should be properly set at this point
end;


procedure TDiffMiniMap.SetDiffAtIndex(AIndex: Integer; ADiff: TChangeKind);
begin
  if (AIndex < 0) or (AIndex > Length(FChanges) - 1) then
    raise Exception.Create('Invalid minimap diff index: ' + IntToStr(AIndex));

  FChanges[AIndex] := ADiff;
end;


procedure TDiffMiniMap.DrawSelectedLine(ACnv: TCanvas);
var
  y: Int64;
begin
  if Length(FChanges) = 0 then
    Exit;

  y := Round(Int64(FSelectedEntryIndex) * Int64(Height) / Length(FChanges));
  if y > Int64(Height) - 1 then
    y := Int64(Height) - 1;

  ACnv.Pen.Color := CSelLeftDiffColors[FChanges[FSelectedEntryIndex]];
  ACnv.Line(0, y, FHalfWidth, y);

  ACnv.Pen.Color := CSelRightDiffColors[FChanges[FSelectedEntryIndex]];
  ACnv.Line(FHalfWidth, y, Width, y);
end;


procedure TDiffMiniMap.DrawMiniMap;
var
  i: Integer;
  YOfLn: array of Integer;
  DrawingBuffer: TBitmap;
  y, LastY: Int64;
  FullLen: Integer;
begin
  DrawingBuffer := TBitmap.Create;
  try
    DrawingBuffer.Width := Width;
    DrawingBuffer.Height := Height;

    //draw background
    DrawingBuffer.Canvas.Brush.Color := clWindow;
    DrawingBuffer.Canvas.Pen.Color := DrawingBuffer.Canvas.Brush.Color;
    DrawingBuffer.Canvas.Rectangle(0, 0, Width, Height);

    FullLen := Length(FChanges);
    try
      SetLength(YOfLn, FullLen);
      try
        for i := 0 to FullLen - 1 do
          YOfLn[i] := Round(Int64(i) * Int64(Height) / FullLen);

        DrawingBuffer.Canvas.Lock;
        try
          LastY := -1;

          for i := 0 to FullLen - 1 do
          begin
            y := YOfLn[i];

            if FChanges[i] <> ckNone then
              if LastY <> y then
              begin
                LastY := y;

                DrawingBuffer.Canvas.Pen.Color := CLeftDiffColors[FChanges[i]];
                DrawingBuffer.Canvas.Line(0, y, FHalfWidth, y);

                DrawingBuffer.Canvas.Pen.Color := CRightDiffColors[FChanges[i]];
                DrawingBuffer.Canvas.Line(FHalfWidth, y, Width, y);
              end;
          end; //for
        finally
          DrawingBuffer.Canvas.Unlock;
        end;
      finally
        SetLength(YOfLn, 0);
      end;

      DrawSelectedLine(DrawingBuffer.Canvas);

      DrawingBuffer.Canvas.Pen.Color := clSilver;
      DrawingBuffer.Canvas.Line(FHalfWidth, 1, FHalfWidth, Height - 1);
    finally
      Canvas.Draw(0, 0, DrawingBuffer);
//      Canvas.Font.Color := clBlack;
//      Canvas.TextOut(0, 0, IntToStr(FullLen));
    end;

    //currently selected line
    if FSelectedEntryIndex = -1 then
      Exit;
  finally
    DrawingBuffer.Free;
  end;
end;


procedure TDiffMiniMap.Paint;
begin
  DrawMiniMap;
end;


procedure TDiffMiniMap.SaveToFile(AFileName: TFileName);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;

  try
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    ABitmap.Canvas.CopyRect(Self.ClientRect, Canvas, Self.ClientRect);
    ABitmap.SaveToFile(AFileName);
  finally
    ABitmap.Free;
  end;
end;


procedure TDiffMiniMap.CopyToClipboard;
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;

  try
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    ABitmap.Canvas.CopyRect(Self.ClientRect, Canvas, Self.ClientRect);
    Clipboard.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;


end.
