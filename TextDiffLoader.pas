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


unit TextDiffLoader;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Interfaces, HashUnit, Diff, {FGL} IntegerList;


type
  TDiffLoader = class
  private
    FDiff: TDiff;
    FSrc1, FSrc2: TStringList;
    FH1, FH2: TCardinalList;

    FLeftFileLineIndex: array of Integer;
    FRightFileLineIndex: array of Integer;

    procedure BuildHashes;
  public
    constructor Create;
    destructor Destroy; override;

    function CompareFiles: Integer;
    function LoadFilesFromMem(AFilePointer1, AFilePointer2: Pointer; Size1, Size2: Int64): Integer;
    function LoadFiles(AFilePath1, AFilePath2: string): Integer;
    function GetSourceLineIndexLeft(ADiffIndex: Integer): Integer;
    function GetSourceLineIndexRight(ADiffIndex: Integer): Integer;
    function GetDiffAtIndex(ADiffIndex: Integer): Byte;
  end;


implementation


constructor TDiffLoader.Create;
begin
  inherited Create;

  FSrc1 := TStringList.Create;
  FSrc2 := TStringList.Create;

  FDiff := TDiff.Create(nil);

  FH1 := TCardinalList.Create;
  FH2 := TCardinalList.Create;
end;


destructor TDiffLoader.Destroy;
begin
  FSrc1.Free;
  FSrc2.Free;

  FH1.Free;
  FH2.Free;

  FDiff.Free;
  inherited Destroy;
end;


procedure TDiffLoader.BuildHashes;
var
  i: integer;
begin
  FH1.Clear;
  for i := 0 to FSrc1.Count - 1 do
    FH1.Add(DWord(HashLine(FSrc1[i], False, False)));      //typecasting to DWord should be fine even on 64-bit, since no pointer operation is done

  FH2.Clear;
  for i := 0 to FSrc2.Count - 1 do
    FH2.Add(DWord(HashLine(FSrc2[i], False, False)));      //typecasting to DWord should be fine even on 64-bit, since no pointer operation is done
end;


function TDiffLoader.CompareFiles: Integer;
var
  i: Integer;
begin
  BuildHashes;

  FDiff.Execute(FH1, FH2);   //FDiff.Execute(FStr1, FStr2);   //the string version of Execute gives worse results than the integer one

  if FDiff.Cancelled then
  begin
    Result := 0;
    Exit;
  end;

  Result := FDiff.Count;

  SetLength(FLeftFileLineIndex, Result);
  SetLength(FRightFileLineIndex, Result);

  for i := 0 to FDiff.Count - 1 do
  begin
    FLeftFileLineIndex[i] := -1;
    FRightFileLineIndex[i] := -1;
  end;

  for i := 0 to FDiff.Count - 1 do
  begin
    if FDiff.Compares[i].Kind <> ckAdd then
      FLeftFileLineIndex[i] := FDiff.Compares[i].oldIndex1;

    if FDiff.Compares[i].Kind <> ckDelete then
      FRightFileLineIndex[i] := FDiff.Compares[i].oldIndex2;
  end;
end;


function TDiffLoader.LoadFilesFromMem(AFilePointer1, AFilePointer2: Pointer; Size1, Size2: Int64): Integer;
var
  AMemStream1, AMemStream2: TMemoryStream;
begin
  AMemStream1 := TMemoryStream.Create;
  AMemStream2 := TMemoryStream.Create;
  try
    AMemStream1.Write(AFilePointer1^, Size1);
    AMemStream2.Write(AFilePointer2^, Size2);

    AMemStream1.Position := 0;
    FSrc1.LoadFromStream(AMemStream1);

    AMemStream2.Position := 0;
    FSrc2.LoadFromStream(AMemStream2);

    Result := CompareFiles;
  finally
    AMemStream1.Free;
    AMemStream2.Free;
  end;
end;


function TDiffLoader.LoadFiles(AFilePath1, AFilePath2: string): Integer;
begin
  if FileExists(AFilePath1) then
    FSrc1.LoadFromFile(AFilePath1);

  if FileExists(AFilePath2) then
    FSrc2.LoadFromFile(AFilePath2);

  Result := CompareFiles;
end;


function TDiffLoader.GetSourceLineIndexLeft(ADiffIndex: Integer): Integer;  //ADiffIndex goes up to Diff.Count.  The result can index the file content.
begin
  try
    Result := FLeftFileLineIndex[ADiffIndex];
  except
    Result := -1;   //the caller should expect -1 for an exception and not actually use it as an index
  end;
end;


function TDiffLoader.GetSourceLineIndexRight(ADiffIndex: Integer): Integer;   //ADiffIndex goes up to Diff.Count.  The result can index the file content.
begin
  try
    Result := FRightFileLineIndex[ADiffIndex];
  except
    Result := -1;   //the caller should expect -1 for an exception and not actually use it as an index
  end;
end;


function TDiffLoader.GetDiffAtIndex(ADiffIndex: Integer): Byte;
begin
  try
    Result := Ord(FDiff.Compares[ADiffIndex].Kind);
  except
    Result := 0;
  end;
end;

end.

