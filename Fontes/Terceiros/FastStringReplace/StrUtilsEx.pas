(*------------------------------------------------------------------------------
LICENSE: THIS CODE is free. Yes, free. There are no boring license terms that forbid you to do anything with it!
If you make improvements or bug fixes, I would like to receive a copy.
This software is distributed "AS IS", WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

SOURCE: https://bitbucket.org/alex7691/delphi/

AUTHOR: Alexandre C. Machado (alex7691 [at] gmail dot com)

LAST MODIFIED: 23-OCT-2019

DELPHI VERSIONS: Compatible with any Unicode-enabled Delphi compiler: 2009-XE8 (including)

Version History
1.0.2 - Oct. 23, 2019 - It can now be used in FPC and Delphi 7
1.0.1 - June 14, 2015 - Bug fix when rfIgnoreCase is used and OldPattern is in upper case
1.0.0 - March 5, 2015 - Initial version
------------------------------------------------------------------------------*)

unit StrUtilsEx;

{$IfDef CPUX64}
 {$Define USE_FASTPOS}
{$EndIf}

{$IfDef FPC}
  {$Mode Delphi}
  {$Define FPC_OR_LEGACY24}
  {$UnDef USE_FASTPOS}
{$Else}
  {$IF CompilerVersion < 24}
   {$Define FPC_OR_LEGACY24}
  {$IfEnd}
  {$IFDef NEXTGEN}
   {$ZEROBASEDSTRINGS OFF}
  {$EndIf}
{$EndIf}

interface

uses
  SysUtils;

function FastStringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
{$IFDEF USE_FASTPOS}
function FastPos(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
function FastPosEx(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
{$ENDIF}

implementation

{$IFDEF FPC_OR_LEGACY24}
uses
  StrUtils;
{$ENDIF}

// Much faster StringReplace implementation than RTL StringReplace() (including XE7 implementation)
// In our tests it is at least 20% faster in small strings and up to 500% faster in most cases.
// Some corner cases tests showed that this version can be 100x faster than std StringReplace(), e.g.
// Length(OldPattern) = Length(NewPattern) and huge number of occurrences of OldPattern in the string.
// It also beats TStringBuilder.Replace() in all tested scenarios
function FastStringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
const
  Size_of_Char = SizeOf(Char);
var
  Str: string;
  xOldPattern: string;
  i, j: Integer;
  SourceIdx,
  DestIdx,
  p: Integer;
  FindCount: Integer;
  PosArray: array of Integer;
  LenOP, LenNP, ArrLen: Integer;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end;

  if rfIgnoreCase in Flags then begin
    xOldPattern := AnsiUpperCase(OldPattern);
    {$IFDEF FPC_OR_LEGACY24}
    if (CompareStr(xOldPattern,AnsiLowerCase(OldPattern)) = 0) then begin   // if AnsiUpperCase(OldPattern) = AnsiLowerCase(OldPattern) we don't need to use UpperCase() in the whole string
    {$ELSE}
    if SameStr(xOldPattern, AnsiLowerCase(OldPattern)) then begin   // if AnsiUpperCase(OldPattern) = AnsiLowerCase(OldPattern) we don't need to use UpperCase() in the whole string
    {$ENDIF}
      Str := S;
    end else begin
      Str := AnsiUpperCase(S);
    end;
  end else begin
    xOldPattern := OldPattern;
    Str := S;
  end;

  LenOP := Length(OldPattern);
  LenNP := Length(NewPattern);

  i := 1;
  FindCount := 0;
  ArrLen := 0;
  repeat
    //In x64 we call FastPos() directly. In XE3 or below, we call PosEx(). If IDE >= XE3, then call Pos()
    i := {$IFDEF USE_FASTPOS}FastPos{$ELSE}
         {$IFDEF FPC_OR_LEGACY24}PosEx{$ELSE}
         Pos{$ENDIF}
         {$ENDIF}(xOldPattern, Str, i);
    if i = 0 then begin
      Break;
    end;
    Inc(FindCount);
    if ArrLen < FindCount then begin
      if ArrLen = 0 then begin
        ArrLen := 32;
      end else begin
        ArrLen := ArrLen * 2;
      end;
      SetLength(PosArray, ArrLen);   // call SetLength less frequently makes a huge difference when replacing multiple occurrences
    end;
    PosArray[FindCount - 1] := i;
    Inc(i, LenOP);
  until Flags * [rfReplaceAll] = [];

  if FindCount > 0 then begin
    if LenNP = LenOP then begin   // special case where Length(OldPattern) = Length(NewPattern)
      Result := S;                // in this case, we can optimize it even further
      for j := 0 to FindCount - 1 do begin
        DestIdx := PosArray[j];
        if LenNP > 0 then begin
          Move(NewPattern[1], Result[DestIdx], LenNP * Size_of_Char);
        end;
      end;
    end else begin
      SetLength(Result, Length(S) + (LenNP - LenOP) * FindCount);
      SourceIdx := 1;
      DestIdx := 1;
      for j := 0 to FindCount - 1 do begin
        p := PosArray[j] - SourceIdx;
        if p > 0 then begin
          Move(S[SourceIdx], Result[DestIdx], p * Size_of_Char);
          Inc(SourceIdx, p);
          Inc(DestIdx, p);
        end;
        if LenNP > 0 then begin
          Move(NewPattern[1], Result[DestIdx], LenNP * Size_of_Char);
        end;
        Inc(SourceIdx, LenOP);
        Inc(DestIdx, LenNP);
      end;
      p := Length(S) + 1 - SourceIdx;
      if p > 0 then begin
        Move(S[SourceIdx], Result[DestIdx], p * Size_of_Char);
      end;
    end;
  end else begin
    Result := S;
  end;
end;

{-------------------------------------------------------------------------------
Faster Pos() function from FastCode project (original name is PosEx_Sha_Pas_2).
This is 5-10 times faster than RTL version in x64. In x64 we patch the original
Pos() function, replacing it. In x86 this is a little slower than the std RTL
Pos(), so we don't need to patch when compiling as x86
-------------------------------------------------------------------------------}
{$IFDEF CPUX64}
function FastPos(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
Type
  PInteger =^Integer;
var
  len, lenSub: Integer;
  ch: char;
  p, pSub, pStart, pStop: pchar;
label
  Loop0, Loop4,
  TestT, Test0, Test1, Test2, Test3, Test4,
  AfterTestT, AfterTest0,
  Ret, Exit;
begin;
  pSub := pointer(SubStr);
  p := pointer(Str);

  if (p = nil) or (pSub = nil) or (Offset < 1) then
  begin;
    Result := 0;
    goto Exit;
  end;

  lenSub := PLongInt(PByte(pSub) - 4)^ - 1; // <- Modified
  len := PLongInt(PByte(p) - 4)^; // <- Modified
  if (len < lenSub + Offset) or (lenSub < 0) then
  begin;
    Result := 0;
    goto Exit;
  end;

  pStop := p + len;
  p := p + lenSub;
  pSub := pSub + lenSub;
  pStart := p;
  p := p + Offset + 3;

  ch := pSub[0];
  lenSub := -lenSub;
  if p < pStop then
    goto Loop4;
  p := p - 4;
  goto Loop0;

Loop4:
  if ch = p[-4] then
    goto Test4;
  if ch = p[-3] then
    goto Test3;
  if ch = p[-2] then
    goto Test2;
  if ch = p[-1] then
    goto Test1;
Loop0:
  if ch = p[0] then
    goto Test0;
AfterTest0:
  if ch = p[1] then
    goto TestT;
AfterTestT:
  p := p + 6;
  if p < pStop then
    goto Loop4;
  p := p - 4;
  if p < pStop then
    goto Loop0;
  Result := 0;
  goto Exit;

Test3:
  p := p - 2;
Test1:
  p := p - 2;
TestT:
  len := lenSub;
  if lenSub <> 0 then
    repeat
      ;
      if (pSub[len] <> p[len + 1]) or (pSub[len + 1] <> p[len + 2]) then
        goto AfterTestT;
      len := len + 2;
    until len >= 0;
  p := p + 2;
  if p <= pStop then
    goto Ret;
  Result := 0;
  goto Exit;

Test4:
  p := p - 2;
Test2:
  p := p - 2;
Test0:
  len := lenSub;
  if lenSub <> 0 then
    repeat
      ;
      if (pSub[len] <> p[len]) or (pSub[len + 1] <> p[len + 1]) then
        goto AfterTest0;
      len := len + 2;
    until len >= 0;
  Inc(p);
Ret:
  Result := p - pStart;
Exit:
end;

function FastPosEx(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
begin
  Result := FastPos(SubStr, Str, Offset);
end;

{$ENDIF}

end.
