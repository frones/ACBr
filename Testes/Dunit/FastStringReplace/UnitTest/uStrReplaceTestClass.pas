unit uStrReplaceTestClass;

interface

uses
  SysUtils, Classes;

type
  TStrReplaceTest = class
  public
    function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
    function FastStringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
  end;

implementation

uses
  StrUtilsEx;

{ TStrReplaceTest }

function TStrReplaceTest.FastStringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
begin
  Result := StrUtilsEx.FastStringReplace(S, OldPattern, NewPattern, Flags);
end;

function TStrReplaceTest.StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
begin
  Result := SysUtils.StringReplace(S, OldPattern, NewPattern, Flags);
end;

end.
