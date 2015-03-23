{$I ACBr.inc}

unit ACBrNFSeDANFSeRLReg;

interface

uses
  SysUtils, {$IFDEF FPC}LResources ,{$ENDIF} Classes, Dialogs, ACBrNFSeDANFSeRLClass;

procedure Register;

implementation

{$IFNDEF FPC}
   {$R ACBrNFSe.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrNFSe', [TACBrNFSeDANFSeRL]);
end;

{$IFDEF FPC}
initialization
   {$i ACBrNFSe.lrs}
{$ENDIF}

end.
