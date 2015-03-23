{$I ACBr.inc}

unit ACBrNFSeDANFSeRVReg;

interface

uses
  SysUtils, Classes, ACBrNFSeDANFSeRVClass,
  {$IFDEF FPC}
     LResources
  {$ENDIF} ;

procedure Register;

implementation

{$IFNDEF FPC}
   {$R ACBrNFSe.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrNFSe', [TACBrNFSeDANFSeRV]);
end;

initialization
{$IFDEF FPC}
//   {$i acbrNFSepcn_lcl.lrs}
{$ENDIF}


end.
