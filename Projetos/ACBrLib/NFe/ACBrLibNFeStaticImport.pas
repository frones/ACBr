unit ACBrLibNFeStaticImport;

{$IfDef FPC}
{$mode objfpc}{$H+}
{$EndIf}

{.$Define STDCALL}

interface

uses
  Classes, SysUtils;

const
 {$IfDef MSWINDOWS}
  {$IfDef CPU64}
   CACBrNFeLIBName = 'ACBrNFe64.dll';
  {$Else}
   CACBrNFeLIBName = 'ACBrNFe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
   CACBrNFeLIBName = 'ACBrNFe64.so';
  {$Else}
   CACBrNFeLIBName = 'ACBrNFe32.so';
  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}
{$I ACBrLibNFeStaticImport.inc}

implementation

end.

