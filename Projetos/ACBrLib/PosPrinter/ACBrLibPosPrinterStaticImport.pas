unit ACBrLibPosPrinterStaticImport;

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
  CACBrPosPrinterLIBName = 'ACBrPosPrinter64.dll';
  {$Else}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter64.so';
  {$Else}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function POS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Versao/Retorno}
function POS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Ler/Gravar Config }
function POS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}


implementation

end.
