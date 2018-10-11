unit ACBrLibLCBStaticImport;

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
  CACBrLCBLIBName = 'ACBrLCB64.dll';
  {$Else}
  CACBrLCBLIBName = 'ACBrLCB32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrLCBLIBName = 'ACBrLCB64.so';
  {$Else}
  CACBrLCBLIBName = 'ACBrLCB32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function LCB_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
{%endregion}

{%region Versao/Retorno}
function LCB_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
{%endregion}

{%region Ler/Gravar Config }
function LCB_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;

function LCB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
{%endregion}

{%region Cheque}
function LCB_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
function LCB_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
function LCB_ApagarFila: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
function LCB_EnviarString(const eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
function LCB_LerFila(var eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
function LCB_LerString(var eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrLCBLIBName;
{%endregion}

implementation

end.
