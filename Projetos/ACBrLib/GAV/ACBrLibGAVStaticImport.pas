unit ACBrLibGAVStaticImport;

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
  CACBrGAVLIBName = 'ACBrGAV64.dll';
  {$Else}
  CACBrGAVLIBName = 'ACBrGAV32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrGAVLIBName = 'ACBrGAV64.so';
  {$Else}
  CACBrGAVLIBName = 'ACBrGAV32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function GAV_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
{%endregion}

{%region Versao/Retorno}
function GAV_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
{%endregion}

{%region Ler/Gravar Config }
function GAV_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;

function GAV_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
{%endregion}

{%region Gaveta}
function GAV_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
function GAV_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
function GAV_AbreGaveta: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGAVLIBName;
{%endregion}

implementation

end.
