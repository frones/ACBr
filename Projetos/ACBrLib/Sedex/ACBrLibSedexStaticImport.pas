unit ACBrLibSedexStaticImport;

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
  CACBrSedexLIBName = 'ACBrSedex64.dll';
  {$Else}
  CACBrSedexLIBName = 'ACBrSedex32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrSedexLIBName = 'ACBrSedex64.so';
  {$Else}
  CACBrSedexLIBName = 'ACBrSedex32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function Sedex_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
{%endregion}

{%region Versao/Retorno}
function Sedex_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
{%endregion}

{%region Ler/Gravar Config }
function Sedex_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;

function Sedex_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
{%endregion}

{%region Sedex}
function Sedex_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
function Sedex_Consultar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
function Sedex_Rastrear(const eCodRastreio: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSedexLIBName;
{%endregion}

implementation

end.
