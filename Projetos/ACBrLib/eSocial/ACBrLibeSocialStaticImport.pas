unit ACBrLibeSocialStaticImport;

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
  CACBreSocialLIBName = 'ACBreSocial64.dll';
  {$Else}
  CACBreSocialLIBName = 'ACBreSocial32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBreSocialLIBName = 'ACBreSocial64.so';
  {$Else}
  CACBreSocialLIBName = 'ACBreSocial32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function eSocial_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region Versao/Retorno}
function eSocial_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region Ler/Gravar Config }
function eSocial_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region eSocial}
function eSocial_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
function eSocial_Consultar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
function eSocial_Rastrear(const eCodRastreio: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

implementation

end.
