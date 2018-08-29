unit ACBrLibCEPStaticImport;

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
  CACBrCEPLIBName = 'ACBrCEP64.dll';
  {$Else}
  CACBrCEPLIBName = 'ACBrCEP32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrCEPLIBName = 'ACBrCEP64.so';
  {$Else}
  CACBrCEPLIBName = 'ACBrCEP32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function CEP_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
{%endregion}

{%region Versao/Retorno}
function CEP_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
{%endregion}

{%region Ler/Gravar Config }
function CEP_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;

function CEP_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
{%endregion}

{%region CEPança}
function CEP_BuscarPorCEP(eCEP: PChar; var Qtde: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
function CEP_BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF,
  eBairro: PChar; var Qtde: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
{%endregion}

implementation

end.
