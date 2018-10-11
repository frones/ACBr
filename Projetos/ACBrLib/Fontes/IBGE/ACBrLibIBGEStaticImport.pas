unit ACBrLibIBGEStaticImport;

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
  CACBrIBGELIBName = 'ACBrIBGE64.dll';
  {$Else}
  CACBrIBGELIBName = 'ACBrIBGE32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrIBGELIBName = 'ACBrIBGE64.so';
  {$Else}
  CACBrIBGELIBName = 'ACBrIBGE32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function IBGE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;
{%endregion}

{%region Versao/Retorno}
function IBGE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;
{%endregion}

{%region Ler/Gravar Config }
function IBGE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;

function IBGE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;
{%endregion}

{%region IBGE}
function IBGE_BuscarPorCodigo(const ACodMun: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;
function IBGE_BuscarPorNome(const eCidade, eUF: PChar; const Exata: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrIBGELIBName;
{%endregion}

implementation

end.
