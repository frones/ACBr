unit ACBrLibBALStaticImport;

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
  CACBrBALLIBName = 'ACBrBAL64.dll';
  {$Else}
  CACBrBALLIBName = 'ACBrBAL32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrBALLIBName = 'ACBrBAL64.so';
  {$Else}
  CACBrBALLIBName = 'ACBrBAL32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function BAL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
{%endregion}

{%region Versao/Retorno}
function BAL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
{%endregion}

{%region Ler/Gravar Config }
function BAL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;

function BAL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
{%endregion}

{%region Balança}
function BAL_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
function BAL_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
function BAL_LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
function BAL_SolicitarPeso: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
function BAL_InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBALLIBName;
{%endregion}

implementation

end.
