unit ACBrLibSATStaticImport;

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
  CACBrSATLIBName = 'ACBrSAT64.dll';
  {$Else}
  CACBrSATLIBName = 'ACBrSAT32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrSATLIBName = 'ACBrSAT64.so';
  {$Else}
  CACBrSATLIBName = 'ACBrSAT32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Versao/Retorno}
function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ler/Gravar Config }
function SAT_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ativar}
function SAT_Ativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Desativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Impressão}
function SAT_Imprimir(eString: PChar; PulaLinha, DecodificarTags, CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirLinha(eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirCmd(eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirTags: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Diversos}
function SAT_TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResSATta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Zerar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Reset: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_PularLinhas(NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CortarPapel(Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_AbrirGaveta: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_LerInfoImpressora(const sResSATta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_LerStatusImpressora(Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_RetornarTags(const sResSATta: PChar; var esTamanho: longint; IncluiAjuda: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}


implementation

end.
