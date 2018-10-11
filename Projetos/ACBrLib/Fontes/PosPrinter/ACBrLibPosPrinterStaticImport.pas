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

{%region Ativar}
function POS_Ativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Desativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Impressão}
function POS_Imprimir(eString: PChar; PulaLinha, DecodificarTags, CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirLinha(eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirCmd(eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirTags: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Diversos}
function POS_TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Zerar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_InicializarPos: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Reset: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_PularLinhas(NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_CortarPapel(Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_AbrirGaveta: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerInfoImpressora(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerStatusImpressora(Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_RetornarTags(const sResposta: PChar; var esTamanho: longint; IncluiAjuda: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}


implementation

end.
