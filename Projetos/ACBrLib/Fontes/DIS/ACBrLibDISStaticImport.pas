unit ACBrLibDISStaticImport;

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
  CACBrDISLIBName = 'ACBrDIS64.dll';
  {$Else}
  CACBrDISLIBName = 'ACBrDIS32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrDISLIBName = 'ACBrDIS64.so';
  {$Else}
  CACBrDISLIBName = 'ACBrDIS32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function DIS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
{%endregion}

{%region Versao/Retorno}
function DIS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
{%endregion}

{%region Ler/Gravar Config }
function DIS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;

function DIS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
{%endregion}

{%region Cheque}
function DIS_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_LimparDisplay: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_LimparLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_PosicionarCursor(const Linha, Coluna: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_Escrever(const eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_ExibirLinha(const Linha: Integer ; const eTexto: PChar;
  const Alinhamento, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_RolarLinha(const Linha, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_Parar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_Continuar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_PararLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
function DIS_ContinuarLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrDISLIBName;
{%endregion}

implementation

end.
