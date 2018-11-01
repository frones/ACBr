unit ACBrLibGNReStaticImport;

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
  CACBrGNReLIBName = 'ACBrGNRe64.dll';
  {$Else}
  CACBrGNReLIBName = 'ACBrGNRe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrGNReLIBName = 'ACBrGNRe64.so';
  {$Else}
  CACBrGNReLIBName = 'ACBrGNRe32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function GNRe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region Versao/Retorno}
function GNRe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region Ler/Gravar Config }
function GNRe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region GNRe}
function GNRe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_LimparListaGuiaRetorno: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_CarregarGuiaRetorno(const eGuiaTXT: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region Servicos}
function GNRe_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_Consultar(const eUF: PChar; const AReceita: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_EnviarEmail(const ePara, eChaveGNRe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;

function GNRe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

implementation

end.
