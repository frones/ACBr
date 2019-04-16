unit ACBrLibBPeStaticImport;

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
  CACBrBPeLIBName = 'ACBrBPe64.dll';
  {$Else}
  CACBrBPeLIBName = 'ACBrBPe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrBPeLIBName = 'ACBrBPe64.so';
  {$Else}
  CACBrBPeLIBName = 'ACBrBPe32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function BPe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;
{%endregion}

{%region Versao/Retorno}
function BPe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function BPe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;
{%endregion}

{%region BPe}
function BPe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;
{%endregion}

{%region Servicos}
function BPe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Consultar(const eChaveOuBPe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echBPe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_EnviarEmail(const ePara, eChaveBPe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveBPe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ImprimirEvento(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;

function BPe_ImprimirEventoPDF(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBPeLIBName;
{%endregion}

implementation

end.
