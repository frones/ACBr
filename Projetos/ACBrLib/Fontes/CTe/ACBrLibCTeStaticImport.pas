unit ACBrLibCTeStaticImport;

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
  CACBrCTeLIBName = 'ACBrCTe64.dll';
  {$Else}
  CACBrCTeLIBName = 'ACBrCTe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrCTeLIBName = 'acbrcte64.so';
  {$Else}
  CACBrCTeLIBName = 'acbrcte32.so';

  {$EndIf}
 {$EndIf}

function CTE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;
function CTE_Finalizar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Nome(const sNome: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLer(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravar(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarXML(const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarINI(const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ObterXml(AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_GravarXml(AIndex: longint;
  const eNomeArquivo, ePathArquivo: PChar): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparLista: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparListaEventos: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Assinar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Validar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ValidarRegrasdeNegocios(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_VerificarAssinatura(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Consultar(const eChaveOuCTe: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Enviar(ALote: integer; Imprimir: boolean; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultarRecibo(ARecibo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Cancelar(const eChave, eJustificativa, eCNPJ: PChar;
  ALote: integer; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEvento(idLote: integer; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultaCadastro(cUF, nDocumento: PChar; nIE: boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer;
  eCNPJCPF, eultNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorNSU(const AcUFAutor: integer;
  eCNPJCPF, eNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorChave(const AcUFAutor: integer;
  eCNPJCPF, echCTe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmail(const ePara, eChaveCTe: PChar; const AEnviaPDF: boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Imprimir(const cImpressora: PChar; nNumCopias: integer;
  const cProtocolo, bMostrarPreview: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirPDF: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEvento(const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEventoPDF(
  const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirInutilizacao(const eArquivoXml: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirInutilizacaoPDF(const eArquivoXml: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

implementation

end.
