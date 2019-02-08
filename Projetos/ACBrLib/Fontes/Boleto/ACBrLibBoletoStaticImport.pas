unit ACBrLibBoletoStaticImport;

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
  CACBrBoletoLIBName = 'ACBrBoleto64.dll';
  {$Else}
  CACBrBoletoLIBName = 'ACBrBoleto32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrBoletoLIBName = 'ACBrBoleto64.so';
  {$Else}
  CACBrBoletoLIBName = 'ACBrBoleto32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function Boleto_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Versao/Retorno}
function Boleto_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Ler/Gravar Config }
function Boleto_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Boleto}
function Boleto_ConfigurarDados(eArquivoIni: PChar; const sResposta: PChar;
    var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_IncluirTitulos(eArquivoIni, eTpSaida: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TotalTitulosLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Imprimir(eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarHTML: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LerRetorno(eDir, eNomeArq: PChar; eListaRelat: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SetDiretorioArquivo(eDir, eArq: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TamNossoNumero(eCarteira: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SelecionaBanco(eCodBanco: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;


{%endregion}


implementation

end.
