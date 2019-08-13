unit ACBrLibETQStaticImport;

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
  CACBrETQLIBName = 'ACBrETQ64.dll';
  {$Else}
  CACBrETQLIBName = 'ACBrETQ32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrETQLIBName = 'libacbretq64.so';
  {$Else}
  CACBrETQLIBName = 'libacbretq32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function ETQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Versao/Retorno}
function ETQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Ler/Gravar Config }
function ETQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Diversos}
function ETQ_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_IniciarEtiqueta: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_CarregarImagem(const eArquivoImagem, eNomeImagem: PChar;
      Flipped: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Impressão}
function ETQ_Imprimir(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

implementation

end.
