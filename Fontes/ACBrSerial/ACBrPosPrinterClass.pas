{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}
unit ACBrPosPrinterClass;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils;

type
  EPosPrinterException = class(Exception);

  TACBrPosPrinterModelo = (ppTexto, ppEscPosEpson, ppEscBematech, ppEscDaruma,
                           ppEscVox, ppEscDiebold, ppEscEpsonP2);

  TACBrPosTipoFonte = (ftNormal, ftCondensado, ftExpandido, ftNegrito,
    ftSublinhado, ftInvertido, ftItalico, ftFonteB, ftAlturaDupla);
  TACBrPosFonte = set of TACBrPosTipoFonte;

  TACBrPosTipoAlinhamento = (alEsquerda, alCentro, alDireita);
  TACBrPosPaginaCodigo = (pcNone, pc437, pc850, pc852, pc860, pcUTF8, pc1252);
  TACBrPosDirecao = (dirEsquerdaParaDireita, dirTopoParaBaixo, dirDireitaParaEsquerda, dirBaixoParaTopo);

  TACBrPosTipoStatus = (stErro, stNaoSerial, stPoucoPapel, stSemPapel,
                        stGavetaAberta, stImprimindo, stOffLine, stTampaAberta,
                        stErroLeitura);
  TACBrPosPrinterStatus = set of TACBrPosTipoStatus;

  TACBrPosTipoCorte = (ctTotal, ctParcial);

  { TACBrPosComandos }

  TACBrPosComandos = class
  private
    FBeep: AnsiString;
    FAlinhadoCentro: AnsiString;
    FAlinhadoDireita: AnsiString;
    FAlinhadoEsquerda: AnsiString;
    FCorteParcial: AnsiString;
    FDesligaAlturaDupla: AnsiString;
    FDesligaInvertido: AnsiString;
    FDesligaModoPagina: AnsiString;
    FEspacoEntreLinhasPadrao: AnsiString;
    FImprimePagina: AnsiString;
    FLigaAlturaDupla: AnsiString;
    FLigaInvertido: AnsiString;
    FFonteNormal: AnsiString;
    FLigaCondensado: AnsiString;
    FCorteTotal: AnsiString;
    FEspacoEntreLinhas: AnsiString;
    FLigaExpandido: AnsiString;
    FDesligaCondensado: AnsiString;
    FDesligaExpandido: AnsiString;
    FDesligaItalico: AnsiString;
    FDesligaNegrito: AnsiString;
    FDesligaSublinhado: AnsiString;
    FFonteA: AnsiString;
    FFonteB: AnsiString;
    FLigaItalico: AnsiString;
    FLigaModoPagina: AnsiString;
    FLigaNegrito: AnsiString;
    FLigaSublinhado: AnsiString;
    FPuloDeLinha: AnsiString;
    FZera: AnsiString;
  public
    constructor Create;

    property Zera: AnsiString read FZera write FZera;
    property EspacoEntreLinhas: AnsiString read FEspacoEntreLinhas
      write FEspacoEntreLinhas;
    property EspacoEntreLinhasPadrao: AnsiString
      read FEspacoEntreLinhasPadrao write FEspacoEntreLinhasPadrao;

    property LigaNegrito: AnsiString read FLigaNegrito write FLigaNegrito;
    property DesligaNegrito: AnsiString read FDesligaNegrito write FDesligaNegrito;
    property LigaExpandido: AnsiString read FLigaExpandido write FLigaExpandido;
    property DesligaExpandido: AnsiString read FDesligaExpandido write FDesligaExpandido;
    property LigaAlturaDupla: AnsiString read FLigaAlturaDupla write FLigaAlturaDupla;
    property DesligaAlturaDupla: AnsiString read FDesligaAlturaDupla write FDesligaAlturaDupla;
    property LigaSublinhado: AnsiString read FLigaSublinhado write FLigaSublinhado;
    property DesligaSublinhado: AnsiString read FDesligaSublinhado
      write FDesligaSublinhado;
    property LigaItalico: AnsiString read FLigaItalico write FLigaItalico;
    property DesligaItalico: AnsiString read FDesligaItalico write FDesligaItalico;
    property LigaCondensado: AnsiString read FLigaCondensado write FLigaCondensado;
    property DesligaCondensado: AnsiString read FDesligaCondensado
      write FDesligaCondensado;
    property LigaInvertido: AnsiString read FLigaInvertido write FLigaInvertido;
    property DesligaInvertido: AnsiString read FDesligaInvertido write FDesligaInvertido;

    property FonteNormal: AnsiString read FFonteNormal write FFonteNormal;
    property FonteA: AnsiString read FFonteA write FFonteA;
    property FonteB: AnsiString read FFonteB write FFonteB;

    property AlinhadoEsquerda: AnsiString read FAlinhadoEsquerda write FAlinhadoEsquerda;
    property AlinhadoDireita: AnsiString read FAlinhadoDireita write FAlinhadoDireita;
    property AlinhadoCentro: AnsiString read FAlinhadoCentro write FAlinhadoCentro;

    property Beep: AnsiString read FBeep write FBeep;
    property CorteTotal: AnsiString read FCorteTotal write FCorteTotal;
    property CorteParcial: AnsiString read FCorteParcial write FCorteParcial;
    property PuloDeLinha: AnsiString read FPuloDeLinha write FPuloDeLinha;

    property LigaModoPagina: AnsiString read FLigaModoPagina write FLigaModoPagina;
    property DesligaModoPagina: AnsiString read FDesligaModoPagina write FDesligaModoPagina;
    property ImprimePagina: AnsiString read FImprimePagina write FImprimePagina;
  end;

  { TACBrPosRazaoColunaFonte }
  TACBrPosRazaoColunaFonte = class
  private
    FCondensada: Double;
    FExpandida: Double;
  public
    constructor Create;
  published
    property Condensada: Double read FCondensada write FCondensada;
    property Expandida: Double read FExpandida write FExpandida;
  end;

  { TACBrPosPrinterClass }

  TACBrPosPrinterClass = class
  private
    FCmd: TACBrPosComandos;
    FRazaoColunaFonte: TACBrPosRazaoColunaFonte;
    FTagsNaoSuportadas: TStringList;

  protected
    fpModeloStr: String;
    fpOwner: TComponent;

  public
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString): AnsiString; virtual;
    function ComandoCodBarras(const ATag: String; const ACodigo: AnsiString): AnsiString; virtual;
    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; virtual;
    function ComandoEspacoEntreLinhas(Espacos: byte): AnsiString; virtual;
    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString; virtual;
    function ComandoGaveta(NumGaveta: Integer = 1): AnsiString; virtual;
    function ComandoInicializa: AnsiString; virtual;
    function ComandoPuloLinhas(NLinhas: Integer): AnsiString; virtual;
    function ComandoFonte(TipoFonte: TACBrPosTipoFonte; Ligar: Boolean): AnsiString; virtual;
    function ComandoConfiguraModoPagina: AnsiString; virtual;

    procedure Configurar; virtual;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); virtual;
    function LerInfo: String; virtual;

    function ComandoImprimirImagemRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; virtual;
    function ComandoImprimirImagemArquivo(ArquivoBMP: String): AnsiString;
    function ComandoImprimirImagemStream(ABMPStream: TStream): AnsiString;

    function ComandoLogo: AnsiString; virtual;
    function ComandoGravarLogoRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; virtual;
    function ComandoGravarLogoArquivo(ArquivoBMP: String): AnsiString;
    function ComandoGravarLogoStream(ABMPStream: TStream): AnsiString;
    function ComandoApagarLogo: AnsiString; virtual;

    procedure ArquivoImagemToRasterStr(ArquivoImagem: String; out AWidth: Integer;
      out AHeight: Integer; out ARasterStr: AnsiString);

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property RazaoColunaFonte: TACBrPosRazaoColunaFonte read FRazaoColunaFonte;
    property Cmd: TACBrPosComandos read FCmd;
    property ModeloStr: String read fpModeloStr;

    property TagsNaoSuportadas: TStringList read FTagsNaoSuportadas;
  end;


implementation
uses
  strutils,
  {$IfNDef NOGUI}
    {$IfDef FMX}
      FMX.Graphics,
    {$Else}
      Graphics,
    {$EndIf}
  {$EndIf}
  ACBrPosPrinter, ACBrImage, ACBrUtil, ACBrConsts;

{ TACBrPosComandos }

constructor TACBrPosComandos.Create;
begin
  inherited;
  FPuloDeLinha := sLineBreak;
end;

{ TACBrPosRazaoColunaFonte }

constructor TACBrPosRazaoColunaFonte.Create;
begin
  FCondensada := 0.75;
  FExpandida := 2;
end;

{ TACBrPosPrinterClass }

constructor TACBrPosPrinterClass.Create(AOwner: TComponent);
begin
  inherited Create;
  if not (AOwner is TACBrPosPrinter) then
    raise EPosPrinterException.Create('Essa Classe deve ser instanciada por TACBrPosPrinter');

  fpModeloStr := 'Texto';
  fpOwner := AOwner;

  FCmd := TACBrPosComandos.Create;
  FRazaoColunaFonte := TACBrPosRazaoColunaFonte.Create;
  FTagsNaoSuportadas := TStringList.Create;
end;

destructor TACBrPosPrinterClass.Destroy;
begin
  FCmd.Free;
  FRazaoColunaFonte.Free;
  FTagsNaoSuportadas.Free;

  inherited;
end;

function TACBrPosPrinterClass.TraduzirTagBloco(
  const ATag, ConteudoBloco: AnsiString): AnsiString;
begin
  Result := ConteudoBloco;
end;

function TACBrPosPrinterClass.ComandoCodBarras(const ATag: String;
  const ACodigo: AnsiString): AnsiString;
begin
  Result := ACodigo;
end;

function TACBrPosPrinterClass.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoEspacoEntreLinhas(Espacos: byte): AnsiString;
begin
  if Espacos = 0 then
    Result := Cmd.EspacoEntreLinhasPadrao
  else
  begin
    if Length(Cmd.EspacoEntreLinhas) > 0 then
      Result := Cmd.EspacoEntreLinhas + AnsiChr(Espacos)
    else
      Result := '';
  end;
end;

function TACBrPosPrinterClass.ComandoPaginaCodigo(
  APagCodigo: TACBrPosPaginaCodigo): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGaveta(NumGaveta: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoInicializa: AnsiString;
begin
  with TACBrPosPrinter(fpOwner) do
  begin
    Result := ComandoEspacoEntreLinhas(EspacoEntreLinhas) +
              ComandoPaginaCodigo(PaginaDeCodigo);
  end;
end;

function TACBrPosPrinterClass.ComandoPuloLinhas(NLinhas: Integer): AnsiString;
begin
  Result := AnsiString( DupeString(' '+Cmd.PuloDeLinha,NLinhas) );
end;

function TACBrPosPrinterClass.ComandoFonte(TipoFonte: TACBrPosTipoFonte;
  Ligar: Boolean): AnsiString;
begin
  Result := '';

  case TipoFonte of
    ftExpandido:
      if Ligar then
        Result := Cmd.LigaExpandido
      else
        Result := Cmd.DesligaExpandido;

    ftAlturaDupla:
      if Ligar then
        Result := Cmd.LigaAlturaDupla
      else
        Result := Cmd.DesligaAlturaDupla;

    ftCondensado:
      if Ligar then
        Result := Cmd.LigaCondensado
      else
        Result :=  Cmd.DesligaCondensado;

    ftNegrito:
      if Ligar then
        Result := Cmd.LigaNegrito
      else
        Result := Cmd.DesligaNegrito;

    ftItalico:
      if Ligar then
        Result := Cmd.LigaItalico
      else
         Result := Cmd.DesligaItalico;

    ftInvertido:
       if Ligar then
         Result := Cmd.LigaInvertido
       else
         Result := Cmd.DesligaInvertido;

    ftSublinhado:
       if Ligar then
         Result := Cmd.LigaSublinhado
       else
         Result := Cmd.DesligaSublinhado;

    ftFonteB:
      if Ligar then
        Result := Cmd.FonteB
      else
        Result := Cmd.FonteA;
  end;
end;

function TACBrPosPrinterClass.ComandoConfiguraModoPagina: AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoImprimirImagemRasterStr(const RasterStr: AnsiString;
  AWidth: Integer; AHeight: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoImprimirImagemArquivo(ArquivoBMP: String): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  ArquivoImagemToRasterStr(ArquivoBMP, AWidth, AHeight, ARasterStr);
  Result := ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoImprimirImagemStream(ABMPStream: TStream
  ): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  BMPMonoToRasterStr(ABMPStream, True, AWidth, AHeight, ARasterStr );

  Result := ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoLogo: AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGravarLogoRasterStr(const RasterStr: AnsiString;
  AWidth: Integer; AHeight: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGravarLogoArquivo(ArquivoBMP: String): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  ArquivoImagemToRasterStr(ArquivoBMP, AWidth, AHeight, ARasterStr);
  Result := ComandoGravarLogoRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoGravarLogoStream(ABMPStream: TStream
  ): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  BMPMonoToRasterStr(ABMPStream, True, AWidth, AHeight, ARasterStr );

  Result := ComandoGravarLogoRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoApagarLogo: AnsiString;
begin
  Result := '';
end;

procedure TACBrPosPrinterClass.ArquivoImagemToRasterStr(ArquivoImagem: String; out
  AWidth: Integer; out AHeight: Integer; out ARasterStr: AnsiString);
var
  {$IfNDef NOGUI}
   ABitMap: TBitmap;
  {$Else}
   MS: TMemoryStream;
  {$EndIf}
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  if (Trim(ArquivoImagem) = '') then
    Exit;

  if not FileExists(ArquivoImagem) then
    raise EPosPrinterException.Create(ACBrStr(Format(cACBrArquivoNaoEncontrado,[ArquivoImagem])));

  {$IfNDef NOGUI}
   ABitMap := TBitmap.Create;
   try
     ABitMap.LoadFromFile(ArquivoImagem);
     BitmapToRasterStr(ABitMap, True, AWidth, AHeight, ARasterStr);
   finally
     ABitMap.Free;
   end;
  {$Else}
   MS := TMemoryStream.Create;
   try
     MS.LoadFromFile(ArquivoBMP);
     BMPMonoToRasterStr(MS, True, AWidth, AHeight, ARasterStr );
   finally
     MS.Free;
   end;
  {$EndIf}
end;

procedure TACBrPosPrinterClass.Configurar;
begin
  {nada aqui, método virtual}
end;

procedure TACBrPosPrinterClass.LerStatus(var AStatus: TACBrPosPrinterStatus);
begin
  {nada aqui, método virtual}
end;

function TACBrPosPrinterClass.LerInfo: String;
begin
  Result := '';
end;


end.

