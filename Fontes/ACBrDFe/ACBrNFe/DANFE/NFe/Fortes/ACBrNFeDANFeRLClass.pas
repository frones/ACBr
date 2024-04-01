{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Peterson de Cerqueira Matos                     }
{                              Wemerson Souto                                  }
{                              Daniel Simoes de Almeida                        }
{                              André Ferreira de Moraes                        }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}
unit ACBrNFeDANFeRLClass;

interface

uses
  SysUtils, Classes, Graphics,
  ACBrBase, pcnNFe, ACBrNFeDANFEClass, pcnConversao;

type
  TNomeFonte = (nfTimesNewRoman, nfCourierNew, nfArial);

  { TFonte }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TFonte = class(TComponent)
  protected
    FNome: TNomeFonte;
    FNegrito: Boolean;
    FTamanhoFonteRazaoSocial: Integer;
    FTamanhoFonteEndereco: Integer;
    FTamanhoFonteInformacoesComplementares: Integer;
    FTamanhoFonteDemaisCampos: Integer;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Nome: TNomeFonte read FNome write FNome default nfTimesNewRoman;
    property Negrito: Boolean read FNegrito write FNegrito default False;
    property TamanhoFonteRazaoSocial: Integer read FTamanhoFonteRazaoSocial write FTamanhoFonteRazaoSocial default 8;
    property TamanhoFonteEndereco: Integer read FTamanhoFonteEndereco write FTamanhoFonteEndereco default 0;
    property TamanhoFonteInformacoesComplementares: Integer read FTamanhoFonteInformacoesComplementares write FTamanhoFonteInformacoesComplementares default 8;
    property TamanhoFonteDemaisCampos: Integer read FTamanhoFonteDemaisCampos write FTamanhoFonteDemaisCampos default 8;
  end;

  { TACBrNFeDANFeRL }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFeDANFeRL = class(TACBrNFeDANFEClass)
  protected
    FExibeEAN: Boolean;

    procedure SetExibeEAN(AValue: Boolean); virtual;
    procedure SetTipoDANFE(AValue: TpcnTipoImpressao); override;
  private
    FMarcadagua: String;
    FLarguraCodProd: Integer;
    FFonte: TFonte;
    FAltLinhaComun: Integer;
    FEspacoEntreProdutos: Integer;
    FAlternaCoresProdutos: Boolean;
    FCorDestaqueProdutos: TColor;
    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;
    FRecuoEndereco: Integer;
    FRecuoEmpresa: Integer;
    FLogoEmCima: Boolean;
    FRecuoLogo: Integer;
    FImprimeContinuacaoDadosAdicionaisPrimeiraPagina: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(ANFe: TNFe = nil); override;
    procedure ImprimirDANFECancelado(ANFe: TNFe = nil); override;
    procedure ImprimirDANFEPDF(ANFe: TNFe = nil); override;
    procedure ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirEVENTO(ANFe: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(ANFe: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAO(ANFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(ANFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(AStream: TStream; ANFe: TNFe = nil); override;

  published
    property MarcadAgua: String read FMarcadagua write FMarcadagua;
    property LarguraCodProd: Integer read FLarguraCodProd write FLarguraCodProd default 54;
    property Fonte: TFonte read FFonte;
    property ExibeEAN: Boolean read FExibeEAN write SetExibeEAN default False;
    {@prop AltLinhaComun - Alturas das linhas mais comuns do Danfe.
     @links TACBrNFeDANFeRL.AltLinhaComun :/}
    property AltLinhaComun: Integer read FAltLinhaComun write FAltLinhaComun default 30;
    {@prop EspacoEntreProdutos - Altura dos espaços entre os produtos.
     @links TACBrNFeDANFeRL.EspacoEntreProdutos :/}
    property EspacoEntreProdutos: Integer read FEspacoEntreProdutos write FEspacoEntreProdutos default 7;
    {@prop AlternaCoresProdutos - Alterna as cores de fundo dos produtos para destaca-los.
     @links TACBrNFeDANFeRL.AlternaCoresProdutos :/}
    property AlternaCoresProdutos: Boolean read FAlternaCoresProdutos write FAlternaCoresProdutos default False;
    {@prop CorDestaqueProdutos - Cor usada para destacar produtos na lista alternando entre fundo coloridos e não colorido.
     @links TACBrNFeDANFeRL.CorDestaqueProdutos :/}
    property CorDestaqueProdutos: TColor read FCorDestaqueProdutos write FCorDestaqueProdutos default clWhite;
    property TamanhoLogoHeight: Integer read FTamanhoLogoHeight write FTamanhoLogoHeight default 0;
    property TamanhoLogoWidth: Integer read FTamanhoLogoWidth write FTamanhoLogoWidth default 0;
    property RecuoEndereco: Integer read FRecuoEndereco write FRecuoEndereco default 0;
    property RecuoEmpresa: Integer read FRecuoEmpresa write FRecuoEmpresa default 0;
    property LogoemCima: Boolean read FLogoEmCima write FLogoEmCima default False;
    property RecuoLogo: Integer read FRecuoLogo write FRecuoLogo default 0;
    property ImprimeContinuacaoDadosAdicionaisPrimeiraPagina: Boolean read FImprimeContinuacaoDadosAdicionaisPrimeiraPagina write FImprimeContinuacaoDadosAdicionaisPrimeiraPagina default False;
  end;

implementation

uses
  synautil, ACBrNFe,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrNFeNotasFiscais, pcnEnvEventoNFe,
  ACBrNFeDANFeRL, ACBrNFeDANFeEventoRL,
  ACBrNFeDANFeRLRetrato, ACBrNFeDANFeRLPaisagem,
  ACBrNFeDANFeEventoRLRetrato, ACBrNFeDANFeRLSimplificado,
  ACBrNFeDAInutRLRetrato;

{ TFonte }

constructor TFonte.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNome := nfTimesNewRoman;
  FNegrito := False;
  FTamanhoFonteRazaoSocial := 8;
  FTamanhoFonteEndereco := 0;
  FTamanhoFonteDemaisCampos := 8;
  FTamanhoFonteInformacoesComplementares := 8;
end;

{ TACBrNFeDANFeRL }

constructor TACBrNFeDANFeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFonte      := TFonte.Create(self);
  FFonte.Name := 'Fonte';

  {$IFDEF COMPILER6_UP}
  FFonte.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  FLarguraCodProd       := 54;
  FExibeEAN             := False;
  FAltLinhaComun        := 30;
  FEspacoEntreProdutos  := 7;
  FAlternaCoresProdutos := False;
  FCorDestaqueProdutos  := clWhite;
  FTamanhoLogoHeight    := 0;
  FTamanhoLogoWidth     := 0;
  FRecuoEndereco        := 0;
  FRecuoEmpresa         := 0;
  FLogoEmCima           := False;
  FRecuoLogo            := 0;
  FImprimeContinuacaoDadosAdicionaisPrimeiraPagina := False;
end;

destructor TACBrNFeDANFeRL.Destroy;
begin
  FFonte.Free;
  inherited Destroy;
end;

procedure TACBrNFeDANFeRL.SetExibeEAN(AValue: Boolean);
begin
  if TipoDANFE = tiRetrato then
    FExibeEAN := False
  else
    FExibeEAN := AValue;
end;

procedure TACBrNFeDANFeRL.SetTipoDANFE(AValue: TpcnTipoImpressao);
begin
  inherited SetTipoDANFE(AValue);

  if (TipoDANFE = tiRetrato) then
    FExibeEAN := False;
end;

procedure TACBrNFeDANFeRL.ImprimirDANFE(ANFe: TNFe = nil);
var
  i: Integer;
  Notas: array of TNFe;
begin
  if (ANFe = nil) then
  begin
    SetLength(Notas, TACBrNFe(ACBrNFe).NotasFiscais.Count);

    for i := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
      Notas[i] := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe;
  end
  else
  begin
    SetLength(Notas, 1);
    Notas[0] := ANFe;
  end;

  case TipoDANFE of
    tiPaisagem:
      TfrlDANFeRLPaisagem.Imprimir(Self, Notas);
    tiSimplificado:
      TfrlDANFeRLSimplificado.Imprimir(Self, Notas);
  else
    TfrlDANFeRLRetrato.Imprimir(Self, Notas);
  end;
end;

procedure TACBrNFeDANFeRL.ImprimirDANFECancelado(ANFe: TNFe = nil);
begin
  Cancelada := True;
  ImprimirDANFE(ANFe);
  Cancelada := False;
end;

procedure TACBrNFeDANFeRL.ImprimirDANFEPDF(ANFe: TNFe = nil);
var
  i: Integer;
  ArqPDF: String;

  function ImprimirDANFEPDFTipo(ANFe: TNFe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(ANFe.infNFe.ID) + '-nfe.pdf',
                                 Self.NomeDocumento);

    case Self.TipoDANFE of
      tiPaisagem:
        TfrlDANFeRLPaisagem.SalvarPDF(Self, ANFe, Result);
      tiSimplificado:
        TfrlDANFeRLSimplificado.SalvarPDF(Self, ANFe, Result);
    else
      TfrlDANFeRLRetrato.SalvarPDF(Self, ANFe, Result);
    end;
  end;

begin
  FPArquivoPDF := '';

  if (ANFe = nil) then
  begin
    for i := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
    begin
      ArqPDF := ImprimirDANFEPDFTipo(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe);

      FPArquivoPDF := FPArquivoPDF + ArqPDF;
      if (i < (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1)) then
        FPArquivoPDF := FPArquivoPDF + sLinebreak;
    end;
  end
  else
    FPArquivoPDF := ImprimirDANFEPDFTipo(ANFe);
end;

procedure TACBrNFeDANFeRL.ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil);
var
  i: Integer;

  procedure StreamDANFEPDFTipo(ANFe: TNFe; const AStream: TStream);
  begin
    AStream.Size := 0;
    case Self.TipoDANFE of
      tiPaisagem:
        TfrlDANFeRLPaisagem.SalvarPDF(Self, ANFe, AStream);
      tiSimplificado:
        TfrlDANFeRLSimplificado.SalvarPDF(Self, ANFe, AStream);
    else
      TfrlDANFeRLRetrato.SalvarPDF(Self, ANFe, AStream);
    end;
  end;

begin
  if not Assigned(AStream) then
    raise EACBrNFeException.Create('AStream precisa estar definido');

  if (ANFe = nil) then
  begin
    for i := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
      StreamDANFEPDFTipo(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe, AStream);
  end
  else
    StreamDANFEPDFTipo(ANFe, AStream);
end;

procedure TACBrNFeDANFeRL.ImprimirEVENTO(ANFe: TNFe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrNFe(ACBrNFe) do
  begin
    if (ANFe = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNFe.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NFe.infNFe.ID);
          if (NumID = OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNFe)) then
          begin
            TfrlDANFeEventoRLRetrato.Imprimir(Self, EventoNFe.Evento.Items[i], NotasFiscais.Items[j].NFe);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso then
          TfrlDANFeEventoRLRetrato.Imprimir(Self, EventoNFe.Evento.Items[i]);
      end;
    end
    else
    begin
      for i := 0 to (EventoNFe.Evento.Count - 1) do
        TfrlDANFeEventoRLRetrato.Imprimir(Self, EventoNFe.Evento.Items[i], ANFe);
    end;
  end;
end;

procedure TACBrNFeDANFeRL.ImprimirEVENTOPDF(ANFe: TNFe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID, ArqPDF: String;

  function ImprimirEVENTOPDFTipo(EventoNFeItem: TInfEventoCollectionItem; ANFe: TNFe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(EventoNFeItem.InfEvento.id) + '-procEventoNFe.pdf',
                                 Self.NomeDocumento);

    // TipoDANFE ainda não está sendo utilizado no momento
    TfrlDANFeEventoRLRetrato.SalvarPDF(Self, EventoNFeItem, Result, ANFe);
  end;

begin
  FPArquivoPDF := '';

  with TACBrNFe(ACBrNFe) do
  begin
    if (ANFe = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNFe.Evento.Count - 1) do
      begin
        Impresso := False;
        ArqPDF := '';
        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NFe.infNFe.ID);
          if (NumID = OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNFe)) then
          begin
            ArqPDF := ImprimirEVENTOPDFTipo(EventoNFe.Evento.Items[i], NotasFiscais.Items[j].NFe);
            Impresso := True;
            Break;
          end;
        end;

        if (not Impresso) then
          ArqPDF := ImprimirEVENTOPDFTipo(EventoNFe.Evento.Items[i], nil);

        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoNFe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end
    else
    begin
      for i := 0 to (EventoNFe.Evento.Count - 1) do
      begin
        ArqPDF := ImprimirEVENTOPDFTipo(EventoNFe.Evento.Items[i], ANFe);
        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoNFe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end;
  end;
end;

procedure TACBrNFeDANFeRL.ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrNFe(ACBrNFe) do
  begin
    if (ANFe = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNFe.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NFe.infNFe.ID);
          if (NumID = OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNFe)) then
          begin
            TfrlDANFeEventoRLRetrato.SalvarPDF(Self, EventoNFe.Evento.Items[i], AStream, NotasFiscais.Items[j].NFe);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso and (EventoNFe.Evento.Count > 0) then
          TfrlDANFeEventoRLRetrato.SalvarPDF(Self, EventoNFe.Evento.Items[0], AStream, nil);
      end;
    end
    else
    begin
      NumID := OnlyNumber(ANFe.infNFe.ID);
      Impresso := False;

      for i := 0 to (EventoNFe.Evento.Count - 1) do
      begin
        if (NumID = OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNFe)) then
        begin
          TfrlDANFeEventoRLRetrato.SalvarPDF(Self, EventoNFe.Evento.Items[i], AStream, ANFe);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso and (EventoNFe.Evento.Count > 0) then
        TfrlDANFeEventoRLRetrato.SalvarPDF(Self, EventoNFe.Evento.Items[0], AStream, nil);
    end;
  end;
end;

procedure TACBrNFeDANFeRL.ImprimirINUTILIZACAO(ANFe: TNFe);
begin
  TfrmNFeDAInutRLRetrato.Imprimir(Self, TACBrNFe(ACBrNFe).InutNFe, ANFe);
end;

procedure TACBrNFeDANFeRL.ImprimirINUTILIZACAOPDF(ANFe: TNFe);
begin
  FPArquivoPDF := DefinirNomeArquivo(Self.PathPDF,
                                     OnlyNumber(TACBrNFe(ACBrNFe).InutNFe.ID) + '-procInutNFe.pdf',
                                     Self.NomeDocumento);

  TfrmNFeDAInutRLRetrato.SalvarPDF(Self, TACBrNFe(ACBrNFe).InutNFe, FPArquivoPDF, ANFe);
end;

procedure TACBrNFeDANFeRL.ImprimirINUTILIZACAOPDF(AStream: TStream; ANFe: TNFe);
begin
  TfrmNFeDAInutRLRetrato.SalvarPDF(Self, TACBrNFe(ACBrNFe).InutNFe, AStream, ANFe);
end;

end.
