{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Fabio Pasquali                                  }
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
{                                                                              }
{ O formato desta nota segue o padrão emitido pela NotaControl no DF           }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeXDANFSeRLISSNet;

interface

uses
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  RLFilters,
  RLPDFFilter,
  RLReport,
  ACBrDelphiZXingQRCode,
  ACBrNFSeXConversao,
  ACBrNFSeXDANFSeRL, ACBrBase, ACBrDFe, ACBrNFSeX, Types;

type

  { TfrlXDANFSeRLRetrato }

  { TfrlXDANFSeRLISSnet }

  TfrlXDANFSeRLISSnet = class(TfrlXDANFSeRL)
    rlbCabecalho: TRLBand;
    rllNumNF0: TRLLabel;
    rliLogo: TRLImage;
    rlbPrestador: TRLBand;
    RLLabel30: TRLLabel;
    RLLabel32: TRLLabel;
    rllPrestInscMunicipal: TRLLabel;
    rllPrestCNPJ: TRLLabel;
    rliPrestLogo: TRLImage;
    rllPrestNome: TRLLabel;
    rlbTomador: TRLBand;
    rllTomaCNPJ: TRLLabel;
    RLLabel11: TRLLabel;
    rllTomaInscMunicipal: TRLLabel;
    RLLabel15: TRLLabel;
    rllTomaNome: TRLLabel;
    RLLabel17: TRLLabel;
    rllTomaEndereco: TRLLabel;
    RLLabel19: TRLLabel;
    rllTomaMunicipio: TRLLabel;
    RLLabel21: TRLLabel;
    rllTomaUF: TRLLabel;
    RLLabel10: TRLLabel;
    rllTomaEmail: TRLLabel;
    RLLabel25: TRLLabel;
    rllTomaComplemento: TRLLabel;
    RLLabel27: TRLLabel;
    rllTomaTelefone: TRLLabel;
    rlbHeaderItens: TRLBand;
    rlbItens: TRLBand;
    rlbISSQN: TRLBand;
    rllBaseCalc: TRLLabel;
    rllValorISS: TRLLabel;
    rllAliquota: TRLLabel;
    rllValorCOFINS: TRLLabel;
    rllValorIR: TRLLabel;
    rllValorINSS: TRLLabel;
    rllValorCSLL: TRLLabel;
    rllValorServicos1: TRLLabel;
    rllDescIncondicionado1: TRLLabel;
    rllDescCondicionado: TRLLabel;
    rllOutrasRetencoes: TRLLabel;
    rllValorIssRetido: TRLLabel;
    rllValorLiquido: TRLLabel;
    rllValorDeducoes: TRLLabel;
    rllISSReter: TRLLabel;
    rbOutrasInformacoes: TRLBand;
    rlmDadosAdicionais: TRLMemo;
    rlbCanhoto: TRLBand;
    RLLabel26: TRLLabel;
    rllPrestNomeEnt: TRLLabel;
    RLLabel28: TRLLabel;
    RLDraw1: TRLDraw;
    RLLabel57: TRLLabel;
    RLLabel33: TRLLabel;
    RLDraw5: TRLDraw;
    RLLabel58: TRLLabel;
    RLLabel59: TRLLabel;
    RLDraw7: TRLDraw;
    RLLabel61: TRLLabel;
    rllTomaInscEstadual: TRLLabel;
    rlmDescricao: TRLMemo;
    rlbHeaderItensDetalhado: TRLBand;
    RLLabel65: TRLLabel;
    RLLabel66: TRLLabel;
    RLLabel67: TRLLabel;
    RLLabel68: TRLLabel;
    subItens: TRLSubDetail;
    rlbItensServico: TRLBand;
    txtServicoQtde: TRLLabel;
    rlmServicoDescricao: TRLMemo;
    txtServicoUnitario: TRLLabel;
    txtServicoTotal: TRLLabel;
    RLLabel69: TRLLabel;
    rllPrestInscEstadual: TRLLabel;
    RLBand1: TRLBand;
    rllDataHoraImpressao: TRLLabel;
    rllSistema: TRLLabel;
    rlmPrefeitura1: TRLLabel;
    rlmPrefeitura2: TRLLabel;
    rlmPrefeitura3: TRLLabel;
    RLDraw18: TRLDraw;
    RLDraw19: TRLDraw;
    RLLabel73: TRLLabel;
    RLMemo1: TRLMemo;
    RLLabel74: TRLLabel;
    RLLabel75: TRLLabel;
    RLDraw20: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw22: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw24: TRLDraw;
    RLLabel1: TRLLabel;
    RLLabel22: TRLLabel;
    RLLabel31: TRLLabel;
    RLLabel76: TRLLabel;
    rlImgQrCode: TRLImage;
    rllEmissao: TRLLabel;
    rllCompetencia: TRLLabel;
    rllCodVerificacao: TRLLabel;
    RLDraw2: TRLDraw;
    RLLabel7: TRLLabel;
    RLLabel4: TRLLabel;
    rllTomaCEP: TRLLabel;
    RLLabel8: TRLLabel;
    rlbDadosNota: TRLBand;
    RLDraw3: TRLDraw;
    RLLabel88: TRLLabel;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw25: TRLDraw;
    RLLabel12: TRLLabel;
    RLLabel13: TRLLabel;
    rllNumeroRPS: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel18: TRLLabel;
    RLLabel20: TRLLabel;
    rllDataRPS: TRLLabel;
    RLLabel60: TRLLabel;
    rllLocalServico: TRLLabel;
    RLLabel62: TRLLabel;
    rllMunicipioIncidencia: TRLLabel;
    RLLabel44: TRLLabel;
    RLDraw13: TRLDraw;
    RLDraw26: TRLDraw;
    RLDraw27: TRLDraw;
    RLLabel63: TRLLabel;
    rllAtividade: TRLLabel;
    RLDraw28: TRLDraw;
    RLLabel64: TRLLabel;
    RLDraw29: TRLDraw;
    RLLabel77: TRLLabel;
    rllItem: TRLLabel;
    RLLabel78: TRLLabel;
    rllCodNBS: TRLLabel;
    RLDraw30: TRLDraw;
    RLLabel79: TRLLabel;
    rllCodCNAE: TRLLabel;
    RLDraw31: TRLDraw;
    RLLabel81: TRLLabel;
    RLDraw32: TRLDraw;
    RLLabel47: TRLLabel;
    RLDraw34: TRLDraw;
    RLLabel82: TRLLabel;
    RLDraw35: TRLDraw;
    RLLabel83: TRLLabel;
    RLDraw36: TRLDraw;
    RLLabel84: TRLLabel;
    RLDraw37: TRLDraw;
    RLLabel85: TRLLabel;
    RLDraw33: TRLDraw;
    RLLabel80: TRLLabel;
    RLLabel94: TRLLabel;
    RLDraw39: TRLDraw;
    RLLabel95: TRLLabel;
    RLDraw40: TRLDraw;
    RLLabel96: TRLLabel;
    RLDraw41: TRLDraw;
    RLLabel97: TRLLabel;
    RLDraw42: TRLDraw;
    RLLabel98: TRLLabel;
    RLDraw43: TRLDraw;
    RLLabel99: TRLLabel;
    RLDraw44: TRLDraw;
    RLLabel100: TRLLabel;
    rllValorPIS: TRLLabel;
    RLDraw45: TRLDraw;
    RLLabel86: TRLLabel;
    rllNatOperacao: TRLLabel;
    RLLabel3: TRLLabel;
    RLDraw4: TRLDraw;
    rllPrestFantasia: TRLLabel;
    rbConstrucao: TRLBand;
    rllTituloConstCivil: TRLLabel;
    rllCodigoObra: TRLLabel;
    rllCodObra: TRLLabel;
    rllCodigoArt: TRLLabel;
    rllCodART: TRLLabel;
    rllMsgTeste: TRLLabel;
    rllSite: TRLLabel;
    rllPrestEndereco: TRLMemo;
    lbIdentificacao: TRLLabel;
    rllTomadorNomeEnt: TRLLabel;
    rllNumNF0Ent: TRLLabel;
    rllRespRetencao: TRLLabel;

    procedure rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensServicoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbPrestadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbTomadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbISSQNBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rbOutrasInformacoesBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure RLNFSeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo: Integer;
      CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure rlbDadosNotaBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rbConstrucaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
    FNumItem: Integer;
  public
    { Public declarations }
    class procedure QuebradeLinha(const sQuebradeLinha: String); override;
  end;

var
  frlXDANFSeRLISSnet: TfrlXDANFSeRLISSnet;

implementation

uses
  StrUtils, DateUtils,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrDFeUtil,
  ACBrNFSeXClass, ACBrNFSeXInterface,
  ACBrValidador, ACBrDFeReportFortes;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

var
  FQuebradeLinha: String;

  { TfrlXDANFSeRLRetrato }

class procedure TfrlXDANFSeRLISSnet.QuebradeLinha(const sQuebradeLinha: String);
begin
  FQuebradeLinha := sQuebradeLinha;
end;

procedure TfrlXDANFSeRLISSnet.rbConstrucaoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  with fpNFSe do
  begin
    rllCodObra.Caption := ConstrucaoCivil.CodigoObra;
    rllCodART.Caption := ConstrucaoCivil.Art;
  end;

  rllMsgTeste.Visible := (fpDANFSe.Producao = snNao);
  rllMsgTeste.Enabled := (fpDANFSe.Producao = snNao);

  if fpDANFSe.Cancelada or (fpNFSe.NfseCancelamento.DataHora <> 0) or
    (fpNFSe.SituacaoNfse = snCancelado) or (fpNFSe.StatusRps = srCancelado) then
  begin
    rllMsgTeste.Caption := 'NFS-e CANCELADA';
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end;

  rllMsgTeste.Repaint;

  PrintIt := (rllCodObra.Caption <> '') or (rllCodART.Caption <> '') or rllMsgTeste.Visible;
end;

procedure TfrlXDANFSeRLISSnet.rbOutrasInformacoesBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  rlmDadosAdicionais.Lines.BeginUpdate;
  rlmDadosAdicionais.Lines.Clear;

  if fpDANFSe.OutrasInformacaoesImp <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(fpDANFSe.OutrasInformacaoesImp, ';', #13#10, [rfReplaceAll]))
  else
    if fpNFSe.OutrasInformacoes <> '' then
      rlmDadosAdicionais.Lines.Add(StringReplace(fpNFSe.OutrasInformacoes, FQuebradeLinha, #13#10, [rfReplaceAll]));

  if fpNFSe.InformacoesComplementares <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(fpNFSe.InformacoesComplementares, FQuebradeLinha, #13#10, [rfReplaceAll]));

  rlmDadosAdicionais.Lines.EndUpdate;
  rllDataHoraImpressao.Caption := Format(ACBrStr('DATA E HORA DA IMPRESSÃO: %s'), [FormatDateTime('dd/mm/yyyy hh:nn', Now)]);

  if fpDANFSe.Usuario <> '' then
    rllDataHoraImpressao.Caption := Format(ACBrStr('%s   USUÁRIO: %s'), [rllDataHoraImpressao.Caption, fpDANFSe.Usuario]);

  // imprime sistema
  if fpDANFSe.Sistema <> '' then
    rllSistema.Caption := Format('Desenvolvido por %s', [fpDANFSe.Sistema])
  else
    rllSistema.Caption := '';

  // Exibe canhoto
  rlbCanhoto.Visible := fpDANFSe.ImprimeCanhoto;
end;

procedure TfrlXDANFSeRLISSnet.RLBand1BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  rllSite.Caption := fpDANFSe.Site;
end;

procedure TfrlXDANFSeRLISSnet.rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  strPrefeitura: TSplitResult;
begin
  inherited;

  With fpNFSe do
  begin
    TDFeReportFortes.CarregarLogo(rliLogo, fpDANFSe.Logo);

    rllNumNF0.Caption := Numero;

    // Somente as 3 primeiras linhas serão utilizadas
    strPrefeitura := ACBrUtil.Strings.Split(';', fpDANFSe.Prefeitura);

    if (Length(strPrefeitura) >= 1) then
      rlmPrefeitura1.Caption := strPrefeitura[0];

    if (Length(strPrefeitura) >= 2) then
      rlmPrefeitura2.Caption := strPrefeitura[1];

    if (Length(strPrefeitura) >= 3) then
      rlmPrefeitura3.Caption := strPrefeitura[2];
  end;
end;

procedure TfrlXDANFSeRLISSnet.rlbDadosNotaBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  FProvider: IACBrNFSeXProvider;
begin
  inherited;

  FProvider := ACBrNFSe.Provider;

  With fpNFSe do
  begin
    rllNatOperacao.Caption := ACBrStr(FProvider.NaturezaOperacaoDescricao(NaturezaOperacao));
    rllNumeroRPS.Caption := IdentificacaoRps.Numero;
    rllDataRPS.Caption := FormatDateTime('dd/mm/yyyy', DataEmissaoRps);
    rllLocalServico.Caption := Servico.MunicipioPrestacaoServico;
    rllMunicipioIncidencia.Caption := Servico.xMunicipioIncidencia;
    rllRespRetencao.Caption := ACBrStr(FProvider.ResponsavelRetencaoDescricao(Servico.ResponsavelRetencao));
  end;
end;

procedure TfrlXDANFSeRLISSnet.rlbItensServicoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  with fpNFSe.Servico.ItemServico.Items[FNumItem] do
  begin
    txtServicoQtde.Caption := FormatFloatBr(Quantidade);
    rlmServicoDescricao.Lines.Clear;
    rlmServicoDescricao.Lines.Add(Descricao);
    txtServicoUnitario.Caption := FormatFloatBr(ValorUnitario);

    if ValorTotal = 0.0 then
      ValorTotal := Quantidade * ValorUnitario;

    txtServicoTotal.Caption := FormatFloatBr(ValorTotal);
  end;
end;

procedure TfrlXDANFSeRLISSnet.rlbISSQNBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  FProvider: IACBrNFSeXProvider;
begin
  inherited;

  FProvider := ACBrNFSe.Provider;

  with fpNFSe do
  begin
    rllAtividade.Caption := fpNFSe.DescricaoCodigoTributacaoMunicipio;
    rllItem.Caption := Servico.ItemListaServico;
    rllCodNBS.Caption := Servico.CodigoNBS;
    rllCodCNAE.Caption := Servico.CodigoCnae;

    rllAliquota.Caption := fpDANFSe.FormatarAliquota(Servico.Valores.Aliquota);
    rllValorServicos1.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorServicos);
    rllDescIncondicionado1.Caption := FormatCurr('R$ ,0.00', Servico.Valores.DescontoIncondicionado);
    rllValorDeducoes.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorDeducoes);
    rllBaseCalc.Caption := FormatCurr('R$ ,0.00', Servico.Valores.BaseCalculo);
    rllValorISS.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorIss);
    rllISSReter.Caption := FProvider.SituacaoTributariaDescricao(Servico.Valores.IssRetido);
    rllDescCondicionado.Caption := FormatCurr('R$ ,0.00', Servico.Valores.DescontoCondicionado);
    rllValorPIS.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorPis);
    rllValorCOFINS.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorCofins);
    rllValorIR.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorIr);
    rllValorINSS.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorInss);
    rllValorCSLL.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorCsll);
    rllOutrasRetencoes.Caption := FormatCurr('R$ ,0.00', Servico.Valores.OutrasRetencoes);
    rllValorIssRetido.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorIssRetido);
    rllValorLiquido.Caption := FormatCurr('R$ ,0.00', Servico.Valores.ValorLiquidoNfse);
  end;
end;

procedure TfrlXDANFSeRLISSnet.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmDescricao.Lines.Clear;
  rlmDescricao.Lines.Add(fpNFSe.Servico.Discriminacao);
end;

procedure TfrlXDANFSeRLISSnet.rlbPrestadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  QrCode: TDelphiZXingQRCode;
  QrCodeBitmap: TBitmap;
  QRCodeData: String;
  Row, Column: Integer;
  xEndereco, xNumero, xComplemento, xBairro, xMunic, xUF, xCEP, xFone,
  xEmail: string;
begin
  inherited;

  if FileExists(fpDANFSe.Prestador.Logo) then
     TDFeReportFortes.CarregarLogo(rliPrestLogo, fpDANFSe.Prestador.Logo)
  else
     rliPrestLogo.Visible:=False;

  with fpNFSe do
  begin
    rllEmissao.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', DataEmissao);
    rllCompetencia.Caption := IfThen(Competencia > 0, FormatDateTime('dd/mm/yyyy', Competencia), '');
    rllCodVerificacao.Caption := CodigoVerificacao;
  end;

  if not rliPrestLogo.Visible then
  begin
    rllPrestNome.Left := rliPrestLogo.left;
    rllPrestFantasia.Left := rliPrestLogo.left + 1;

    rllPrestNome.Caption := copy(trim(fpDANFSe.Prestador.RazaoSocial), 1, 58);
    rllPrestFantasia.Caption := copy(trim(fpDANFSe.Prestador.NomeFantasia), 1, 65);

    rllPrestEndereco.Left := rliPrestLogo.left + 10;

    RLLabel32.Left := rllPrestEndereco.LEFT;

    rllPrestCNPJ.Left := RLLabel32.left+RLLabel32.width;

    RLLabel30.Left := rllPrestEndereco.LEFT;
    rllPrestInscMunicipal.Left := RLLabel30.left + RLLabel30.width;
  end
  else
  begin
    rllPrestNome.Caption := copy(fpDANFSe.Prestador.RazaoSocial, 1, 44);
    rllPrestFantasia.Caption := copy(fpDANFSe.Prestador.NomeFantasia, 1, 50);
  end;

  rllPrestNomeEnt.Caption := rllPrestNome.Caption;

  rllPrestCNPJ.Caption := FormatarCNPJouCPF(fpDANFSe.Prestador.CNPJ);
  rllPrestInscMunicipal.Caption := fpDANFSe.Prestador.InscricaoMunicipal;
  rllPrestInscEstadual.Caption := fpDANFSe.Prestador.InscricaoEstadual;

  xEndereco := Trim(fpDANFSe.Prestador.Endereco);
  xNumero := Trim(fpDANFSe.Prestador.Numero);

  if xNumero <> '' then
    xEndereco := xEndereco + ', ' + xNumero;

  xComplemento := Trim(fpDANFSe.Prestador.Complemento);
  xBairro := Trim(fpDANFSe.Prestador.Bairro);
  xMunic := Trim(fpDANFSe.Prestador.Municipio);
  xUF := Trim(fpDANFSe.Prestador.UF);
  xCEP := Trim(fpDANFSe.Prestador.CEP);

  if xCEP <> '' then
    xCEP := ' CEP: ' + FormatarCEP(xCEP);

  xFone := Trim(fpDANFSe.Prestador.Fone);

  if xFone <> '' then
    xFone := 'Fone: ' + FormatarFone(xFone);

  xEmail := Trim(fpDANFSe.Prestador.Email);

  if xEmail <> '' then
    xEmail := 'e-mail: ' + xEmail;

  rllPrestEndereco.Lines.Text :=  xEndereco + ' ' + xComplemento + #13 +
    xBairro + ' - ' + xMunic + ' - ' + xUF + xCEP + #13 +
    xFone + ' ' + xEmail;

  if fpNFSe.Link <> '' then
  begin
    QRCodeData := fpNFSe.Link;
    QrCode := TDelphiZXingQRCode.Create;
    QrCodeBitmap := TBitmap.Create;

    try
      QrCode.Encoding := qrUTF8NoBOM;
      QrCode.QuietZone := 1;
      QrCode.Data := WideString(QRCodeData);

      QrCodeBitmap.Width := QrCode.Columns;
      QrCodeBitmap.Height := QrCode.Rows;

      for Row := 0 to QrCode.Rows - 1 do
        for Column := 0 to QrCode.Columns - 1 do
          if (QrCode.IsBlack[Row, Column]) then
            QrCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
          else
            QrCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;

      rlImgQrCode.Picture.Bitmap.Assign(QrCodeBitmap);
    finally
      QrCode.Free;
      QrCodeBitmap.Free;
    end;
  end;

  rllNumNF0Ent.Caption := FormatFloat('00000000000', StrToFloatDef(fpNFSe.Numero, 0));
  rllTomadorNomeEnt.Caption := ACBrStr('Emissão:') +
    FormatDateTime('dd/mm/yy', fpNFSe.DataEmissao) +
    '-Tomador:' + fpNFSe.Tomador.RazaoSocial +
    '-Total:' +
    FormatFloat(',0.00', fpNFSe.Servico.Valores.ValorLiquidoNfse);

end;

procedure TfrlXDANFSeRLISSnet.rlbTomadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  with fpNFSe.Tomador do
  begin
    rllTomaNome.Caption := RazaoSocial;

    lbIdentificacao.Caption := 'CPF/CNPJ:';
    if (Length(IdentificacaoTomador.Nif) > 0) then
    begin
      lbIdentificacao.Caption := 'NIF:';
      rllTomaCNPJ.Caption := IdentificacaoTomador.Nif;
    end
    else
    begin
      if Length(IdentificacaoTomador.CpfCnpj) <= 11 then
        rllTomaCNPJ.Caption := FormatarCPF(IdentificacaoTomador.CpfCnpj)
      else
        rllTomaCNPJ.Caption := FormatarCNPJ(IdentificacaoTomador.CpfCnpj);
    end;
    rllTomaInscMunicipal.Caption := IfThen(IdentificacaoTomador.InscricaoMunicipal <> '',
      IdentificacaoTomador.InscricaoMunicipal, fpDANFSe.Tomador.InscricaoMunicipal);

    rllTomaInscEstadual.Caption := IfThen(IdentificacaoTomador.InscricaoEstadual <> '',
      IdentificacaoTomador.InscricaoEstadual, fpDANFSe.Tomador.InscricaoEstadual);

    if Endereco.Endereco <> '' then
    begin
      if Endereco.UF = 'EX' then
      begin
        rllTomaEndereco.Caption := Trim(Endereco.Endereco) +
          ', Pais: ' + Trim(Endereco.xPais);
      end
      else
        rllTomaEndereco.Caption := Trim(Endereco.Endereco) + ', ' +
          Trim(Endereco.Numero) + ' - ' +
          Trim(Endereco.Bairro) + ' - CEP: ' +
          FormatarCEP(Endereco.CEP);
    end
    else
      rllTomaEndereco.Caption := Trim(fpDANFSe.Tomador.Endereco) + ' - CEP: ' +
        FormatarCEP(Endereco.CEP);

    rllTomaComplemento.Caption := IfThen(Endereco.Complemento <> '',
      Endereco.Complemento, fpDANFSe.Tomador.Complemento);
    rllTomaMunicipio.Caption := Endereco.xMunicipio;
    rllTomaUF.Caption := Endereco.UF;
    rllTomaCEP.Caption := FormatarCEP(Endereco.CEP);

    rllTomaTelefone.Caption := IfThen(Contato.Telefone <> '',
      FormatarFone(Contato.Telefone), FormatarFone(fpDANFSe.Tomador.Fone));
    rllTomaEmail.Caption := IfThen(Contato.Email <> '',
      Contato.Email, fpDANFSe.Tomador.Email);
  end;
end;

procedure TfrlXDANFSeRLISSnet.RLNFSeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  Detalhar: Boolean;
begin
  inherited;

  Detalhar := ACBrNFSe.Provider.ConfigGeral.DetalharServico;

  RLNFSe.Title := 'NFS-e: ' + fpNFSe.Numero;
  TDFeReportFortes.AjustarMargem(RLNFSe, fpDANFSe);
  rlbItens.Visible := not Detalhar;
  rlbHeaderItensDetalhado.Visible := Detalhar;
  subItens.Visible := Detalhar;

  // rlbItens.Visible := not (fpDANFSe.DetalharServico);
  // rlbHeaderItensDetalhado.Visible := fpDANFSe.DetalharServico;
  // subItens.Visible := fpDANFSe.DetalharServico;

  // Estudar a melhor forma de não especificar o provedor.
  RLLabel65.Visible := not (ACBrNFSe.Configuracoes.Geral.Provedor in [proSimple]);
  txtServicoQtde.Visible := not (ACBrNFSe.Configuracoes.Geral.Provedor in [proSimple]);
end;

procedure TfrlXDANFSeRLISSnet.subItensDataRecord(Sender: TObject;
  RecNo: Integer; CopyNo: Integer; var Eof: Boolean;
  var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  Eof := (RecNo > fpNFSe.Servico.ItemServico.Count);
  RecordAction := raUseIt;
end;

end.
