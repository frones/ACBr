{ ****************************************************************************** }
{ Projeto: Componentes ACBr                                                      }
{ Biblioteca multiplataforma de componentes Delphi para intera��o com equipa-    }
{ mentos de Automa��o Comercial utilizados no Brasil                             }
{                                                                                }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                 }
{                                                                                }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                           }
{                                                                                }
{ Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr       }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr        }
{                                                                                }
{ Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la    }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela    }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio)   }
{ qualquer vers�o posterior.                                                     }
{                                                                                }
{ Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM      }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU        }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor  }
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)                }
{                                                                                }
{ Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto   }
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,    }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.            }
{ Voc� tamb�m pode obter uma copia da licen�a em:                                }
{ http://www.opensource.org/licenses/lgpl-license.php                            }
{                                                                                }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br  }
{ Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170                 }
{ ****************************************************************************** }

{$I ACBr.inc}

unit ACBrNFSeXDANFSeRLRetrato;

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
  ACBrNFSeXDANFSeRL;

type

  { TfrlXDANFSeRLRetrato }

  TfrlXDANFSeRLRetrato = class(TfrlXDANFSeRL)
    rlbCabecalho: TRLBand;
    RLDraw10: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw3: TRLDraw;
    RLDraw70: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    rllNumNF0: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel12: TRLLabel;
    rliLogo: TRLImage;
    rllEmissao: TRLLabel;
    rllCodigoChave: TRLLabel;
    rllCodVerificacao: TRLLabel;
    RLLabel7: TRLLabel;
    rllCompetencia: TRLLabel;
    RLLabel18: TRLLabel;
    rllNumeroRps: TRLLabel;
    RLLabel20: TRLLabel;
    rllNumNFSeSubstituida: TRLLabel;
    rlmPrefeitura: TRLMemo;
    rlbPrestador: TRLBand;
    RLLabel30: TRLLabel;
    RLLabel32: TRLLabel;
    rllPrestInscMunicipal: TRLLabel;
    rllPrestEndereco: TRLMemo;
    rllPrestCNPJ: TRLLabel;
    rliPrestLogo: TRLImage;
    RLLabel2: TRLLabel;
    RLLabel1: TRLLabel;
    rllPrestNome: TRLLabel;
    rlbTomador: TRLBand;
    RLLabel4: TRLLabel;
    lbIdentificacao: TRLLabel;
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
    RLLabel14: TRLLabel;
    rlbItens: TRLBand;
    rlbISSQN: TRLBand;
    RLDraw52: TRLDraw;
    RLDraw53: TRLDraw;
    RLDraw54: TRLDraw;
    RLDraw55: TRLDraw;
    RLLabel137: TRLLabel;
    RLLabel138: TRLLabel;
    RLLabel139: TRLLabel;
    rllBaseCalc: TRLLabel;
    rllValorISS: TRLLabel;
    RLDraw4: TRLDraw;
    rllValorTotal: TRLLabel;
    RLLabel3: TRLLabel;
    rllAliquota: TRLLabel;
    RLDraw6: TRLDraw;
    rllCodigoObra: TRLLabel;
    rllCodObra: TRLLabel;
    rllTituloConstCivil: TRLLabel;
    rllCodigoArt: TRLLabel;
    rllCodART: TRLLabel;
    RLLabel34: TRLLabel;
    rllValorPIS: TRLLabel;
    RLLabel36: TRLLabel;
    rllValorCOFINS: TRLLabel;
    RLLabel38: TRLLabel;
    rllValorIR: TRLLabel;
    RLLabel40: TRLLabel;
    rllValorINSS: TRLLabel;
    RLLabel42: TRLLabel;
    rllValorCSLL: TRLLabel;
    RLLabel44: TRLLabel;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    RLLabel35: TRLLabel;
    RLLabel37: TRLLabel;
    RLLabel39: TRLLabel;
    RLLabel41: TRLLabel;
    RLLabel43: TRLLabel;
    RLLabel45: TRLLabel;
    RLLabel46: TRLLabel;
    RLLabel47: TRLLabel;
    RLLabel48: TRLLabel;
    RLLabel49: TRLLabel;
    RLLabel50: TRLLabel;
    RLLabel51: TRLLabel;
    RLLabel52: TRLLabel;
    RLLabel53: TRLLabel;
    RLLabel54: TRLLabel;
    rllOpcaoSN: TRLLabel;
    RLLabel56: TRLLabel;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    rllValorServicos1: TRLLabel;
    rllValorServicos2: TRLLabel;
    rllDescIncondicionado1: TRLLabel;
    rllDescIncondicionado2: TRLLabel;
    rllDescCondicionado: TRLLabel;
    rllRetencoesFederais: TRLLabel;
    rllOutrasRetencoes: TRLLabel;
    rllValorIssRetido: TRLLabel;
    rllValorLiquido: TRLLabel;
    RLDraw17: TRLDraw;
    rllIncentivador: TRLLabel;
    rllNatOperacao: TRLMemo;
    rllValorDeducoes: TRLLabel;
    rllRegimeEspecial: TRLLabel;
    rllOpcaoSimples: TRLLabel;
    rllISSReter: TRLLabel;
    rbOutrasInformacoes: TRLBand;
    rlmDadosAdicionais: TRLMemo;
    RLLabel6: TRLLabel;
    rlbCanhoto: TRLBand;
    RLLabel26: TRLLabel;
    rllPrestNomeEnt: TRLLabel;
    RLLabel28: TRLLabel;
    RLDraw1: TRLDraw;
    rllNumNF0Ent: TRLLabel;
    RLLabel57: TRLLabel;
    RLLabel33: TRLLabel;
    RLDraw5: TRLDraw;
    RLLabel58: TRLLabel;
    RLLabel59: TRLLabel;
    RLDraw7: TRLDraw;
    RLLabel60: TRLLabel;
    RLLabel61: TRLLabel;
    rllTomaInscEstadual: TRLLabel;
    rllTomadorNomeEnt: TRLLabel;
    rlmDescricao: TRLMemo;
    RLSystemInfo1: TRLSystemInfo;
    RLSystemInfo2: TRLSystemInfo;
    RLLabel62: TRLLabel;
    RLLabel63: TRLLabel;
    RLDraw11: TRLDraw;
    RLLabel64: TRLLabel;
    rllMunicipioPrestacaoServico: TRLLabel;
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
    RLLabel5: TRLLabel;
    RLLabel9: TRLLabel;
    txtBaseCalculo: TRLLabel;
    txtISS: TRLLabel;
    RLDraw12: TRLDraw;
    rbCodServico: TRLBand;
    rlmCodServico: TRLMemo;
    rllMsgTeste: TRLLabel;

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
    procedure rbCodServicoBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
    FNumItem: Integer;
  public
    { Public declarations }
    class procedure QuebradeLinha(const sQuebradeLinha: String); override;
  end;

var
  frlXDANFSeRLRetrato: TfrlXDANFSeRLRetrato;

implementation

uses
  StrUtils, DateUtils,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrNFSeX, ACBrNFSeXClass, ACBrNFSeXInterface,
  ACBrValidador, ACBrDFeReportFortes;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}


var
  FQuebradeLinha: String;

  { TfrlXDANFSeRLRetrato }

class procedure TfrlXDANFSeRLRetrato.QuebradeLinha(const sQuebradeLinha: String);
begin
  FQuebradeLinha := sQuebradeLinha;
end;

procedure TfrlXDANFSeRLRetrato.rbOutrasInformacoesBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  QrCode: TDelphiZXingQRCode;
  QrCodeBitmap: TBitmap;
  QRCodeData: string;
  rlImgQrCode: TRLImage;
  Row, Column: Integer;
begin
  inherited;

  rlmDadosAdicionais.Lines.BeginUpdate;
  rlmDadosAdicionais.Lines.Clear;

  if fpNFSe.Servico.MunicipioIncidencia <> 0 then
    rlmDadosAdicionais.Lines.Add('Cod/Municipio da incidencia do ISSQN: ' +
      IntToStr(fpNFSe.Servico.MunicipioIncidencia) + ' / ' +
      fpNFSe.Servico.xMunicipioIncidencia);

  if fpDANFSe.OutrasInformacaoesImp <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(fpDANFSe.OutrasInformacaoesImp,
                                        FQuebradeLinha, #13#10, [rfReplaceAll]))
  else
    if fpNFSe.OutrasInformacoes <> '' then
      rlmDadosAdicionais.Lines.Add(StringReplace(fpNFSe.OutrasInformacoes,
                                       FQuebradeLinha, #13#10, [rfReplaceAll]));

  if fpNFSe.InformacoesComplementares <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(fpNFSe.InformacoesComplementares,
                                       FQuebradeLinha, #13#10, [rfReplaceAll]));

  if fpNFSe.Servico.infoCompl.xInfComp <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(fpNFSe.Servico.infoCompl.xInfComp,
                                       FQuebradeLinha, #13#10, [rfReplaceAll]));

  if fpNFSe.Link <> '' then
  begin
    rlmDadosAdicionais.Width := 643;
    rbOutrasInformacoes.AutoSize := True;

    rlImgQrCode := TRLImage.Create(rbOutrasInformacoes);
    rlImgQrCode.Parent := rbOutrasInformacoes;
    rlImgQrCode.Stretch := True;
    rlImgQrCode.AutoSize := False;
    rlImgQrCode.Center := True;
    rlImgQrCode.SetBounds(648, 3, 90, 90);
    rlImgQrCode.BringToFront;

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
      begin
        for Column := 0 to QrCode.Columns - 1 do
        begin
          if (QrCode.IsBlack[Row, Column]) then
            QrCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
          else
            QrCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
        end;
      end;

      rlImgQrCode.Picture.Bitmap.Assign(QrCodeBitmap);
    finally
      QrCode.Free;
      QrCodeBitmap.Free;
    end;
  end;

  rlmDadosAdicionais.Lines.EndUpdate;

  rllDataHoraImpressao.Visible := NaoEstaVazio(fpDANFSe.Usuario);
  rllDataHoraImpressao.Caption := ACBrStr('DATA / HORA DA IMPRESS�O: ') +
                               FormatDateTimeBr(Now) + ' - ' + fpDANFSe.Usuario;

  rllSistema.Visible := NaoEstaVazio(fpDANFSe.Sistema);
  rllSistema.Caption := Format('Desenvolvido por %s', [fpDANFSe.Sistema]);

  // Exibe canhoto
  rlbCanhoto.Visible := fpDANFSe.ImprimeCanhoto;
end;

procedure TfrlXDANFSeRLRetrato.rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  TDFeReportFortes.CarregarLogo(rliLogo, fpDANFSe.Logo);

  if (fpDANFSe.TamanhoLogoHeight = 0) and (fpDANFSe.TamanhoLogoWidth = 0) then
  begin
    // Expande a logomarca
    if fpDANFSe.ExpandeLogoMarca then
    begin
      rlmPrefeitura.Visible := False;

      with rliLogo do
      begin
        Height := 130;
        Width := 580;
        Top := 9;
        Left := 9;

        TDFeReportFortes.AjustarLogo(rliLogo, fpDANFSe.ExpandeLogoMarcaConfig);
      end;
    end;
  end;

  rlmPrefeitura.Lines.Clear;
  rlmPrefeitura.Lines.Add(StringReplace(fpDANFSe.Prefeitura,
                                       FQuebradeLinha, #13#10, [rfReplaceAll]));

  With fpNFSe do
  begin
    rllNumNF0.Caption := FormatFloat('00000000000', StrToFloatDef(Numero, 0));

    if HourOf(DataEmissao) <> 0 then
      rllEmissao.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', DataEmissao)
    else
      rllEmissao.Caption := FormatDateTime('dd/mm/yyyy', DataEmissao);

    rllCodigoChave.Caption := ACBrStr('C�digo de Verifica��o:');

    if fpDANFSe.Provedor = proPadraoNacional then
      rllCodigoChave.Caption := ACBrStr('Chave de Acesso:');

    rllCodVerificacao.Caption := ACBrStr(CodigoVerificacao);

    if fpDANFSe.DataCompetenciaCompleta then
      rllCompetencia.Caption := IfThen(Competencia > 0, FormatDateTime('dd/mm/yyyy', Competencia), '')
    else
      rllCompetencia.Caption := IfThen(Competencia > 0, FormatDateTime('mm/yyyy', Competencia), '');

    rllNumeroRps.Caption := IdentificacaoRps.Numero;

    if IdentificacaoRps.Serie <> '' then
      rllNumeroRps.Caption := rllNumeroRps.Caption + '/' + IdentificacaoRps.Serie;

    rllNumNFSeSubstituida.Caption := NfseSubstituida;

    rllMunicipioPrestacaoServico.Caption := ACBrStr(Servico.MunicipioPrestacaoServico);

    RLLabel64.Visible := ACBrNFSe.Provider.ConfigGeral.ImprimirLocalPrestServ;
    rllMunicipioPrestacaoServico.Visible := ACBrNFSe.Provider.ConfigGeral.ImprimirLocalPrestServ;
  end;
end;

procedure TfrlXDANFSeRLRetrato.rlbItensServicoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  with fpNFSe.Servico.ItemServico.Items[FNumItem] do
  begin
    rlmServicoDescricao.Lines.Clear;
    rlmServicoDescricao.Lines.Add(Descricao);
    txtServicoUnitario.Caption := FormatFloatBr(ValorUnitario);
    txtServicoQtde.Caption := FormatFloatBr(Quantidade);

//    if ValorTotal = 0.0 then
//      ValorTotal := Quantidade * ValorUnitario;

    txtServicoTotal.Caption := FormatFloatBr(ValorTotal);
    txtBaseCalculo.Caption := FormatFloatBr(BaseCalculo);

    txtISS.Caption := FormatFloatBr(ValorISS);
  end;
end;

procedure TfrlXDANFSeRLRetrato.rbCodServicoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  i: Integer;
  xItemLista: string;
  LServicoPossuiItemLista: Boolean;
begin
  inherited;

  with fpNFSe do
  begin
    rlmCodServico.Lines.Clear;
    xItemLista := '';
    LServicoPossuiItemLista := False;

    if Servico.ItemServico.Count > 0 then
    begin
      for i := 0 to Servico.ItemServico.Count - 1 do
      begin
        if (Trim(Servico.ItemServico.Items[i].ItemListaServico) <> '') or
           (Trim(Servico.ItemServico.Items[i].xItemListaServico) <> '') then
        begin
          LServicoPossuiItemLista := True;
          Break;
        end;
      end;

      if LServicoPossuiItemLista then
      begin
        rlmCodServico.Lines.Append(ACBrStr('C�digo Servi�o:'));
        for i := 0 to Servico.ItemServico.Count - 1 do
        begin
          if (Trim(Servico.ItemServico.Items[i].ItemListaServico) <> '') and
             (Pos(Servico.ItemServico.Items[i].ItemListaServico, xItemLista) = 0) then
          begin
            rlmCodServico.Lines.Append('   ' + Servico.ItemServico.Items[i].ItemListaServico +
              ' - ' + ACBrStr(Servico.ItemServico.Items[i].xItemListaServico));
            xItemLista := xItemLista + Servico.ItemServico.Items[i].ItemListaServico + '/';
          end;
        end;
      end;
    end;

    if (not LServicoPossuiItemLista) and (Servico.xItemListaServico <> '') then
    begin
      rlmCodServico.Lines.Append(ACBrStr('C�digo Servi�o:'));
      rlmCodServico.Lines.Append('   ' + Servico.ItemListaServico + ' - ' +
        ACBrStr(Servico.xItemListaServico));
    end;

    if fpDANFSe.Atividade <> '' then
      rlmCodServico.Lines.Append('Atividade: ' + fpDANFSe.Atividade);

    if (Servico.xCodigoTributacaoMunicipio <> '') then
      rlmCodServico.Lines.Append('Cod. Tributacao Municipio: ' + Servico.xCodigoTributacaoMunicipio);

    if Servico.CodigoCnae <> '' then
      rlmCodServico.Lines.Append(ACBrStr('C�digo CNAE: ') + Servico.CodigoCnae);
  end;
end;

procedure TfrlXDANFSeRLRetrato.rlbISSQNBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  MostrarObra, MostrarNaturezaOperacao: Boolean;
  FProvider: IACBrNFSeXProvider;
begin
  inherited;

  FProvider := ACBrNFSe.Provider;

  With fpNFSe do
  begin
    rllRegimeEspecial.Caption := ACBrStr(FProvider.RegimeEspecialTributacaoDescricao(RegimeEspecialTributacao));
    rllNatOperacao.Lines.Text := ACBrStr(FProvider.NaturezaOperacaoDescricao(NaturezaOperacao));
    MostrarNaturezaOperacao := rllNatOperacao.Caption <> '';
    RLLabel137.Visible := MostrarNaturezaOperacao;
    rllOpcaoSimples.Caption := ACBrStr(FProvider.SimNaoDescricao(OptanteSimplesNacional));
    rllIncentivador.Caption := ACBrStr(FProvider.SimNaoDescricao(IncentivadorCultural));

    if fpDANFSe.Provedor = proPadraoNacional then
    begin
      rllOpcaoSimples.Caption := ACBrStr(OptanteSNToDesc(OptanteSN));

      rllNatOperacao.Visible := False;
      RLLabel137.Visible := False;
      rllIncentivador.Visible := False;
      RLLabel56.Visible := False;
    end;

    rllOpcaoSN.Visible := ACBrNFSe.Provider.ConfigGeral.ImprimirOptanteSN;
    rllOpcaoSimples.Visible := ACBrNFSe.Provider.ConfigGeral.ImprimirOptanteSN;

    rllCodObra.Caption := ConstrucaoCivil.CodigoObra;
    rllCodART.Caption := ConstrucaoCivil.Art;
    MostrarObra := (rllCodObra.Caption <> '') or (rllCodART.Caption <> '');
//    rlsLinhaH1.Visible := MostrarObra;
    rllTituloConstCivil.Visible := MostrarObra;
    rllCodigoObra.Visible := MostrarObra;
    rllCodObra.Visible := MostrarObra;
    rllCodigoArt.Visible := MostrarObra;
    rllCodART.Visible := MostrarObra;

    if Servico.Valores.ValorTaxaTurismo > 0 then
      rllValorTotal.Caption := 'VALOR TOTAL DA NOTA + (Taxa Turismo ' +
                               FormatFloat(',0.00', Servico.Valores.ValorTaxaTurismo) + ')' +
                               ' = R$ ' + FormatFloat(',0.00', Servico.Valores.ValorTotalNotaFiscal)
    else
      rllValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ ' +
                             FormatFloat(',0.00', Servico.Valores.ValorTotalNotaFiscal);

    rllValorPIS.Caption := FormatFloat(',0.00', Servico.Valores.ValorPis);
    rllValorCOFINS.Caption := FormatFloat(',0.00', Servico.Valores.ValorCofins);
    rllValorIR.Caption := FormatFloat(',0.00', Servico.Valores.ValorIr);
    rllValorINSS.Caption := FormatFloat(',0.00', Servico.Valores.ValorInss);
    rllValorCSLL.Caption := FormatFloat(',0.00', Servico.Valores.ValorCsll);
    rllValorServicos1.Caption := FormatFloat(',0.00', Servico.Valores.ValorServicos);
    rllDescIncondicionado1.Caption := FormatFloat(',0.00', Servico.Valores.DescontoIncondicionado);
    rllDescCondicionado.Caption := FormatFloat(',0.00', Servico.Valores.DescontoCondicionado);
    rllRetencoesFederais.Caption := FormatFloat(',0.00', Servico.Valores.RetencoesFederais);
    rllOutrasRetencoes.Caption := FormatFloat(',0.00', Servico.Valores.OutrasRetencoes);
    rllValorIssRetido.Caption := FormatFloat(',0.00', Servico.Valores.ValorIssRetido);
    rllValorLiquido.Caption := FormatFloat(',0.00', Servico.Valores.ValorLiquidoNfse);
    rllValorServicos2.Caption := FormatFloat(',0.00', Servico.Valores.ValorServicos);
    rllValorDeducoes.Caption := FormatFloat(',0.00', Servico.Valores.ValorDeducoes);
    rllDescIncondicionado2.Caption := FormatFloat(',0.00', Servico.Valores.DescontoIncondicionado);
    rllBaseCalc.Caption := FormatFloat(',0.00', Servico.Valores.BaseCalculo);

    rllAliquota.Caption := fpDANFSe.FormatarAliquota(Servico.Valores.Aliquota);
    rllISSReter.Caption := FProvider.SituacaoTributariaDescricao(Servico.Valores.IssRetido);
    rllValorISS.Caption := FormatFloat(',0.00', Servico.Valores.ValorIss);
  end;
end;

procedure TfrlXDANFSeRLRetrato.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmDescricao.Lines.Clear;
  rlmDescricao.Lines.Add(fpNFSe.Servico.Discriminacao);
end;

procedure TfrlXDANFSeRLRetrato.rlbPrestadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  xEndereco, xNumero, xComplemento, xBairro, xMunic, xUF, xCEP, xFone,
  xEmail: string;
begin
  inherited;

  TDFeReportFortes.CarregarLogo(rliPrestLogo, fpDANFSe.Prestador.Logo);

  rllPrestNome.Caption := fpDANFSe.Prestador.RazaoSocial;

  if rllPrestNome.Caption = '' then
    rllPrestNome.Caption := fpDANFSe.Prestador.NomeFantasia;

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

  rllNumNF0Ent.Caption := FormatFloat('00000000000', StrToFloatDef(fpNFSe.Numero, 0));
  rllTomadorNomeEnt.Caption := ACBrStr('Emiss�o:') +
    FormatDateTime('dd/mm/yy', fpNFSe.DataEmissao) +
    '-Tomador:' + fpNFSe.Tomador.RazaoSocial +
    '-Total:' + FormatFloat(',0.00', fpNFSe.Servico.Valores.ValorLiquidoNfse);
end;

procedure TfrlXDANFSeRLRetrato.rlbTomadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  fpDANFSe.SetDadosTomador(fpNFSe);

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
      rllTomaCNPJ.Caption := FormatarCNPJouCPF(IdentificacaoTomador.CpfCnpj);

    rllTomaInscMunicipal.Caption := fpDANFSe.Tomador.InscricaoMunicipal;

    rllTomaInscEstadual.Caption := fpDANFSe.Tomador.InscricaoEstadual;

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

    rllTomaComplemento.Caption := fpDANFSe.Tomador.Complemento;

    rllTomaMunicipio.Caption := Endereco.xMunicipio;

    rllTomaUF.Caption := Endereco.UF;

    rllTomaTelefone.Caption := FormatarFone(fpDANFSe.Tomador.Fone);
    rllTomaEmail.Caption := fpDANFSe.Tomador.Email;
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
end;

procedure TfrlXDANFSeRLRetrato.RLNFSeBeforePrint(Sender: TObject;
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
end;

procedure TfrlXDANFSeRLRetrato.subItensDataRecord(Sender: TObject;
  RecNo: Integer; CopyNo: Integer; var Eof: Boolean;
  var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  Eof := (RecNo > fpNFSe.Servico.ItemServico.Count);
  RecordAction := raUseIt;
end;

end.
