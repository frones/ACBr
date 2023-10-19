{ ****************************************************************************** }
{ Projeto: Componentes ACBr                                                      }
{ Biblioteca multiplataforma de componentes Delphi para interação com equipa-    }
{ mentos de Automação Comercial utilizados no Brasil                             }
{                                                                                }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                 }
{                                                                                }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                           }
{                                                                                }
{ Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr       }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr        }
{                                                                                }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la    }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela    }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)   }
{ qualquer versão posterior.                                                     }
{                                                                                }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM      }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU        }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor  }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)                }
{                                                                                }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto   }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,    }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.            }
{ Você também pode obter uma copia da licença em:                                }
{ http://www.opensource.org/licenses/lgpl-license.php                            }
{                                                                                }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br  }
{ Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170                 }
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
    RLLabel16: TRLLabel;
    rlmCodServico: TRLMemo;
    RLLabel3: TRLLabel;
    rllAliquota: TRLLabel;
    RLDraw6: TRLDraw;
    rlsLinhaH1: TRLDraw;
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
    RLLabel55: TRLLabel;
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
    rllMsgTeste: TRLLabel;
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
    rllCodTributacaoMunicipio: TRLLabel;
    rlmDescCodTributacaoMunicipio: TRLMemo;
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
  ACBrUtil.Base, ACBrUtil.Strings,
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
    rlmDadosAdicionais.Lines.Add(StringReplace(fpDANFSe.OutrasInformacaoesImp, FQuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase]))
  else
    if fpNFSe.OutrasInformacoes <> '' then
    rlmDadosAdicionais.Lines.Add(fpNFSe.OutrasInformacoes);

  if fpNFSe.InformacoesComplementares <> '' then
    rlmDadosAdicionais.Lines.Add(fpNFSe.InformacoesComplementares);

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

procedure TfrlXDANFSeRLRetrato.rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  TDFeReportFortes.CarregarLogo(rliLogo, fpDANFSe.Logo);

  rlmPrefeitura.Lines.Clear;
  rlmPrefeitura.Lines.Add(StringReplace(fpDANFSe.Prefeitura, FQuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase]));

  With fpNFSe do
  begin
    rllNumNF0.Caption := FormatFloat('00000000000', StrToFloatDef(Numero, 0));
    rllEmissao.Caption := FormatDateTime('dd/mm/yyyy hh:nn', DataEmissao);
    rllCodigoChave.Caption := 'Código de Verificação:';

    if fpDANFSe.Provedor = proPadraoNacional then
      rllCodigoChave.Caption := 'Chave de Acesso:';

    rllCodVerificacao.Caption := CodigoVerificacao;

    rllCompetencia.Caption := IfThen(Competencia > 0, FormatDateTime('mm/yyyy', Competencia), '');

    rllNumeroRps.Caption := IdentificacaoRps.Numero;

    if IdentificacaoRps.Serie <> '' then
      rllNumeroRps.Caption := rllNumeroRps.Caption + '/' + IdentificacaoRps.Serie;

    rllNumNFSeSubstituida.Caption := NfseSubstituida;

    rllMunicipioPrestacaoServico.Caption := Servico.MunicipioPrestacaoServico;

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

    if ValorTotal = 0.0 then
      ValorTotal := Quantidade * ValorUnitario;

    txtServicoTotal.Caption := FormatFloatBr(ValorTotal);
    txtBaseCalculo.Caption := FormatFloatBr(BaseCalculo);

    if ValorISS = 0.0 then
      ValorISS := BaseCalculo * Aliquota/100;

    txtISS.Caption := FormatFloatBr(ValorISS);
  end;
end;

procedure TfrlXDANFSeRLRetrato.rlbISSQNBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  MostrarObra, MostrarNaturezaOperacao: Boolean;
  FProvider: IACBrNFSeXProvider;
  i: Integer;
begin
  inherited;

  RLLabel16.Visible := False;
  rllCodTributacaoMunicipio.Visible := False;
  rlmDescCodTributacaoMunicipio.Visible := False;

  FProvider := ACBrNFSe.Provider;

  With fpNFSe do
  begin
    rllNatOperacao.Lines.Text := ACBrStr(FProvider.NaturezaOperacaoDescricao(NaturezaOperacao));
    MostrarNaturezaOperacao := rllNatOperacao.Caption <> '';
    RLLabel137.Visible := MostrarNaturezaOperacao;
    rllRegimeEspecial.Caption := ACBrStr(FProvider.RegimeEspecialTributacaoDescricao(RegimeEspecialTributacao));
    rllOpcaoSimples.Caption := ACBrStr(FProvider.SimNaoDescricao(OptanteSimplesNacional));
    rllIncentivador.Caption := ACBrStr(FProvider.SimNaoDescricao(IncentivadorCultural));
    rllCodObra.Caption := ConstrucaoCivil.CodigoObra;
    rllCodART.Caption := ConstrucaoCivil.Art;

    MostrarObra := (rllCodObra.Caption <> '') or (rllCodART.Caption <> '');
    rlsLinhaH1.Visible := MostrarObra;
    rllTituloConstCivil.Visible := MostrarObra;
    rllCodigoObra.Visible := MostrarObra;
    rllCodObra.Visible := MostrarObra;
    rllCodigoArt.Visible := MostrarObra;
    rllCodART.Visible := MostrarObra;

    with Servico.Valores do
    begin
      rllValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ ' +
                               FormatFloat(',0.00', ValorTotalNotaFiscal);
      rlmCodServico.Lines.Clear;

      if (Servico.xItemListaServico = '') and (Servico.ItemServico.Count > 0) then
      begin
        RLLabel16.Visible := True;

        for i := 0 to Servico.ItemServico.Count -1 do
        begin
          rlmCodServico.Lines.Append(Servico.ItemServico.Items[i].ItemListaServico +
            ' - ' + Servico.ItemServico.Items[i].xItemListaServico);
        end;
      end;

      if Servico.xItemListaServico <> '' then
      begin
        RLLabel16.Visible := True;

        if fpDANFSe.Atividade <> '' then
          rlmCodServico.Lines.Append('Atividade: ' + fpDANFSe.Atividade);

        rlmCodServico.Lines.Append(Servico.ItemListaServico + ' - ' + Servico.xItemListaServico);

        if (Servico.xCodigoTributacaoMunicipio <> '') then
        begin
          rllCodTributacaoMunicipio.Visible := True;
          rlmDescCodTributacaoMunicipio.Visible := True;
          rlmDescCodTributacaoMunicipio.Lines.Append(Servico.xCodigoTributacaoMunicipio);
        end
        else
          rlmCodServico.Height := Trunc(rlmCodServico.Height * 2.5);
      end
      else
      begin
        if fpDANFSe.Atividade <> '' then
        begin
          RLLabel16.Visible := True;
          RLLabel16.Caption := 'Atividade:';
          rlmCodServico.Lines.Append(fpDANFSe.Atividade);
        end
      end;

      rllValorPIS.Caption := FormatFloat(',0.00', ValorPis);
      rllValorCOFINS.Caption := FormatFloat(',0.00', ValorCofins);
      rllValorIR.Caption := FormatFloat(',0.00', ValorIr);
      rllValorINSS.Caption := FormatFloat(',0.00', ValorInss);
      rllValorCSLL.Caption := FormatFloat(',0.00', ValorCsll);
      rllValorServicos1.Caption := FormatFloat(',0.00', ValorServicos);
      rllDescIncondicionado1.Caption := FormatFloat(',0.00', DescontoIncondicionado);
      rllDescCondicionado.Caption := FormatFloat(',0.00', DescontoCondicionado);
      rllRetencoesFederais.Caption := FormatFloat(',0.00', RetencoesFederais);
      rllOutrasRetencoes.Caption := FormatFloat(',0.00', OutrasRetencoes);
      rllValorIssRetido.Caption := FormatFloat(',0.00', ValorIssRetido);
      rllValorLiquido.Caption := FormatFloat(',0.00', ValorLiquidoNfse);
      rllValorServicos2.Caption := FormatFloat(',0.00', ValorServicos);
      rllValorDeducoes.Caption := FormatFloat(',0.00', ValorDeducoes);
      rllDescIncondicionado2.Caption := FormatFloat(',0.00', DescontoIncondicionado);
      rllBaseCalc.Caption := FormatFloat(',0.00', BaseCalculo);

      rllAliquota.Caption := fpDANFSe.FormatarAliquota(Aliquota);

//      rllAliquota.Caption := FormatFloat(',0.00', Aliquota);
      rllISSReter.Caption := FProvider.SituacaoTributariaDescricao(IssRetido);
      rllValorISS.Caption := FormatFloat(',0.00', ValorIss);
    end;
  end;
end;

procedure TfrlXDANFSeRLRetrato.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmDescricao.Lines.Clear;
  rlmDescricao.Lines.Add(fpNFSe.Servico.Discriminacao);
end;

procedure TfrlXDANFSeRLRetrato.rlbPrestadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  TDFeReportFortes.CarregarLogo(rliPrestLogo, fpDANFSe.Prestador.Logo);

  with fpNFSe.Prestador do
  begin
    rllPrestNome.Caption := IfThen(RazaoSocial <> '', RazaoSocial, fpDANFSe.Prestador.RazaoSocial);

    if rllPrestNome.Caption = '' then
      rllPrestNome.Caption := IfThen(NomeFantasia <> '', NomeFantasia, fpDANFSe.Prestador.NomeFantasia);

    with IdentificacaoPrestador do
    begin
      rllPrestCNPJ.Caption := FormatarCNPJouCPF(IfThen(CpfCnpj <> '', CpfCnpj, fpDANFSe.Prestador.CNPJ));
      rllPrestInscMunicipal.Caption := IfThen(InscricaoMunicipal <> '', InscricaoMunicipal, fpDANFSe.Prestador.InscricaoMunicipal);
      rllPrestInscEstadual.Caption := IfThen(InscricaoEstadual <> '', InscricaoEstadual, fpDANFSe.Prestador.InscricaoEstadual);
    end;

    with Endereco do
    begin
      rllPrestEndereco.Lines.Text := IfThen(Endereco <> '', Trim(Endereco) + ', ' +
        Trim(Trim(Numero) + ' ' + Trim(IfThen(Complemento <> '', Complemento, fpDANFSe.Prestador.Complemento))) + #13 +
        Trim(Bairro) + ' - ' + Trim(IfThen(xMunicipio <> '', xMunicipio, fpDANFSe.Prestador.Municipio)) + ' - ' +
        IfThen(UF <> '', UF, fpDANFSe.Prestador.UF) + ' CEP: ' + FormatarCEP(CEP) + #13 +
        Trim(IfThen(Contato.Telefone <> '', FormatarFone(Contato.Telefone), FormatarFone(fpDANFSe.Prestador.Fone)) + '  ' +
        IfThen(Contato.Email <> '', Contato.Email, fpDANFSe.Prestador.Email)),

        Trim(fpDANFSe.Prestador.Endereco));

    end;

    rllPrestNomeEnt.Caption := IfThen(RazaoSocial <> '', RazaoSocial, fpDANFSe.Prestador.RazaoSocial);
  end;

  rllNumNF0Ent.Caption := FormatFloat('00000000000', StrToFloatDef(fpNFSe.Numero, 0));
  rllTomadorNomeEnt.Caption := ACBrStr('Emissão:') +
    FormatDateTime('dd/mm/yy', fpNFSe.DataEmissao) +
    '-Tomador:' + fpNFSe.Tomador.RazaoSocial +
    '-Total:' +
    FormatFloat(',0.00', fpNFSe.Servico.Valores.ValorLiquidoNfse);
end;

procedure TfrlXDANFSeRLRetrato.rlbTomadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  with fpNFSe.Tomador do
  begin
    rllTomaNome.Caption := RazaoSocial;

    with IdentificacaoTomador do
    begin
      lbIdentificacao.Caption := 'CPF/CNPJ:';
      if (Length(Nif) > 0) then
      begin
        lbIdentificacao.Caption := 'NIF:';
        rllTomaCNPJ.Caption := Nif;
      end
      else
        rllTomaCNPJ.Caption := FormatarCNPJouCPF(CpfCnpj);

      rllTomaInscMunicipal.Caption := IfThen(InscricaoMunicipal <> '', InscricaoMunicipal, fpDANFSe.Tomador.InscricaoMunicipal);

      rllTomaInscEstadual.Caption := IfThen(InscricaoEstadual <> '', InscricaoEstadual, fpDANFSe.Tomador.InscricaoEstadual);
    end;

    with Endereco do
    begin
      if Endereco <> '' then
      begin
        if UF = 'EX' then
        begin
          rllTomaEndereco.Caption := Trim(Endereco) + ', Pais: ' + Trim(xPais);
        end
        else
          rllTomaEndereco.Caption := Trim(Endereco) + ', ' +
            Trim(Numero) + ' - ' +
            Trim(Bairro) + ' - CEP: ' +
            FormatarCEP(CEP);
      end
      else
        rllTomaEndereco.Caption := Trim(fpDANFSe.Tomador.Endereco) + ' - CEP: ' +
          FormatarCEP(CEP);

      rllTomaComplemento.Caption := IfThen(Complemento <> '', Complemento, fpDANFSe.Tomador.Complemento);

      rllTomaMunicipio.Caption := xMunicipio;

      rllTomaUF.Caption := UF;
    end;

    with Contato do
    begin
      rllTomaTelefone.Caption := IfThen(Telefone <> '', FormatarFone(Telefone), FormatarFone(fpDANFSe.Tomador.Fone));
      rllTomaEmail.Caption := IfThen(Email <> '', Email, fpDANFSe.Tomador.Email);
    end;
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
