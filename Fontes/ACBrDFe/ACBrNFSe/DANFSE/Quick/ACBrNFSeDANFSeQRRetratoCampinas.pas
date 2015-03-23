{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeDANFSeQRRetratoCampinas;
// Fernando Oliveira - 05/08/2013 - ALTERAÇÃO ESPECÍFICA PARA O ASIX

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pnfsConversao, ACBrNFSeDANFSEClass, ACBrNFSeDANFSeQR, ACBrNFSeDANFSeQRClass,
  QRCtrls, QuickRpt, ExtCtrls, qrFramelines, grimgctrl, pnfsNFSe;

type
  TfqrDANFSeQRRetratoCampinas = class(TfqrDANFSeQR)
    QRNFSeCamp: TQuickRep;
    qrb_1_Cabecalho: TQRBand;
    qrsNumeroRPS: TQRShape;
    qrsEmissao: TQRShape;
    qrlEmissao: TQRLabel;
    qrlTEmissao: TQRLabel;
    qrlNumeroRps: TQRLabel;
    qrlTNumeroRPS: TQRLabel;
    qriLogo: TQRImage;
    qrmPrefeitura: TQRMemo;
    qrb_2_PrestadorServico: TQRChildBand;
    qrlTPrestMunicipio: TQRLabel;
    qrlTPrestInscMunicipal: TQRLabel;
    qrlTPrestEndereco: TQRLabel;
    qrlTPrestCNPJ: TQRLabel;
    qrlPrestMunicipio: TQRLabel;
    qrlPrestInscMunicipal: TQRLabel;
    qrlPrestEndereco: TQRLabel;
    qrlPrestCNPJ: TQRLabel;
    qrlTPrestadorDeServicos: TQRLabel;
    qrlTPrestNome: TQRLabel;
    qrlPrestNome: TQRLabel;
    qrlTPrestUF: TQRLabel;
    qrlPrestUF: TQRLabel;
    qrb_3_TomadorServico: TQRChildBand;
    qrlTTomadorDeServicos: TQRLabel;
    qrlTTomaCNPJ: TQRLabel;
    qrlTomaCNPJ: TQRLabel;
    qrlTTomaNome: TQRLabel;
    qrlTomaNome: TQRLabel;
    qrlTTomaEndereco: TQRLabel;
    qrlTomaEndereco: TQRLabel;
    qrlTTomaMunicipio: TQRLabel;
    qrlTomaMunicipio: TQRLabel;
    qrlTTomaUF: TQRLabel;
    qrlTomaUF: TQRLabel;
    qrlTTomaEmail: TQRLabel;
    qrlTomaEmail: TQRLabel;
    qrb_6_Totalizadores: TQRBand;
    qrlAliquotaPIS: TQRLabel;
    qrlValorPIS: TQRLabel;
    qrlAliquotaCOFINS: TQRLabel;
    qrlValorCOFINS: TQRLabel;
    qrlAliquotaIR: TQRLabel;
    qrlValorIR: TQRLabel;
    qrlAliquotaINSS: TQRLabel;
    qrlValorINSS: TQRLabel;
    qrlValorCSLL: TQRLabel;
    qrlAliquotaCSLL: TQRLabel;
    qrlVlrTotalRPS: TQRLabel;
    qrlTValorTotalDoRPS: TQRLabel;
    qrlTValorDeducoes: TQRLabel;
    qrlValorDeducoes: TQRLabel;
    qrlInformacoes: TQRLabel;
    qrlTInformacoesImportantes: TQRLabel;
    qrlBaseCalculo: TQRLabel;
    qrlTBaseCalculo: TQRLabel;
    qrlTAliquota: TQRLabel;
    qrlAliquota: TQRLabel;
    qrTVlrISS: TQRLabel;
    qrlVlrISS: TQRLabel;
    qrsValorPIS: TQRShape;
    qrsValorIR: TQRShape;
    qrsValorCOFINS: TQRShape;
    qrsValorINSS: TQRShape;
    qrsValorCSLL: TQRShape;
    qrsValorDeducoes: TQRShape;
    qrsBaseCalculo: TQRShape;
    qrsAliquota: TQRShape;
    qrsVlrISS: TQRShape;
    qrb_4_Discriminacao: TQRChildBand;
    qrlTDiscriminacaoDosServicos: TQRLabel;
    qrmDescricao: TQRMemo;
    qrlMsgTeste: TQRLabel;
    qrlTItem: TQRLabel;
    qrsItem: TQRShape;
    qrlTQuantidade: TQRLabel;
    qrsQuantidade: TQRShape;
    qrlTValorUnitario: TQRLabel;
    qrsValorUnitario: TQRShape;
    qrlTValorTotal: TQRLabel;
    qrsValorTotal: TQRShape;
    qrb_5_Itens: TQRBand;
    qrlItem: TQRLabel;
    qrsIItem: TQRShape;
    qrlQntde: TQRLabel;
    qrsIQntde: TQRShape;
    qrsIVlrUnit: TQRShape;
    qrlVlrUnit: TQRLabel;
    qrlVlrTotal: TQRLabel;
    qrsIVlrTotal: TQRShape;
    procedure QRNFSeCampBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrb_1_CabecalhoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_2_PrestadorServicoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_3_TomadorServicoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_4_DiscriminacaoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_6_TotalizadoresBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_5_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
  public
    FNFSe           : TNFSe;
    FLogo           : String;
    FEmail          : String;
    FFax            : String;
    FNumCopias      : Integer;
    FSistema        : String;
    FSite           : String;
    FUsuario        : String;
    FPreview        : Boolean;
    FMargemSuperior : Double;
    FMargemInferior : Double;
    FMargemEsquerda : Double;
    FMargemDireita  : Double;
    FImpressora     : String;
    FPrestLogo      : String;
    FPrefeitura     : String;
    FNFSeCancelada  : Boolean;
  end;

var
  fqrDANFSeQRRetratoCampinas: TfqrDANFSeQRRetratoCampinas;
  qntdItens, qntdItensImpressos : Integer;

implementation

uses
 StrUtils, DateUtils, ACBrUtil, ACBrDFeUtil, ACBrNFSeUtil,
  ACBrNFSeNotasFiscais;

{$R *.dfm}

procedure TfqrDANFSeQRRetratoCampinas.QRNFSeCampBeforePrint(
  Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  QRNFSeCamp.ReportTitle := 'NFS-e: ' + FNFSe.Numero;

  QRNFSeCamp.Page.TopMargin    := FMargemSuperior * 100;
  QRNFSeCamp.Page.BottomMargin := FMargemInferior * 100;
  QRNFSeCamp.Page.LeftMargin   := FMargemEsquerda * 100;
  QRNFSeCamp.Page.RightMargin  := FMargemDireita  * 100;

  qntdItens := FNFSe.Servico.ItemServico.Count;
  qntdItensImpressos := 0;
end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_1_CabecalhoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  if (FLogo <> '') and FilesExists(FLogo) then
  begin
    qriLogo.Picture.LoadFromFile(FLogo);
  end;

  qrmPrefeitura.Lines.Clear;
  qrmPrefeitura.Lines.Add(StringReplace(FPrefeitura,
                          ';', #13#10, [rfReplaceAll,rfIgnoreCase]));

  qrlNumeroRPS.Caption := FNFSe.IdentificacaoRps.Numero;
  qrlEmissao.Caption := FormatDateTime(DateTimeToStr(FNFSe.DataEmissao));
end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_2_PrestadorServicoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlPrestCNPJ.Caption := FormatarCNPJ( FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj );
  qrlPrestInscMunicipal.Caption := FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
  qrlPrestNome.Caption := FNFSe.PrestadorServico.RazaoSocial;

  qrlPrestEndereco.Caption := Trim(FNFSe.PrestadorServico.Endereco.Endereco)+', '+
                              Trim(FNFSe.PrestadorServico.Endereco.Numero)+' ' + 
                              Trim(FNFSe.PrestadorServico.Endereco.Complemento)+' - '+
                              Trim(FNFSe.PrestadorServico.Endereco.Bairro)+
                              ' - CEP: '+
                              FormatarCEP(Poem_Zeros(FNFSe.PrestadorServico.Endereco.CEP,8));
 qrlPrestMunicipio.Caption := FNFSe.PrestadorServico.Endereco.CodigoMunicipio +
  ' - ' + FNFSe.PrestadorServico.Endereco.xMunicipio;
 qrlPrestUF.Caption := FNFSe.PrestadorServico.Endereco.UF;
end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_3_TomadorServicoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  if Length(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)<=11 then
    qrlTomaCNPJ.Caption := FormatarCPF(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)
  else
    qrlTomaCNPJ.Caption := FormatarCNPJ(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj);

  qrlTomaNome.Caption := FNFSe.Tomador.RazaoSocial;
  qrlTomaEndereco.Caption := Trim(FNFSe.Tomador.Endereco.Endereco)+', '+
                             Trim(FNFSe.Tomador.Endereco.Numero)+' '+
                             Trim(FNFSe.Tomador.Endereco.Complemento)+' - ' +
                             Trim(FNFSe.Tomador.Endereco.Bairro)+
                             ' - CEP: '+
                             FormatarCEP(Poem_Zeros(FNFSe.Tomador.Endereco.CEP,8));
  qrlTomaMunicipio.Caption := FNFSe.Tomador.Endereco.CodigoMunicipio +
    ' - ' + FNFSe.Tomador.Endereco.xMunicipio;
  qrlTomaUF.Caption := FNFSe.Tomador.Endereco.UF;
  qrlTomaEmail.Caption := FNFSe.Tomador.Contato.Email;
end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_4_DiscriminacaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrmDescricao.Lines.BeginUpdate;
  qrmDescricao.Lines.Clear;

  if trim(FNFSe.Servico.Descricao) = ''
  then qrmDescricao.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                              FNFSe.Servico.CodigoTributacaoMunicipio + ' - ' +
                              FNFSe.Servico.xItemListaServico)

  else qrmDescricao.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                              FNFSe.Servico.CodigoTributacaoMunicipio + ' - ' +
                              FNFSe.Servico.Descricao);

  qrmDescricao.Lines.EndUpdate;

end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_6_TotalizadoresBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  msg : String;
begin
  inherited;

  qrlAliquotaPIS.Caption    := StringReplace(qrlAliquotaPIS.Caption,    'qrlAliquotaPIS',
        ((FormatFloat(FNFSe.Servico.Valores.AliquotaPis)) + '%'), [rfReplaceAll,rfIgnoreCase]);
  qrlAliquotaCOFINS.Caption := StringReplace(qrlAliquotaCOFINS.Caption, 'qrlAliquotaCOFINS',
        ((FormatFloat(FNFSe.Servico.Valores.ValorCofins)) + '%'), [rfReplaceAll,rfIgnoreCase]);
  qrlAliquotaIR.Caption     := StringReplace(qrlAliquotaIR.Caption,     'qrlAliquotaIR',
        ((FormatFloat(FNFSe.Servico.Valores.AliquotaIr)) + '%'), [rfReplaceAll,rfIgnoreCase]);
  qrlAliquotaINSS.Caption   := StringReplace(qrlAliquotaINSS.Caption,   'qrlAliquotaINSS',
        ((FormatFloat(FNFSe.Servico.Valores.AliquotaInss)) + '%'), [rfReplaceAll,rfIgnoreCase]);
  qrlAliquotaCSLL.Caption   := StringReplace(qrlAliquotaCSLL.Caption,   'qrlAliquotaCSLL',
        ((FormatFloat(FNFSe.Servico.Valores.AliquotaCsll)) + '%'), [rfReplaceAll,rfIgnoreCase]);

  qrlValorPIS.Caption    := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorPis);
  qrlValorCOFINS.Caption := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorCofins);
  qrlValorIR.Caption     := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorIr);
  qrlValorINSS.Caption   := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorInss);
  qrlValorCSLL.Caption   := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorCsll);

  qrlVlrTotalRPS.Caption := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorServicos);

  qrlValorDeducoes.Caption := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorDeducoes);
  qrlBaseCalculo.Caption   := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.BaseCalculo);
  qrlAliquota.Caption      := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.Aliquota);
  qrlVlrISS.Caption        := 'R$ ' + FormatFloat(FNFSe.Servico.Valores.ValorIss);

  msg := '';

  case FNFSe.NaturezaOperacao of
    noTributacaoNoMunicipio   : msg := 'NATUREZA DE OPERAÇÃO: 1 - Tributação no município';
    noTributacaoForaMunicipio : msg := 'NATUREZA DE OPERAÇÃO: 2 - Tributação fora do município';
    noIsencao                 : msg := 'NATUREZA DE OPERAÇÃO: 3 - Isenção';
    noImune                   : msg := 'NATUREZA DE OPERAÇÃO: 4 - Imune';
    noSuspensaDecisaoJudicial : msg := 'NATUREZA DE OPERAÇÃO: 5 - Exigibilidade suspensa por decisão judicial';

    noSuspensaProcedimentoAdministrativo : msg := 'NATUREZA DE OPERAÇÃO: 6 - Exigibilidade suspensa por procedimento administrativo';

    noTributacaoNoMunicipio51         : msg := 'NATUREZA DE OPERAÇÃO: 51- Imposto devido no município, com obrigação de retenção na fonte';
    noTributacaoNoMunicipioSemISS52   : msg := 'NATUREZA DE OPERAÇÃO: 52 - Imposto devido no município, sem obrigação de retenção na fonte';
    noNaoTributa58                    : msg := 'NATUREZA DE OPERAÇÃO: 58 - Não tributável';
    noSimplesNacional59               : msg := 'NATUREZA DE OPERAÇÃO: 59 - Imposto recolhido pelo regime único de arrecadação Simples Nacional';
    noTributacaoNoMunicipio61         : msg := 'NATUREZA DE OPERAÇÃO: 61 - Imposto devido no município, com obrigação de retenção na fonte';
    noTributacaoNoMunicipioSemISS62   : msg := 'NATUREZA DE OPERAÇÃO: 62 - Imposto devido no município, sem obrigação de retenção na fonte';
    noTributacaoForaMunicipio63       : msg := 'NATUREZA DE OPERAÇÃO: 63 - Imposto devido fora do município, com obrigação de retenção na fonte';
    noTributacaoForaMunicipioSemISS64 : msg := 'NATUREZA DE OPERAÇÃO: 64 - Imposto devido fora do município, sem obrigação de retenção na fonte';
    noNaoTributa68                    : msg := 'NATUREZA DE OPERAÇÃO: 68 - Não tributável';
    noSimplesNacional69               : msg := 'NATUREZA DE OPERAÇÃO: 69 - Imposto recolhido pelo regime único de arrecadação Simples Nacional';
    noNaoTributa78                    : msg := 'NATUREZA DE OPERAÇÃO: 78 - Não tributável';
  end;

  case FNFSe.RegimeEspecialTributacao of
    retNenhum                    : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 0 - Nenhum';
    retMicroempresaMunicipal     : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 1 - Microempresa municipal';
    retEstimativa                : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 2 - Estimativa';
    retSociedadeProfissionais    : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 3 - Sociendade de profissionais';
    retCooperativa               : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 4 - Cooperativa';
    retMicroempresarioIndividual : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 5 - Microempresário Individual (MEI)';
    retMicroempresarioEmpresaPP  : msg := msg + #13 + 'REGIME ESPECIAL DE TRIBUTAÇÃO: 6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
  end;

  if FNFSe.OptanteSimplesNacional = snSim then
    msg := msg + #13 + 'PRESTADOR OPTANTE PELO SIMPLES NACIONAL';

  if FNFSe.IncentivadorCultural = snSim then
    msg := msg + #13 + 'PRESTADOR É INCENTIVADOR CULTURAL';

  qrlInformacoes.Caption := msg;
end;

procedure TfqrDANFSeQRRetratoCampinas.qrb_5_ItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlItem.Caption := FNFSe.Servico.ItemServico[qntdItensImpressos].Descricao;

  qrlQntde.Caption    := FormatFloat(FNFSe.Servico.ItemServico[qntdItensImpressos].Quantidade);
  qrlVlrUnit.Caption  := 'R$ ' + FormatFloat(FNFSe.Servico.ItemServico[qntdItensImpressos].ValorUnitario);
  qrlVlrTotal.Caption := 'R$ ' + FormatFloat(FNFSe.Servico.ItemServico[qntdItensImpressos].ValorServicos);

  qntdItensImpressos := qntdItensImpressos + 1;
end;

end.
