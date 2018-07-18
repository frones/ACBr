{******************************************************************************}
{ Projeto: TFrmACBrConvenio115_PRN                                             }
{                                                                              }
{ Função: Imprimir Nota Fiscal Modelo 21/22 a partir dos dados inseridos no    }
{         componente TACBrConvenio115                                          }
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
{******************************************************************************}

{******************************************************************************}
{ Direitos Autorais Reservados © 2013 - Jéter Rabelo Ferreira                  }
{ Contato: jeter.rabelo@jerasoft.com.br                                        }
{******************************************************************************}
unit uFrmACBrConvenio115_PRN;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, RLReport, RLRichFilter, DB, BufDataset, RLFilters,
  RLPDFFilter, RLXLSFilter, ACBrConvenio115;

type

  TEmitente = record
    RazaoSocial: string;
    CNPJ: string;
    InscEstadual: string;
    Endereco: string;
    Cidade: string;
    UF: string;
    Telefone: string;
    HomePage: string;
    EMail: string
  end;

  { TFrmACBrConvenio115_PRN }

  TFrmACBrConvenio115_PRN = class(TForm)
    RlReport: TRLReport;
    BTitulo: TRLBand;
    RlImagem: TRLImage;
    RlTitulo: TRLLabel;
    RLDraw1: TRLDraw;
    RLLabel3: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel19: TRLLabel;
    RLLabel21: TRLLabel;
    RLLabel22: TRLLabel;
    RLLabel42: TRLLabel;
    RLLabel43: TRLLabel;
    RLLabel44: TRLLabel;
    RLLabel45: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel26: TRLLabel;
    RLLabel47: TRLLabel;
    RLMemo1: TRLMemo;
    rlmEndereco: TRLMemo;
    rllFone: TRLLabel;
    RLLabel48: TRLLabel;
    RLBand1: TRLBand;
    rllSistema: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    RLSystemInfo2: TRLSystemInfo;
    rllUsuario: TRLLabel;
    rlbDestinatario: TRLBand;
    RLDraw2: TRLDraw;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    rllDestNome: TRLLabel;
    RLLabel9: TRLLabel;
    rllDestEndereco: TRLLabel;
    RLLabel10: TRLLabel;
    rllDestCidade: TRLLabel;
    RLLabel11: TRLLabel;
    rllDestFone: TRLLabel;
    RLLabel12: TRLLabel;
    rllDestBairro: TRLLabel;
    RLLabel13: TRLLabel;
    rllDestUF: TRLLabel;
    RLLabel14: TRLLabel;
    rllDestCNPJ: TRLLabel;
    RLLabel15: TRLLabel;
    rllDestCEP: TRLLabel;
    RLLabel16: TRLLabel;
    rllDestIE: TRLLabel;
    RLDraw3: TRLDraw;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw7: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    rlbReciboHeader: TRLBand;
    rliCanhoto: TRLDraw;
    rliCanhoto1: TRLDraw;
    rliCanhoto2: TRLDraw;
    rllRecebemosDe: TRLLabel;
    rllDataRecebimento: TRLLabel;
    rllIdentificacao: TRLLabel;
    rliCanhoto3: TRLDraw;
    rllNumNF0: TRLLabel;
    rllResumo: TRLLabel;
    RLLabel20: TRLLabel;
    rllNFe: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel29: TRLLabel;
    rlbDivisaoRecibo: TRLBand;
    rliDivisao: TRLDraw;
    RLBand2: TRLBand;
    RLDraw20: TRLDraw;
    RLLabel30: TRLLabel;
    RLLabel31: TRLLabel;
    RLDraw21: TRLDraw;
    RLLabel35: TRLLabel;
    RLLabel36: TRLLabel;
    RLDraw22: TRLDraw;
    RLLabel37: TRLLabel;
    RLLabel38: TRLLabel;
    RLDraw23: TRLDraw;
    RLLabel39: TRLLabel;
    RLLabel41: TRLLabel;
    RLDraw24: TRLDraw;
    RLLabel17: TRLLabel;
    RLLabel18: TRLLabel;
    RLDraw11: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw26: TRLDraw;
    RLDraw27: TRLDraw;
    rlbObsItem: TRLBand;
    LinhaInicioItem: TRLDraw;
    rlmObsItem: TRLMemo;
    LinhaObsItemEsquerda: TRLDraw;
    LinhaObsItemDireita: TRLDraw;
    RLBand3: TRLBand;
    RLLabel25: TRLLabel;
    RLDraw14: TRLDraw;
    rlbCabecalhoItens: TRLBand;
    rlsRectProdutos: TRLDraw;
    RLLabel24: TRLLabel;
    RLDraw12: TRLDraw;
    lblDadosDoProduto: TRLLabel;
    rlsDivProd1: TRLDraw;
    rlsDivProd6: TRLDraw;
    rlsDivProd8: TRLDraw;
    RLLabel23: TRLLabel;
    RLLabel32: TRLLabel;
    rlmCodProd: TRLMemo;
    RLMemo4: TRLMemo;
    RLDraw17: TRLDraw;
    RLLabel33: TRLLabel;
    RLLabel34: TRLLabel;
    RLDraw18: TRLDraw;
    rlbItens: TRLBand;
    txtCodigo: TRLDBText;
    txtQuantidade: TRLDBText;
    txtValorTotal: TRLDBText;
    LinhaQuantidade: TRLDraw;
    LinhaValorTotal: TRLDraw;
    LinhaFinal: TRLDraw;
    LinhaDescricao: TRLDraw;
    LinhaCodigo: TRLDraw;
    RLLabel40: TRLLabel;
    LinhaFimItens: TRLDraw;
    RLDraw13: TRLDraw;
    RLDBText1: TRLDBText;
    RLDBText2: TRLDBText;
    RLDraw16: TRLDraw;
    RLDraw19: TRLDraw;
    RLDBText3: TRLDBText;
    RLBand4: TRLBand;
    RLDraw28: TRLDraw;
    RLMemo2: TRLMemo;
    RLDraw29: TRLDraw;
    RLDraw30: TRLDraw;
    RLBand5: TRLBand;
    RLLabel46: TRLLabel;
    RLDraw31: TRLDraw;
    RLPDFFilter1: TRLPDFFilter;
    RLRichFilter1: TRLRichFilter;
    RLXLSFilter1: TRLXLSFilter;
    dsRPT: TDataSource;
    RLLabel2: TRLLabel;
    procedure rllNumNF0BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel20BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rlmEnderecoBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllFoneBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rlmSiteEmailBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel6BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestNomeBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestCNPJBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestEnderecoBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestBairroBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestCEPBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestCidadeBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestFoneBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestUFBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllDestIEBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel18BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure txtQuantidadeBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rlmObsItemBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure txtValorTotalBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RlImagemBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLLabel21BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel22BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel29BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel27BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel43BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel45BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel31BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel36BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel41BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel38BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLMemo2BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel48BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure RLLabel19BeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rllSistemaBeforePrint(Sender: TObject; var Text: string; var PrintIt: Boolean);
    procedure rllUsuarioBeforePrint(Sender: TObject; var Text: string; var PrintIt: Boolean);
    procedure RLLabel2BeforePrint(Sender: TObject; var Text: string; var PrintIt: Boolean);
    procedure rllResumoBeforePrint(Sender: TObject; var Text: string; var PrintIt: Boolean);
  private
    FCds: TBufDataset;
    FLogoTipo: string;
    FSistema: String;
    FUsuario: String;
    FInformacoesAdicionais: string;
    procedure DoAddDataSet;
  public
    FConvenio115: TACBrConvenio115;
    Emitente: TEmitente;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Convenio115: TACBrConvenio115 read FConvenio115;
    procedure Imprimir(AExibir: Boolean);
    procedure SalvarPDF;
  published
    property LogoTipoBMP: string read FLogoTipo write FLogoTipo;
    property Sistema: string read FSistema write FSistema;
    property Usuario: string read FUsuario write FUsuario;
    property InformacoesAdicionais: string read FInformacoesAdicionais write FInformacoesAdicionais;
  end;

implementation

uses
  ACBrUtil, StrUtils;

{$R *.lfm}


constructor TFrmACBrConvenio115_PRN.Create(AOwner: TComponent);
  procedure CriarFields;
  begin
    with FCds do
    begin
      FCds.Fields.Clear;
      FCds.FieldDefs.Clear;
      FCds.FieldDefs.Add('N_ITEM', ftString, 3);
      FCds.FieldDefs.Add('DESCRICAO', ftString, 40);
      FCds.FieldDefs.Add('ICMS_BC', ftString, 18);
      FCds.FieldDefs.Add('ICMS_VALOR', ftString, 18);
      FCds.FieldDefs.Add('ICMS_ALIQUOTA', ftString, 6);
      FCds.FieldDefs.Add('V_TOTAL', ftString, 6);
      FCds.CreateDataSet;
    end;
  end;
begin
  inherited;
  FSistema := 'Copyright by Jera Soft Co. - 2009/2010 - http://www.jerasoft.com.br';
  FUsuario := '';
  FConvenio115 := TACBrConvenio115.Create(nil);
  FCds := TBufDataset.Create(nil);
  CriarFields;
  dsRPT.DataSet := FCds;
end;

destructor TFrmACBrConvenio115_PRN.Destroy;
begin
  FCds.Close;
  FreeAndNil(FCds);
  FreeAndNil(FConvenio115);
  inherited;
end;

procedure TFrmACBrConvenio115_PRN.DoAddDataSet;
var
  I: Integer;
begin
  inherited;
  if Convenio115.Mestre.Count <> 1 then
    raise Exception.Create('Informe uma Nota Fiscal por vez!');

  for I := 0 to Convenio115.Mestre[0].Detalhes.Count-1 do
  begin
    FCds.Append;
    FCds.FieldByName('N_ITEM').AsString := IntToStr(Convenio115.Mestre[0].Detalhes[i].Item);
    FCds.FieldByName('DESCRICAO').AsString := Convenio115.Mestre[0].Detalhes[i].DescricaoServico;
    FCds.FieldByName('ICMS_BC').AsString := FormatFloat(',0.00', Convenio115.Mestre[0].Detalhes[i].ICMSBaseCalculo);
    FCds.FieldByName('ICMS_VALOR').AsString := FormatFloat(',0.00', Convenio115.Mestre[0].Detalhes[i].ICMSValor);
    FCds.FieldByName('ICMS_ALIQUOTA').AsString := FormatFloat(',0.00', Convenio115.Mestre[0].Detalhes[i].ICMSAliquota);
    FCds.FieldByName('V_TOTAL').AsString := FormatFloat(',0.00', Convenio115.Mestre[0].Detalhes[i].ValorTotal);
    FCds.Post;
  end;
end;

procedure TFrmACBrConvenio115_PRN.Imprimir(AExibir: Boolean);
begin
  DoAddDataSet;
  if AExibir then
    RlReport.PreviewModal
  else
    RlReport.Print;
end;

procedure TFrmACBrConvenio115_PRN.RlImagemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  if LogoTipoBMP = '' then
    Exit;

  if not FileExists(LogoTipoBMP) then
    Exit;

  RlImagem.Picture.Bitmap.LoadFromFile(LogoTipoBMP);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel18BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [Convenio115.Mestre[0].ValorTotal]);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel19BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := IntToStr(Convenio115.Modelo);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel20BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.RazaoSocial;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel21BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Serie;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel22BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Convenio115.Mestre[0].Detalhes.Count - 1 do
  begin
    if Convenio115.Mestre[0].Detalhes[I].CFOP <> '' then
      Text := Convenio115.Mestre[0].Detalhes[I].CFOP;
  end;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel27BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := 'Mod: ' + IntToStr(Convenio115.Modelo);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel29BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := 'Série: ' + Convenio115.Serie + ' ';;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel2BeforePrint(Sender: TObject; var Text: string;
  var PrintIt: Boolean);
begin
  Text := 'Impressão: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss',now);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel31BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [Convenio115.Mestre[0].ICMS_BaseCalculo]);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel36BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [Convenio115.Mestre[0].ICMS_Valor]);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel38BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [Convenio115.Mestre[0].IsentosNaoTributadas]);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel41BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [Convenio115.Mestre[0].OutrosValores]);
end;

procedure TFrmACBrConvenio115_PRN.RLLabel43BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.CNPJ;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel45BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.InscEstadual;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel48BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.HomePage + ' - Email: ' + Emitente.EMail;
end;

procedure TFrmACBrConvenio115_PRN.RLLabel6BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := FormatDateTime('dd/mm/yyyy', Convenio115.Mestre[0].DataEmissao);
end;

procedure TFrmACBrConvenio115_PRN.rllDestBairroBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.Bairro;
end;

procedure TFrmACBrConvenio115_PRN.rllDestCEPBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.Cep;
end;

procedure TFrmACBrConvenio115_PRN.rllDestCidadeBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.Municipio;
end;

procedure TFrmACBrConvenio115_PRN.rllDestCNPJBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.CnpjCpf;
end;

procedure TFrmACBrConvenio115_PRN.rllDestEnderecoBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.Logradouro + ', ' +
    Convenio115.Mestre[0].Destinatario.Numero +
    ifThen(Convenio115.Mestre[0].Destinatario.Complemento <> '', ' - ' +
    Convenio115.Mestre[0].Destinatario.Complemento, '');
end;

procedure TFrmACBrConvenio115_PRN.rllDestFoneBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.Telefone;
end;

procedure TFrmACBrConvenio115_PRN.rllDestIEBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.InscricaoEstadual;
end;

procedure TFrmACBrConvenio115_PRN.rllDestNomeBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.RazaoSocial;
end;

procedure TFrmACBrConvenio115_PRN.rllDestUFBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Convenio115.Mestre[0].Destinatario.UF;
end;

procedure TFrmACBrConvenio115_PRN.rllFoneBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := 'Central de Atendimento:' + Emitente.Telefone;
end;

procedure TFrmACBrConvenio115_PRN.rllNumNF0BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := 'Nº ' + FormatFloat('000,000,000', Convenio115.Mestre[0].NumeroNF);
end;

procedure TFrmACBrConvenio115_PRN.rllResumoBeforePrint(Sender: TObject; var Text: string;
  var PrintIt: Boolean);
begin
  Text := 'Referente a NOTA FISCAL DE SERVIÇO DE COMUNICAÇÃO - Valor Total R$ ' + Format('%m', [Convenio115.Mestre[0].ValorTotal]) + ' - Emissão:' + FormatDateTime('dd/mm/yyyy', Convenio115.Mestre[0].DataEmissao);
end;

procedure TFrmACBrConvenio115_PRN.rllSistemaBeforePrint(Sender: TObject; var Text: string;
  var PrintIt: Boolean);
begin
  Text := 'Powered by ' + FSistema;
end;

procedure TFrmACBrConvenio115_PRN.rllUsuarioBeforePrint(Sender: TObject; var Text: string;
  var PrintIt: Boolean);
begin
   Text := '';
   if FUsuario <> '' then
     Text := 'Usuário: ' + FUsuario;
end;

procedure TFrmACBrConvenio115_PRN.RLMemo2BeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  Text := Convenio115.Mestre[0].AutenticacaoDocumentoFiscal;
end;

procedure TFrmACBrConvenio115_PRN.rlmEnderecoBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.Endereco + #13#10 + Emitente.Cidade + '/' + Emitente.UF;
end;

procedure TFrmACBrConvenio115_PRN.rlmObsItemBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
{var
  AStr: TStringList;
}
begin
  inherited;
  Text := InformacoesAdicionais;
end;

procedure TFrmACBrConvenio115_PRN.rlmSiteEmailBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Emitente.HomePage + #13#10 + Emitente.EMail;
end;

procedure TFrmACBrConvenio115_PRN.SalvarPDF;
begin
  DoAddDataSet;
  { TODO : implementar }
end;

procedure TFrmACBrConvenio115_PRN.txtQuantidadeBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := '1,000';
end;

procedure TFrmACBrConvenio115_PRN.txtValorTotalBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  inherited;
  Text := Format('%m', [dsRPT.DataSet.FieldByName('V_TOTAL').AsFloat]);
end;

end.
