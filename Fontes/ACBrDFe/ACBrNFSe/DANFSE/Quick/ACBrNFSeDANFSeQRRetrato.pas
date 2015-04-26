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

unit ACBrNFSeDANFSeQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DANFSE favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 17/09/2013 por Italo Jurisato Junior
// 13/09/2013 por Italo Jurisato Junior
// 09/09/2013 por Italo Jurisato Junior
// 05/09/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,  XMLIntf, XMLDoc, JPEG, DB, DBClient,
  pnfsConversao, ACBrNFSeDANFSEClass, ACBrNFSeDANFSeQR, ACBrNFSeDANFSeQRClass;

type

  TfqrDANFSeQRRetrato = class(TfqrDANFSeQR)
    cdsItens: TClientDataSet;
    cdsItensCODIGO: TStringField;
    cdsItensDESCRICAO: TStringField;
    qrb_1_Cabecalho: TQRBand;
    qrb_2_PrestadorServico: TQRChildBand;
    qrb_3_TomadorServico: TQRChildBand;
    qrb_4_HeaderItens: TQRBand;
    qrb_6_ISSQN: TQRBand;
    qrb_7_OutrasInformacoes: TQRChildBand;
    QRShape1: TQRShape;
    QRShape3: TQRShape;
    QRShape2: TQRShape;
    qrlNumNF0: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel12: TQRLabel;
    QRShape7: TQRShape;
    QRLabel29: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    QRLabel32: TQRLabel;
    qrlPrestMunicipio: TQRLabel;
    qrlPrestInscMunicipal: TQRLabel;
    qrlPrestEndereco: TQRLabel;
    qrlPrestCNPJ: TQRLabel;
    QRShape12: TQRShape;
    QRShape52: TQRShape;
    QRShape53: TQRShape;
    QRShape54: TQRShape;
    QRShape55: TQRShape;
    QRLabel137: TQRLabel;
    QRLabel138: TQRLabel;
    QRLabel139: TQRLabel;
    qrlBaseCalc: TQRLabel;
    qrlValorISS: TQRLabel;
    QRShape56: TQRShape;
    qrmDadosAdicionais: TQRMemo;
    qrlMsgTeste: TQRLabel;
    qrb_5_Itens: TQRBand;
    qrmProdutoDescricao: TQRDBText;
    qrlDataHoraImpressao: TQRLabel;
    qrlSistema: TQRLabel;
    qriLogo: TQRImage;
    qrlEmissao: TQRLabel;
    QRLabel8: TQRLabel;
    qrlCodVerificacao: TQRLabel;
    QRShape70: TQRShape;
    qriPrestLogo: TQRImage;
    QRLabel2: TQRLabel;
    QRLabel1: TQRLabel;
    qrlPrestNome: TQRLabel;
    QRLabel9: TQRLabel;
    qrlPrestUF: TQRLabel;
    QRLabel4: TQRLabel;
    QRLabel5: TQRLabel;
    qrlTomaCNPJ: TQRLabel;
    QRLabel11: TQRLabel;
    qrlTomaInscMunicipal: TQRLabel;
    QRLabel15: TQRLabel;
    qrlTomaNome: TQRLabel;
    QRLabel17: TQRLabel;
    qrlTomaEndereco: TQRLabel;
    QRLabel19: TQRLabel;
    qrlTomaMunicipio: TQRLabel;
    QRLabel21: TQRLabel;
    qrlTomaUF: TQRLabel;
    QRLabel10: TQRLabel;
    qrlTomaEmail: TQRLabel;
    QRLabel14: TQRLabel;
    QRShape4: TQRShape;
    qrlValorTotal: TQRLabel;
    QRShape5: TQRShape;
    QRLabel16: TQRLabel;
    qrmCodServico: TQRMemo;
    QRLabel3: TQRLabel;
    qrlAliquota: TQRLabel;
    QRShape6: TQRShape;
    QRLabel6: TQRLabel;
    QRShape8: TQRShape;
    QRLabel7: TQRLabel;
    qrlCompetencia: TQRLabel;
    QRShape9: TQRShape;
    QRLabel18: TQRLabel;
    qrlNumeroRps: TQRLabel;
    QRShape10: TQRShape;
    QRLabel20: TQRLabel;
    qrlNumNFSeSubstituida: TQRLabel;
    QRLabel22: TQRLabel;
    qrlPrestComplemento: TQRLabel;
    QRLabel23: TQRLabel;
    qrlPrestTelefone: TQRLabel;
    QRLabel24: TQRLabel;
    qrlPrestEmail: TQRLabel;
    QRLabel25: TQRLabel;
    qrlTomaComplemento: TQRLabel;
    QRLabel27: TQRLabel;
    qrlTomaTelefone: TQRLabel;
    qrsLinhaH1: TQRShape;
    qrlCodigoObra: TQRLabel;
    qrlCodObra: TQRLabel;
    qrlTituloConstCivil: TQRLabel;
    qrlCodigoArt: TQRLabel;
    qrlCodART: TQRLabel;
    QRLabel34: TQRLabel;
    qrlValorPIS: TQRLabel;
    QRLabel36: TQRLabel;
    qrlValorCOFINS: TQRLabel;
    QRLabel38: TQRLabel;
    qrlValorIR: TQRLabel;
    QRLabel40: TQRLabel;
    qrlValorINSS: TQRLabel;
    QRLabel42: TQRLabel;
    qrlValorCSLL: TQRLabel;
    QRLabel44: TQRLabel;
    QRShape13: TQRShape;
    QRShape14: TQRShape;
    QRLabel35: TQRLabel;
    QRLabel37: TQRLabel;
    QRLabel39: TQRLabel;
    QRLabel41: TQRLabel;
    QRLabel43: TQRLabel;
    QRLabel45: TQRLabel;
    QRLabel46: TQRLabel;
    QRLabel47: TQRLabel;
    QRLabel48: TQRLabel;
    QRLabel49: TQRLabel;
    QRLabel50: TQRLabel;
    QRLabel51: TQRLabel;
    QRLabel52: TQRLabel;
    QRLabel53: TQRLabel;
    QRLabel54: TQRLabel;
    QRLabel55: TQRLabel;
    QRLabel56: TQRLabel;
    QRShape15: TQRShape;
    QRShape16: TQRShape;
    qrlValorServicos1: TQRLabel;
    qrlValorServicos2: TQRLabel;
    qrlDescIncondicionado1: TQRLabel;
    qrlDescIncondicionado2: TQRLabel;
    qrlDescCondicionado: TQRLabel;
    qrlRetencoesFederais: TQRLabel;
    qrlOutrasRetencoes: TQRLabel;
    qrlValorIssRetido: TQRLabel;
    qrlValorLiquido: TQRLabel;
    QRShape17: TQRShape;
    qrlIncentivador: TQRLabel;
    qrlValorDeducoes: TQRLabel;
    qrlRegimeEspecial: TQRLabel;
    qrlOpcaoSimples: TQRLabel;
    qrlISSReter: TQRLabel;
    qrmPrefeitura: TQRMemo;
    qrmDescricao: TQRMemo;
    qrlDataServ: TQRLabel;
    QRShape22: TQRShape;
    QRLabel61: TQRLabel;
    qrlCodigoMunicipio: TQRLabel;
    qrmNatOperacao: TQRMemo;
    qrb_8_Canhoto: TQRChildBand;
    QRShape11: TQRShape;
    QRLabel26: TQRLabel;
    qrlPrestNomeCompEnt: TQRLabel;
    QRLabel28: TQRLabel;
    QRLabel60: TQRLabel;
    QRLabel59: TQRLabel;
    QRShape21: TQRShape;
    QRLabel58: TQRLabel;
    QRShape20: TQRShape;
    QRLabel57: TQRLabel;
    qrlNumeroNotaCompEnt: TQRLabel;
    QRShape19: TQRShape;
    QRShape18: TQRShape;
    procedure qrb_1_CabecalhoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_2_PrestadorServicoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_3_TomadorServicoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_5_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_6_ISSQNBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_7_OutrasInformacoesBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure cdsItensAfterScroll(DataSet: TDataSet);
    procedure QRNFSeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrb_8_CanhotoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
    FProvedor: TnfseProvedor;
    procedure Itens;
  public
    { Public declarations }
    procedure QuebradeLinha(const sQuebradeLinha: String);
  end;

implementation

uses
 StrUtils, DateUtils, ACBrValidador,
 ACBrUtil, ACBrDFeUtil, ACBrNFSeUtil, pnfsNFSe;

{$R *.dfm}

var
  FQuebradeLinha: String;

procedure TfqrDANFSeQRRetrato.cdsItensAfterScroll(DataSet: TDataSet);
//var
// intTamanhoDescricao: Integer;
begin
  inherited;

// intTamanhoDescricao:= Length(cdsItens.FieldByName( 'DESCRICAO' ).AsString);

end;

procedure TfqrDANFSeQRRetrato.Itens;
var
 i: Integer;
begin
 cdsItens.Close;
 cdsItens.CreateDataSet;
 cdsItens.Open;

 for i := 0 to FNFSe.Servico.ItemServico.Count -1 do
  begin
   cdsItens.Append;
   cdsItensCodigo.AsString    := '';
   cdsItensDescricao.AsString := trim(FNFSe.Servico.ItemServico.Items[i].Descricao);
//   cdsItensDescricao.AsString := trim(FNFSe.Servico.Descricao);
   cdsItens.Post;
  end;

 if FNFSe.Servico.ItemServico.Count = 0
  then begin
   cdsItens.Append;
   cdsItensCodigo.AsString    := '';
   cdsItensDescricao.AsString := '';
   cdsItens.Post;
  end;

 cdsItens.First;
end;

procedure TfqrDANFSeQRRetrato.qrb_1_CabecalhoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 t: integer;
 vStringStream: TStringStream;
begin
  inherited;
 (*
 if (FLogo <> '') and FilesExists(FLogo)
  then begin
   qriLogo.Picture.LoadFromFile(FLogo);
  end;
 *)

  // Alterado por Augusto Fontana - 17/09/2013
  if (FLogo <> '') then
    begin
      if FilesExists(FLogo) then
        qriLogo.Picture.LoadFromFile(FLogo)
      else
        begin
          vStringStream := TStringStream.Create(FLogo);
          try
            try
              qriLogo.Picture.Bitmap.LoadFromStream(vStringStream);
            except
            end;
          finally
            vStringStream.Free;
          end;
        end;
    end;

 qrmPrefeitura.Lines.Clear;

 qrmPrefeitura.Lines.Add(StringReplace( FPrefeitura,
                         ';', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

 qrlNumNF0.Caption   := FormatFloat('00000000000', StrToFloatDef(FNFSe.Numero, 0));

 if FNFSe.DataEmissao > 0
  then qrlDataServ.Caption := FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissao)
  else qrlDataServ.Caption := FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissaoRps);

 // Alterado em 27/12/2012  Daniel Jr -> passando parâmetro para Comprovante de Entrega.
 qrlNumeroNotaCompEnt.Caption := FormatFloat('00000000000', StrToFloatDef(FNFSe.Numero, 0));

 qrlEmissao.Caption := FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissao);
 qrlCodVerificacao.Caption := FNFSe.CodigoVerificacao;
 t:=length(FNFSe.Competencia);
 if t=6
  then qrlCompetencia.Caption := Copy(FNFSe.Competencia, 5, 2) + '/' + Copy(FNFSe.Competencia, 1, 4)
  else qrlCompetencia.Caption := Copy(FNFSe.Competencia, 6, 2) + '/' + Copy(FNFSe.Competencia, 1, 4);
 qrlNumeroRPS.Caption := FNFSe.IdentificacaoRps.Numero;
 qrlNumNFSeSubstituida.Caption := FNFSe.NfseSubstituida;
 if trim(FNFSe.Servico.CodigoMunicipio)<>''
  then qrlCodigoMunicipio.Caption := FNFSe.Servico.CodigoMunicipio + ' - ' +
                                     CodCidadeToCidade(StrToInt(FNFSe.Servico.CodigoMunicipio))
  else qrlCodigoMunicipio.Caption := '';
end;

procedure TfqrDANFSeQRRetrato.qrb_2_PrestadorServicoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 Ok: Boolean;
  vStringStream: TStringStream;
begin
  inherited;
 (*
 if (FPrestLogo <> '') and FilesExists(FPrestLogo)
  then begin
   qriPrestLogo.Picture.LoadFromFile(FPrestLogo);
  end;
 *)

  // Alterado por Augusto Fontana - 17/09/2013
  if (FPrestLogo <> '') then
    begin
      if FilesExists(FPrestLogo) then
        qriPrestLogo.Picture.LoadFromFile(FPrestLogo)
      else
        begin
          vStringStream := TStringStream.Create(FPrestLogo);
          try
            try
              qriPrestLogo.Picture.Bitmap.LoadFromStream(vStringStream);
            except
            end;
          finally
            vStringStream.Free;
          end;
        end;
    end;

 qrlPrestCNPJ.Caption := FormatarCNPJ( FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj );
 qrlPrestInscMunicipal.Caption := FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
 qrlPrestNome.Caption := FNFSe.PrestadorServico.RazaoSocial;
 // Alterado em 27/12/2012  Daniel Jr -> passando parâmetro para Comprovante de Entrega.
 qrlPrestNomeCompEnt.Caption := FNFSe.PrestadorServico.RazaoSocial;

 qrlPrestEndereco.Caption := Trim( FNFSe.PrestadorServico.Endereco.Endereco )+', '+
                             Trim( FNFSe.PrestadorServico.Endereco.Numero )+' - '+
                             Trim( FNFSe.PrestadorServico.Endereco.Bairro )+
                             ' - CEP: '+
                             FormatarCEP( Poem_Zeros( FNFSe.PrestadorServico.Endereco.CEP, 8 ) );
 qrlPrestComplemento.Caption := FNFSe.PrestadorServico.Endereco.Complemento;
 qrlPrestTelefone.Caption := FormatarFone( FNFSe.PrestadorServico.Contato.Telefone );
 qrlPrestMunicipio.Caption := FNFSe.PrestadorServico.Endereco.CodigoMunicipio +
  ' - ' + FNFSe.PrestadorServico.Endereco.xMunicipio;
 qrlPrestUF.Caption := FNFSe.PrestadorServico.Endereco.UF;
 qrlPrestEmail.Caption := FNFSe.PrestadorServico.Contato.Email;

 FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)));
end;

procedure TfqrDANFSeQRRetrato.qrb_3_TomadorServicoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

 if Length(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)<=11
  then qrlTomaCNPJ.Caption := FormatarCPF( FNFSe.Tomador.IdentificacaoTomador.CpfCnpj )
  else qrlTomaCNPJ.Caption := FormatarCNPJ( FNFSe.Tomador.IdentificacaoTomador.CpfCnpj );

 qrlTomaInscMunicipal.Caption := FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal;
 qrlTomaNome.Caption := FNFSe.Tomador.RazaoSocial;
 qrlTomaEndereco.Caption := Trim( FNFSe.Tomador.Endereco.Endereco )+', '+
                            Trim( FNFSe.Tomador.Endereco.Numero )+' - '+
                            Trim( FNFSe.Tomador.Endereco.Bairro )+
                            ' - CEP: '+
                            FormatarCEP( Poem_Zeros( FNFSe.Tomador.Endereco.CEP, 8 ) );
 qrlTomaComplemento.Caption := FNFSe.Tomador.Endereco.Complemento;
 qrlTomaTelefone.Caption := FormatarFone( FNFSe.Tomador.Contato.Telefone );
 qrlTomaMunicipio.Caption := FNFSe.Tomador.Endereco.CodigoMunicipio +
  ' - ' + FNFSe.Tomador.Endereco.xMunicipio;
 qrlTomaUF.Caption := FNFSe.Tomador.Endereco.UF;
 qrlTomaEmail.Caption := FNFSe.Tomador.Contato.Email;

 // Mensagem para modo Homologacao.
 qrlMsgTeste.Visible := False;
 qrlMsgTeste.Enabled := False;
 if (FNFSe.NfseCancelamento.DataHora<>0) or FNFSeCancelada
  then begin
   qrlMsgTeste.Caption  := 'NFS-e CANCELADA';
   qrlMsgTeste.Visible  := True;
   qrlMsgTeste.Enabled := True;
  end;
 qrlMsgTeste.Repaint;
end;

procedure TfqrDANFSeQRRetrato.qrb_5_ItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

 qrmDescricao.Lines.BeginUpdate;
 qrmDescricao.Lines.Clear;

// qrmDescricao.Lines.Add( StringReplace( FNFSe.Servico.Discriminacao,
//                         ';', #13#10, [rfReplaceAll, rfIgnoreCase] ) );

 qrmDescricao.Lines.Add( StringReplace( FNFSe.Servico.Discriminacao,
                         FQuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase] ) );

 qrmDescricao.Lines.EndUpdate;
end;

procedure TfqrDANFSeQRRetrato.qrb_6_ISSQNBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 MostrarObra: Boolean;
 Municipio1, Municipio2, sNatOperacao: String;
begin
  inherited;

  PrintBand := (QRNFSe.PageNumber = 1);
// qrlValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ '+
//    FormatFloat( FNFSe.Servico.Valores.ValorLiquidoNfse );

 qrlValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ '+
    FormatFloat('#,##0.00', FNFSe.Servico.Valores.ValorServicos );

 qrmCodServico.Lines.BeginUpdate;
 qrmCodServico.Lines.Clear;

 if trim(FNFSe.Servico.Descricao) = ''
  then begin
   (*
   qrmCodServico.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                           FNFSe.Servico.CodigoTributacaoMunicipio);
   qrmCodServico.Lines.Add(FNFSe.Servico.xItemListaServico);
   *)
   qrmCodServico.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                           FNFSe.Servico.CodigoTributacaoMunicipio + ' - ' +
                           FNFSe.Servico.xItemListaServico);
  end
  else begin
   (*
   qrmCodServico.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                           FNFSe.Servico.CodigoTributacaoMunicipio);
   qrmCodServico.Lines.Add(FNFSe.Servico.Descricao);
   *)
   qrmCodServico.Lines.Add(FNFSe.Servico.ItemListaServico + ' / ' +
                           FNFSe.Servico.CodigoTributacaoMunicipio + ' - ' +
                           FNFSe.Servico.Descricao);
  end;

 qrmCodServico.Lines.EndUpdate;

 qrlCodObra.Caption := trim(FNFSe.ConstrucaoCivil.CodigoObra);
 qrlCodART.Caption  := trim(FNFSe.ConstrucaoCivil.Art);

 MostrarObra := (qrlCodObra.Caption <> '') or (qrlCodART.Caption <> '');

 qrsLinhaH1.Enabled          := MostrarObra;
 qrlTituloConstCivil.Enabled := MostrarObra;
 qrlCodigoObra.Enabled       := MostrarObra;
 qrlCodObra.Enabled          := MostrarObra;
 qrlCodigoArt.Enabled        := MostrarObra;
 qrlCodART.Enabled           := MostrarObra;

 qrlValorPIS.Caption    := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorPis );
 qrlValorCOFINS.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorCofins );
 qrlValorIR.Caption     := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorIr );
 qrlValorINSS.Caption   := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorInss );
 qrlValorCSLL.Caption   := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorCsll );

 qrlValorServicos1.Caption      := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorServicos );
 qrlDescIncondicionado1.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.DescontoIncondicionado );
 qrlDescCondicionado.Caption    := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.DescontoCondicionado );
 qrlRetencoesFederais.Caption   := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorPis +
                                     FNFSe.Servico.Valores.ValorCofins + FNFSe.Servico.Valores.ValorInss +
                                     FNFSe.Servico.Valores.ValorIr + FNFSe.Servico.Valores.ValorCsll );
 qrlOutrasRetencoes.Caption     := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.OutrasRetencoes );

 qrlValorIssRetido.Caption      := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorIssRetido );

 qrlValorLiquido.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorLiquidoNfse );

 // TnfseNaturezaOperacao = ( noTributacaoNoMunicipio, noTributacaoForaMunicipio, noIsencao, noImune, noSuspensaDecisaoJudicial, noSuspensaProcedimentoAdministrativo )

 case StrToIntDef(FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 0) of
  4318705: begin  // São Leopoldo/RS
            Municipio1 := 'em São Leopoldo';
            Municipio2 := 'fora de São Leopoldo';
           end;
  4303103: begin  // Cachoeirinha/RS
            Municipio1 := 'em Cachoeirinha';
            Municipio2 := 'fora de Cachoeirinha';
//            Municipio1 := 'no Municipio de Cachoeirinha';
//            Municipio2 := 'fora do Municipio de Cachoeirinha';
           end;
  else begin
        Municipio1 := 'no município';
        Municipio2 := 'fora do município';
       end;
 end;

 qrmNatOperacao.Lines.Clear;

 case FNFSe.NaturezaOperacao of
  noTributacaoNoMunicipio   : sNatOperacao := '1 - Tributação no município';
  noTributacaoForaMunicipio : sNatOperacao := '2 - Tributação fora do município';
  noIsencao                 : sNatOperacao := '3 - Isenção';
  noImune                   : sNatOperacao := '4 - Imune';
  noSuspensaDecisaoJudicial : sNatOperacao := '5 - Exigibilidade susp. por decisão judicial';

  noSuspensaProcedimentoAdministrativo : sNatOperacao := '6 - Exigibilidade susp. por proced. adm.';

  noTributacaoNoMunicipio51         : sNatOperacao := '51- Imposto devido ' + Municipio1 + ', com obrigação de retenção na fonte';     // alterado por Rafael Müller para o provedor thema
  noTributacaoNoMunicipioSemISS52   : sNatOperacao := '52 - Imposto devido ' + Municipio1 + ', sem obrigação de retenção na fonte';
  noNaoTributa58                    : sNatOperacao := '58 - Não tributável';
  noSimplesNacional59               : sNatOperacao := '59 - Imposto recolhido pelo regime único de arrecadação Simples Nacional';
  noTributacaoNoMunicipio61         : sNatOperacao := '61 - Imposto devido ' + Municipio1 + ', com obrigação de retenção na fonte';
  noTributacaoNoMunicipioSemISS62   : sNatOperacao := '62 - Imposto devido ' + Municipio1 + ', sem obrigação de retenção na fonte';
  noTributacaoForaMunicipio63       : sNatOperacao := '63 - Imposto devido ' + Municipio2 + ', com obrigação de retenção na fonte';
  noTributacaoForaMunicipioSemISS64 : sNatOperacao := '64 - Imposto devido ' + Municipio2 + ', sem obrigação de retenção na fonte';
  noNaoTributa68                    : sNatOperacao := '68 - Não tributável';
  noSimplesNacional69               : sNatOperacao := '69 - Imposto recolhido pelo regime único de arrecadação Simples Nacional';
  noNaoTributa78                    : sNatOperacao := '78 - Não tributável';
 end;

 qrmNatOperacao.Lines.Add(sNatOperacao);
 
 // TnfseRegimeEspecialTributacao = ( retNenhum, retMicroempresaMunicipal, retEstimativa, retSociedadeProfissionais, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP )
 case FNFSe.RegimeEspecialTributacao of
  retNenhum                    : qrlRegimeEspecial.Caption := '0 - Nenhum';
  retMicroempresaMunicipal     : qrlRegimeEspecial.Caption := '1 - Microempresa municipal';
  retEstimativa                : qrlRegimeEspecial.Caption := '2 - Estimativa';
  retSociedadeProfissionais    : qrlRegimeEspecial.Caption := '3 - Sociendade de profissionais';
  retCooperativa               : qrlRegimeEspecial.Caption := '4 - Cooperativa';
  retMicroempresarioIndividual : qrlRegimeEspecial.Caption := '5 - Microempresário Individual (MEI)';
  retMicroempresarioEmpresaPP  : qrlRegimeEspecial.Caption := '6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
 end;

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.OptanteSimplesNacional of
  snSim : qrlOpcaoSimples.Caption := 'Sim';
  snNao : qrlOpcaoSimples.Caption := 'Não';
 end;

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.IncentivadorCultural of
  snSim : qrlIncentivador.Caption := 'Sim';
  snNao : qrlIncentivador.Caption := 'Não';
 end;

 qrlValorServicos2.Caption      := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorServicos );
 qrlValorDeducoes.Caption       := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorDeducoes );
 qrlDescIncondicionado2.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.DescontoIncondicionado );
 qrlBaseCalc.Caption            := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.BaseCalculo );

 // Checar os provedores que retornam a Aliquota dividida por 100
 // e multiplicar por 100 para que seja apresentada no formado x.xx %
 if FProvedor in [proThema, proWebISS, proActCon]
  then qrlAliquota.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.Aliquota * 100)
  else qrlAliquota.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.Aliquota);

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.Servico.Valores.IssRetido of
  stRetencao     : qrlISSReter.Caption := 'Sim';
  stNormal       : qrlISSReter.Caption := 'Não';
  stSubstituicao : qrlISSReter.Caption := 'ST';
 end;

 // Alterado esta linha em 27/12/2012  Daniel Jr - Pois o ICMS não estava sendo dividido por 100) Ex 1,00 estava 100,00
 // Alterado por Italo em 17/07/2013 (> removido a divisão por 100
 // qrlValorISS.Caption := FormatFloat( (FNFSe.Servico.Valores.ValorIss / 100) );

 qrlValorISS.Caption := FormatFloat('#,##0.00',  FNFSe.Servico.Valores.ValorIss );

// qrlValorCredito.Caption := FormatFloat( FNFSe.ValorCredito );

end;

procedure TfqrDANFSeQRRetrato.qrb_7_OutrasInformacoesBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (QRNFSe.PageNumber = 1);

 qrmDadosAdicionais.Lines.BeginUpdate;
 qrmDadosAdicionais.Lines.Clear;

 qrmDadosAdicionais.Lines.Add(StringReplace( trim(FNFSe.OutrasInformacoes),
                         ';', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

 qrmDadosAdicionais.Lines.EndUpdate;

 // imprime data e hora da impressao
 QrlDataHoraImpressao.Caption := 'DATA E HORA DA IMPRESSÃO: ' + FormatDateTime('dd/mm/yyyy hh:nn',Now);

 // imprime usuario
 if FUsuario <> ''
  then begin
   QrlDataHoraImpressao.Caption := QrlDataHoraImpressao.Caption + '   USUÁRIO: ' + FUsuario;
  end;

 // imprime sistema
 if FSistema <> ''
  then begin
   qrlSistema.Caption := 'Desenvolvido por ' + FSistema;
  end
  else begin
   qrlSistema.Caption := '';
  end;

end;

procedure TfqrDANFSeQRRetrato.qrb_8_CanhotoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (QRNFSe.PageNumber = 1) and FImprimeCanhoto;

end;

procedure TfqrDANFSeQRRetrato.QRNFSeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
begin
  inherited;

  if FImprimeCanhoto
   then begin
    qrb_5_Itens.Height  := 106;
    qrmDescricao.Height := 100;
   end
   else begin
    qrb_5_Itens.Height  := 196;
    qrmDescricao.Height := 190;
   end;

  Itens;

  QRNFSe.ReportTitle := 'NFS-e: ' + FNFSe.Numero;

  QRNFSe.Page.TopMargin    := FMargemSuperior * 100;
  QRNFSe.Page.BottomMargin := FMargemInferior * 100;
  QRNFSe.Page.LeftMargin   := FMargemEsquerda * 100;
  QRNFSe.Page.RightMargin  := FMargemDireita  * 100;

end;

procedure TfqrDANFSeQRRetrato.QuebradeLinha(const sQuebradeLinha: String);
begin
  FQuebradeLinha := sQuebradeLinha;
end;

end.
