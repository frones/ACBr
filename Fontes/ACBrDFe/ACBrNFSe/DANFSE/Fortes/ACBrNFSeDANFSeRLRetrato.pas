{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{ Biblioteca multiplataforma de componentes Delphi para                        }
{ Emissão de Nota Fiscal de Serviço                                            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015 Italo Jurisato Junior                  }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}


unit ACBrNFSeDANFSeRLRetrato;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ACBrNFSeDANFSeRL, RLFilters, RLPDFFilter, RLReport, DB,
  pnfsConversao, ACBrDelphiZXingQRCode ;

type

  { TfrlDANFSeRLRetrato }

  TfrlDANFSeRLRetrato = class(TfrlDANFSeRL)
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
    RLLabel8: TRLLabel;
    rllCodVerificacao: TRLLabel;
    RLLabel7: TRLLabel;
    rllCompetencia: TRLLabel;
    RLLabel18: TRLLabel;
    rllNumeroRps: TRLLabel;
    RLLabel20: TRLLabel;
    rllNumNFSeSubstituida: TRLLabel;
    rlmPrefeitura: TRLMemo;
    rlbPrestador: TRLBand;
    RLLabel29: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel31: TRLLabel;
    RLLabel32: TRLLabel;
    rllPrestMunicipio: TRLLabel;
    rllPrestInscMunicipal: TRLLabel;
    rllPrestEndereco: TRLLabel;
    rllPrestCNPJ: TRLLabel;
    rliPrestLogo: TRLImage;
    RLLabel2: TRLLabel;
    RLLabel1: TRLLabel;
    rllPrestNome: TRLLabel;
    RLLabel9: TRLLabel;
    rllPrestUF: TRLLabel;
    RLLabel22: TRLLabel;
    rllPrestComplemento: TRLLabel;
    RLLabel23: TRLLabel;
    rllPrestTelefone: TRLLabel;
    RLLabel24: TRLLabel;
    rllPrestEmail: TRLLabel;
    rlbTomador: TRLBand;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
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
    rllNatOperacao: TRLLabel;
    rllValorDeducoes: TRLLabel;
    rllRegimeEspecial: TRLLabel;
    rllOpcaoSimples: TRLLabel;
    rllISSReter: TRLLabel;
    rllMsgTeste: TRLLabel;
    rbOutrasInformacoes: TRLBand;
    rlmDadosAdicionais: TRLMemo;
    rllDataHoraImpressao: TRLLabel;
    rllSistema: TRLLabel;
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
    function ManterAliquota(dAliquota: Double): String;
  public
    { Public declarations }
    procedure QuebradeLinha(const sQuebradeLinha: String);
  end;

var
  frlDANFSeRLRetrato: TfrlDANFSeRLRetrato;

implementation

uses
 StrUtils, DateUtils, ACBrUtil, pnfsNFSe, ACBrValidador;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FQuebradeLinha: String;

{ TfrlDANFSeRLRetrato }

procedure TfrlDANFSeRLRetrato.QuebradeLinha(const sQuebradeLinha: String);
begin
  FQuebradeLinha := sQuebradeLinha;
end;

procedure TfrlDANFSeRLRetrato.rbOutrasInformacoesBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  QrCode      : TDelphiZXingQRCode;
  QrCodeBitmap: TBitmap;
  QRCodeData  : String;
  rlImgQrCode : TRLImage;
  Row, Column : Integer;
begin
  inherited;
  rlmDadosAdicionais.Lines.BeginUpdate;
  rlmDadosAdicionais.Lines.Clear;

  if FNFSe.OutrasInformacoes <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(FNFSe.OutrasInformacoes, ';', #13#10, [rfReplaceAll,rfIgnoreCase]))
  else if FOutrasInformacaoesImp <> '' then
    rlmDadosAdicionais.Lines.Add(StringReplace(FOutrasInformacaoesImp, ';', #13#10, [rfReplaceAll,rfIgnoreCase]));

  if pos('http://', LowerCase( FNFSe.OutrasInformacoes) ) > 0 then
  begin
    rlmDadosAdicionais.Width := 643;

    rlImgQrCode          := TRLImage.Create(rbOutrasInformacoes);
    rlImgQrCode.Parent   := rbOutrasInformacoes;
    rlImgQrCode.Stretch  := True;
    rlImgQrCode.AutoSize := False;
    rlImgQrCode.Center   := true;
    rlImgQrCode.SetBounds(648, 3, 90, 90);
    rlImgQrCode.BringToFront;

    QRCodeData   := Trim(MidStr(FNFSe.OutrasInformacoes, pos('http://', LowerCase( FNFSe.OutrasInformacoes)), Length(FNFSe.OutrasInformacoes) ));
    QRCode       := TDelphiZXingQRCode.Create;
    QRCodeBitmap := TBitmap.Create;
    try
      QRCode.Data      := QRCodeData;
      QRCode.Encoding  := qrUTF8NoBOM;
      QRCode.QuietZone := 1;

      QRCodeBitmap.Width  := QRCode.Columns;
      QRCodeBitmap.Height := QRCode.Rows;

      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Column := 0 to QRCode.Columns - 1 do
        begin
          if (QRCode.IsBlack[Row, Column]) then
            QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
          else
            QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
        end;
      end;

      rlImgQrCode.Picture.Bitmap.Assign(QRCodeBitmap);
    finally
      QRCode.Free;
      QRCodeBitmap.Free;
    end;
  end;

  rlmDadosAdicionais.Lines.EndUpdate;
  rllDataHoraImpressao.Caption := Format(ACBrStr('DATA E HORA DA IMPRESSÃO: %s') , [FormatDateTime('dd/mm/yyyy hh:nn',Now)]);

  if FUsuario <> '' then
    rllDataHoraImpressao.Caption := Format(ACBrStr('%s   USUÁRIO: %s'), [rllDataHoraImpressao.Caption, FUsuario]);

  // imprime sistema
  if FSistema <> '' then
  begin
    rllSistema.Caption := Format('Desenvolvido por %s' , [FSistema]);
  end
  else
  begin
    rllSistema.Caption := '';
  end;

  //Exibe canhoto
  rlbCanhoto.Visible:= FImprimeCanhoto;
end;

procedure TfrlDANFSeRLRetrato.rlbCabecalhoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vStringStream: TStringStream;
begin
  inherited;
  if (FLogo <> '') then
  begin
    if FilesExists(FLogo) then
      rliLogo.Picture.LoadFromFile(FLogo)
    else
    begin
      vStringStream := TStringStream.Create(FLogo);
      try
        try
          vStringStream.Position := 0;
          rliLogo.Picture.Bitmap.LoadFromStream(vStringStream);
        except
        end;
      finally
        vStringStream.Free;
      end;
    end;
  end;

  rlmPrefeitura.Lines.Clear;
  rlmPrefeitura.Lines.Add(StringReplace( FPrefeitura, ';', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

  With FNFSe do
  begin
    rllNumNF0.Caption         := FormatFloat('00000000000'        , StrToFloatDef(Numero, 0));
    rllEmissao.Caption        := FormatDateTime('dd/mm/yyyy hh:nn', DataEmissao);
    rllCodVerificacao.Caption := CodigoVerificacao;

    if length( Competencia ) = 6 then
      rllCompetencia.Caption := Copy(Competencia, 5, 2) + '/' + Copy(Competencia, 1, 4)
    else
    begin
      if length( Competencia ) = 10 then // dd/mm/aaaa ou aaaa/mm/dd
      begin
        if (Pos('/', Competencia) = 3) or (Pos('-', Competencia) = 3) then
          rllCompetencia.Caption := Copy(Competencia, 4, Length(Competencia) )
        else
          rllCompetencia.Caption := Copy(Competencia, 6, 2) + '/' + Copy(Competencia, 1, 4);
      end
      else
        rllCompetencia.Caption := Copy(Competencia, 6, 2) + '/' + Copy(Competencia, 1, 4);
    end;

    rllNumeroRPS.Caption          := IdentificacaoRps.Numero;
    rllNumNFSeSubstituida.Caption := NfseSubstituida;
    rllMunicipioPrestacaoServico.Caption := CodCidadeToCidade(StrToIntDef(Servico.CodigoMunicipio, 0));
  end;
end;

procedure TfrlDANFSeRLRetrato.rlbItensServicoBeforePrint(Sender: TObject;
   var PrintIt: Boolean);
begin
  with FNFSe.Servico.ItemServico.Items[FNumItem] do
  begin
    txtServicoQtde.Caption := FormatFloatBr( Quantidade );
    rlmServicoDescricao.Lines.Clear;
    rlmServicoDescricao.Lines.Add( StringReplace( Descricao, FQuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase] ) );
    txtServicoUnitario.Caption := FormatFloatBr( ValorUnitario );
    txtServicoTotal.Caption    := FormatFloatBr( ValorTotal );
  end;
end;

procedure TfrlDANFSeRLRetrato.rlbISSQNBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  MostrarObra, MostrarNaturezaOperacao: Boolean;
begin
  inherited;
  RLLabel16.Visible := False;

  With FNFSe do
  begin
    rllNatOperacao.Caption    := NaturezaOperacaoDescricao( NaturezaOperacao );
    MostrarNaturezaOperacao   := rllNatOperacao.Caption<>'';
    RLLabel137.Visible        := MostrarNaturezaOperacao;
    rllRegimeEspecial.Caption := nfseRegimeEspecialTributacaoDescricao( RegimeEspecialTributacao );
    rllOpcaoSimples.Caption   := SimNao( Integer ( OptanteSimplesNacional ) );
    rllIncentivador.Caption   := SimNao( Integer ( IncentivadorCultural ) );
    rllCodObra.Caption        := ConstrucaoCivil.CodigoObra;
    rllCodART.Caption         := ConstrucaoCivil.Art;

    MostrarObra                 := (rllCodObra.Caption<>'') or (rllCodART.Caption<>'');
    rlsLinhaH1.Visible          := MostrarObra;
    rllTituloConstCivil.Visible := MostrarObra;
    rllCodigoObra.Visible       := MostrarObra;
    rllCodObra.Visible          := MostrarObra;
    rllCodigoArt.Visible        := MostrarObra;
    rllCodART.Visible           := MostrarObra;

    with Servico.Valores  do
    begin
      rllValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ '+ FormatFloat('#,##0.00' , ValorServicos );
      rlmCodServico.Lines.Clear;

      if Servico.xItemListaServico <> '' then
      begin
        RLLabel16.Visible := True;
        if FAtividade <> '' then
          rlmCodServico.Lines.Append('Atividade: ' + FAtividade);
        rlmCodServico.Lines.Append( Servico.ItemListaServico + ' - '+ Servico.xItemListaServico);
      end
      else
      begin
        if FAtividade <> '' then
        begin
          RLLabel16.Visible := True;
          RLLabel16.Caption := 'Atividade:';
          rlmCodServico.Lines.Append(FAtividade);
        end
      end;
      rllValorPIS.Caption            := FormatFloat('#,##0.00', ValorPis );
      rllValorCOFINS.Caption         := FormatFloat('#,##0.00', ValorCofins );
      rllValorIR.Caption             := FormatFloat('#,##0.00', ValorIr );
      rllValorINSS.Caption           := FormatFloat('#,##0.00', ValorInss );
      rllValorCSLL.Caption           := FormatFloat('#,##0.00', ValorCsll );
      rllValorServicos1.Caption      := FormatFloat('#,##0.00', ValorServicos );
      rllDescIncondicionado1.Caption := FormatFloat('#,##0.00', DescontoIncondicionado );
      rllDescCondicionado.Caption    := FormatFloat('#,##0.00', DescontoCondicionado );
      rllRetencoesFederais.Caption   := FormatFloat('#,##0.00', ValorPis +
                                                                 ValorCofins +
                                                                 ValorInss +
                                                                 ValorIr +
                                                                 ValorCsll );
      rllOutrasRetencoes.Caption     := FormatFloat('#,##0.00', OutrasRetencoes );
      rllValorIssRetido.Caption      := FormatFloat('#,##0.00', ValorIssRetido );
      rllValorLiquido.Caption        := FormatFloat('#,##0.00', ValorLiquidoNfse );
      rllValorServicos2.Caption      := FormatFloat('#,##0.00', ValorServicos );
      rllValorDeducoes.Caption       := FormatFloat('#,##0.00', ValorDeducoes );
      rllDescIncondicionado2.Caption := FormatFloat('#,##0.00', DescontoIncondicionado );
      rllBaseCalc.Caption            := FormatFloat('#,##0.00', BaseCalculo );
      rllAliquota.Caption            := ManterAliquota ( Aliquota );
      rllISSReter.Caption            := SituacaoTributariaDescricao( IssRetido );
      rllValorISS.Caption            := FormatFloat('#,##0.00',ValorIss);
    end;
  end;
end;

procedure TfrlDANFSeRLRetrato.rlbItensBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  rlmDescricao.Lines.Clear;
  rlmDescricao.Lines.Add( StringReplace( FNFSe.Servico.Discriminacao,
                          FQuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase] ) );
end;

procedure TfrlDANFSeRLRetrato.rlbPrestadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vStringStream: TStringStream;
begin
  inherited;
  if (FPrestLogo <> '') then
  begin
    if FilesExists(FPrestLogo) then
      rliPrestLogo.Picture.LoadFromFile(FPrestLogo)
    else
    begin
      vStringStream := TStringStream.Create(FPrestLogo);
      try
        try
          rliPrestLogo.Picture.Bitmap.LoadFromStream(vStringStream);
        except
        end;
      finally
        vStringStream.Free;
      end;
    end;
  end;

  With FNFSe do
  begin
    with PrestadorServico do
    begin
      with IdentificacaoPrestador do
      begin
        rllPrestCNPJ.Caption          := FormatarCNPJ( Cnpj );
        rllPrestInscMunicipal.Caption := IfThen( InscricaoMunicipal <> '' , InscricaoMunicipal  , FInscMunicipal );
        rllPrestNome.Caption          := IfThen( RazaoSocial <> ''        , RazaoSocial         , FRazaoSocial);
        with Tomador.IdentificacaoTomador do
          rllTomaInscEstadual.Caption := IfThen( InscricaoEstadual <> ''  , InscricaoEstadual   , FT_InscEstadual );
      end;

      With Endereco do
      begin
        rllPrestEndereco.Caption    := IfThen( Endereco <> '' , Trim( Endereco )+', '+
                                                                  Trim( Numero )+' - '+
                                                                  Trim( Bairro )+
                                                                  ' - CEP: '+
                                                                  FormatarCEP( CEP ) ,
                                                                                                  Trim(FEndereco) ) ;
        rllPrestComplemento.Caption := IfThen( Complemento <> '', Complemento , FComplemento);
        rllPrestMunicipio.Caption   := IfThen( xMunicipio <> '' , CodigoMunicipio + ' - ' + xMunicipio , FMunicipio);
        rllPrestUF.Caption          := IfThen( UF <> ''         , UF  , FUF);
      end;

      With Contato do
      begin
        rllPrestTelefone.Caption := IfThen( Telefone <> '' , FormatarFone( Telefone) , FormatarFone(FFone) );
        rllPrestEmail.Caption    := IfThen( Email <> '' , Email , FEMail_Prestador);
      end;

      rllPrestNomeEnt.Caption   := IfThen(RazaoSocial <> '', RazaoSocial, FRazaoSocial);
      rllNumNF0Ent.Caption      := FormatFloat('00000000000', StrToFloatDef(Numero, 0));
      rllTomadorNomeEnt.Caption := ACBrStr('Emissão:') + FormatDateTime('dd/mm/yy',DataEmissao) +
                                         '-Tomador:'+Tomador.RazaoSocial+
                                         '-Total:' + FormatFloat('##,##0.00', Servico.Valores.ValorLiquidoNfse) ;
    end;
  end;
end;

procedure TfrlDANFSeRLRetrato.rlbTomadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;
  With FNFSe do
  begin
    With Tomador do
    begin
      rllTomaNome.Caption := RazaoSocial;
      with IdentificacaoTomador do
      begin
        if Length( CpfCnpj)<=11
        then
          rllTomaCNPJ.Caption := FormatarCPF( CpfCnpj )
        else
          rllTomaCNPJ.Caption := FormatarCNPJ( CpfCnpj );

        rllTomaInscMunicipal.Caption := IfThen( InscricaoMunicipal <> '' , InscricaoMunicipal , FT_InscMunicipal);
      end;

      With Endereco do
      begin
        if Endereco <> '' then
        begin
          rllTomaEndereco.Caption :=  Trim(Endereco) + ', '  +
                                      Trim(Numero )  + ' - ' +
                                      Trim(Bairro )  + ' - CEP: ' +
                                      FormatarCEP(CEP);
        end
        else
         rllTomaEndereco.Caption := Trim(FT_Endereco) + ' - CEP: ' +
                                    FormatarCEP(CEP);

        rllTomaComplemento.Caption := IfThen( Complemento <> '' , Complemento , FT_Complemento);

        rllTomaMunicipio.Caption := CodigoMunicipio + ' - ' + xMunicipio;
        rllTomaUF.Caption        := UF;
      end;

      with Contato do
      begin
        rllTomaTelefone.Caption := IfThen( Telefone <> '' , FormatarFone(Telefone) , FormatarFone(FT_Fone));
        rllTomaEmail.Caption    := IfThen( Email    <> '' , Email , FT_Email);
      end;

     rllMsgTeste.Visible := False;
     rllMsgTeste.Enabled := False;
    end;

    if NfseCancelamento.DataHora <> 0 then
    begin
      rllMsgTeste.Caption := 'NFS-e CANCELADA';
      rllMsgTeste.Visible := True;
      rllMsgTeste.Enabled := True;
    end;

    rllMsgTeste.Repaint;
  end;
end;

procedure TfrlDANFSeRLRetrato.RLNFSeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

  RLNFSe.Title := 'NFS-e: ' + FNFSe.Numero;

  RLNFSe.Margins.TopMargin    := FMargemSuperior * 10;
  RLNFSe.Margins.BottomMargin := FMargemInferior * 10;
  RLNFSe.Margins.LeftMargin   := FMargemEsquerda * 10;
  RLNFSe.Margins.RightMargin  := FMargemDireita * 10;

  rlbItens.Visible := Not(FDetalharServico);
  rlbHeaderItensDetalhado.Visible := FDetalharServico;
  subItens.Visible := FDetalharServico;
end;

procedure TfrlDANFSeRLRetrato.subItensDataRecord(Sender: TObject;
   RecNo: Integer; CopyNo: Integer; var Eof: Boolean;
   var RecordAction: TRLRecordAction);
begin
  inherited;
  FNumItem := RecNo - 1 ;
  Eof := (RecNo > FNFSe.Servico.ItemServico.Count) ;
  RecordAction := raUseIt ;
end;


function TfrlDANFSeRLRetrato.ManterAliquota(dAliquota: Double): String;
begin
  // thema precisa ser desta forma pois usa aliquota 2,5 => 0,025
  if (dAliquota > 0) and (dAliquota < 1) then
    Result := FormatFloat('#,##0.00', dAliquota * 100 )
  else
    Result := FormatFloat('#,##0.00', dAliquota );
end;

end.
