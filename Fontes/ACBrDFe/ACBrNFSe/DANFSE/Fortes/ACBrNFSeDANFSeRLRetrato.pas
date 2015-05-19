{$WARNINGS OFF}
{$HINTS OFF}
{$I ACBr.inc}

unit ACBrNFSeDANFSeRLRetrato;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ACBrNFSeDANFSeRL, RLFilters, RLPDFFilter, RLReport, DB,
  pnfsConversao, ACBrNFSeDANFSEClass, ACBrNFSeDANFSeRLClass, ACBrDelphiZXingQRCode ;

type
  TfrlDANFSeRLRetrato = class(TfrlDANFSeRL)
    rlbCabecalho: TRLBand;
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
    procedure rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbPrestadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbTomadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbISSQNBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rbOutrasInformacoesBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure RLNFSeBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
    procedure Itens;
  public
    { Public declarations }
    procedure QuebradeLinha(const sQuebradeLinha: String);
  end;

var
  frlDANFSeRLRetrato: TfrlDANFSeRLRetrato;

implementation

uses
 StrUtils, DateUtils, ACBrUtil, pnfsNFSe, ACBrDFeUtil; //Astrogildo em 13/12/12

{$R *.dfm}

var
  FQuebradeLinha: String;

{ TfrlDANFSeRLRetrato }

procedure TfrlDANFSeRLRetrato.Itens;
begin
 cdsItens.Close;
 cdsItens.CreateDataSet;
 cdsItens.Open;

 cdsItens.Append;
 cdsItens.FieldByName('DISCRIMINACAO').AsString := FNFSe.Servico.Discriminacao;
 cdsItens.Post;

 cdsItens.First;
end;

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

  If FNFSe.OutrasInformacoes <> '' Then
    rlmDadosAdicionais.Lines.Add(StringReplace(FNFSe.OutrasInformacoes, ';', #13#10, [rfReplaceAll,rfIgnoreCase]))
  Else If FOutrasInformacaoesImp <> '' Then
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

//      rlImgQrCode.Assign(QRCodeBitmap);
      rlImgQrCode.Picture.Bitmap.Assign(QRCodeBitmap);
    finally
      QRCode.Free;
      QRCodeBitmap.Free;
    end;
  end;

  rlmDadosAdicionais.Lines.EndUpdate;

  // imprime data e hora da impressao
  rllDataHoraImpressao.Caption := Format('DATA E HORA DA IMPRESSÃO: %s' , [FormatDateTime('dd/mm/yyyy hh:nn',Now)]);

  // imprime usuario
  if FUsuario <> '' then
  begin
    rllDataHoraImpressao.Caption := Format('%s   USUÁRIO: %s', [rllDataHoraImpressao.Caption, FUsuario]);
  end;

  // imprime sistema
  if FSistema <> '' then
  begin
    rllSistema.Caption := Format('Desenvolvido por %s' , [FSistema]);
  end
  else
  begin
    rllSistema.Caption := '';
  end;
end;

procedure TfrlDANFSeRLRetrato.rlbCabecalhoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
 t: integer;
 vStringStream: TStringStream;
begin
  inherited;
 (*
 if (FLogo <> '') and FilesExists(FLogo)
  then begin
   rliLogo.Picture.LoadFromFile(FLogo);
  end;
 *)
 // Alterado por Augusto Fontana - 17/09/2013
 if (FLogo <> '') then
   begin
     if FilesExists(FLogo) then
       rliLogo.Picture.LoadFromFile(FLogo)
     else
       begin
         vStringStream := TStringStream.Create(FLogo);
         try
           try
             rliLogo.Picture.Bitmap.LoadFromStream(vStringStream);
           except
           end;
         finally
           vStringStream.Free;
         end;
       end;
   end;

 rlmPrefeitura.Lines.Clear;

 rlmPrefeitura.Lines.Add(StringReplace( FPrefeitura,
                         ';', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

 rllNumNF0.Caption  := {FormatDateTime('yyyy', FNFSe.DataEmissao)+}
                       FormatFloat('00000000000', StrToFloat(FNFSe.Numero));
 rllEmissao.Caption := FormatDateTime(DateTimeToStr(FNFSe.DataEmissao));  //Astrogildo em 13/12/12
 rllCodVerificacao.Caption := FNFSe.CodigoVerificacao;
 t:=length(FNFSe.Competencia);
 if t=6
  then rllCompetencia.Caption := Copy(FNFSe.Competencia, 5, 2) + '/' + Copy(FNFSe.Competencia, 1, 4)
  else rllCompetencia.Caption := Copy(FNFSe.Competencia, 6, 2) + '/' + Copy(FNFSe.Competencia, 1, 4);
 rllNumeroRPS.Caption := FNFSe.IdentificacaoRps.Numero;
 rllNumNFSeSubstituida.Caption := FNFSe.NfseSubstituida;
end;

procedure TfrlDANFSeRLRetrato.rlbISSQNBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
 MostrarObra: Boolean;
begin
  inherited;

// rllValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ '+
//    NotaUtil.FormatFloat( FNFSe.Servico.Valores.ValorLiquidoNfse );

 rllValorTotal.Caption := 'VALOR TOTAL DA NOTA = R$ '+
    FormatFloat( FNFSe.Servico.Valores.ValorServicos ); //Astrogildo em 13/12/12

 rlmCodServico.Lines.Clear;

 If FNFSe.Servico.xItemListaServico <> '' Then Begin

   RLLabel16.Visible := True;
   rlmCodServico.Lines.Append('Atividade: ' + FAtividade);
   rlmCodServico.Lines.Append(FNFSe.Servico.ItemListaServico + ' - '+ FNFSe.Servico.xItemListaServico);
 End Else Begin

   If FAtividade <> '' Then Begin

     RLLabel16.Visible := True;
     RLLabel16.Caption := 'Atividade:';

     rlmCodServico.Lines.Append(FAtividade);
   End Else Begin

     RLLabel16.Visible := False;
   End;

 End;

 rllCodObra.Caption := FNFSe.ConstrucaoCivil.CodigoObra;
 rllCodART.Caption  := FNFSe.ConstrucaoCivil.Art;

 MostrarObra := (rllCodObra.Caption<>'') or (rllCodART.Caption<>'');
 rlsLinhaH1.Visible:=MostrarObra;
 rllTituloConstCivil.Visible:=MostrarObra;
 rllCodigoObra.Visible:=MostrarObra;
 rllCodObra.Visible:=MostrarObra;
 rllCodigoArt.Visible:=MostrarObra;
 rllCodART.Visible:=MostrarObra;

 rllValorPIS.Caption    := FormatFloat( FNFSe.Servico.Valores.ValorPis );//Astrogildo em 13/12/12
 rllValorCOFINS.Caption := FormatFloat( FNFSe.Servico.Valores.ValorCofins );//Astrogildo em 13/12/12
 rllValorIR.Caption     := FormatFloat( FNFSe.Servico.Valores.ValorIr );//Astrogildo em 13/12/12
 rllValorINSS.Caption   := FormatFloat( FNFSe.Servico.Valores.ValorInss );//Astrogildo em 13/12/12
 rllValorCSLL.Caption   := FormatFloat( FNFSe.Servico.Valores.ValorCsll );//Astrogildo em 13/12/12

 rllValorServicos1.Caption      := FormatFloat( FNFSe.Servico.Valores.ValorServicos );//Astrogildo em 13/12/12
 rllDescIncondicionado1.Caption := FormatFloat( FNFSe.Servico.Valores.DescontoIncondicionado );//Astrogildo em 13/12/12
 rllDescCondicionado.Caption    := FormatFloat( FNFSe.Servico.Valores.DescontoCondicionado );//Astrogildo em 13/12/12
 rllRetencoesFederais.Caption   := FormatFloat( FNFSe.Servico.Valores.ValorPis +//Astrogildo em 13/12/12
                                     FNFSe.Servico.Valores.ValorCofins + FNFSe.Servico.Valores.ValorInss +
                                     FNFSe.Servico.Valores.ValorIr + FNFSe.Servico.Valores.ValorCsll );
 rllOutrasRetencoes.Caption     := FormatFloat( FNFSe.Servico.Valores.OutrasRetencoes );//Astrogildo em 13/12/12
 
 rllValorIssRetido.Caption      := FormatFloat( FNFSe.Servico.Valores.ValorIssRetido );//Astrogildo em 13/12/12

 rllValorLiquido.Caption := FormatFloat( FNFSe.Servico.Valores.ValorLiquidoNfse );//Astrogildo em 13/12/12
 
 {
 If FNFSe.Servico.Valores.ValorIssRetido > 0 Then Begin

   rllValorIssRetido.Caption := FormatFloat(FNFSe.Servico.Valores.ValorIssRetido);

   rllValorLiquido.Caption   := FormatFloat(FNFSe.Servico.Valores.ValorLiquidoNfse -
                                                    FNFSe.Servico.Valores.ValorIssRetido);
 End Else Begin

   Case FNFSe.Servico.Valores.IssRetido Of
   stRetencao     : Begin

                      rllValorIssRetido.Caption := FormatFloat(FNFSe.Servico.Valores.ValorIss);

                      rllValorLiquido.Caption   := FormatFloat(FNFSe.Servico.Valores.ValorLiquidoNfse -
                                                                       FNFSe.Servico.Valores.ValorIss);
                    End;

   stNormal       : Begin

                      rllValorIssRetido.Caption := FormatFloat(0);

                      rllValorLiquido.Caption   := FormatFloat(FNFSe.Servico.Valores.ValorLiquidoNfse);
                    End;

   stSubstituicao : Begin

                      rllValorIssRetido.Caption := FormatFloat(0);

                      rllValorLiquido.Caption   := FormatFloat(FNFSe.Servico.Valores.ValorLiquidoNfse);
                    End;
   end;
 End;
 }

 // TnfseNaturezaOperacao = ( noTributacaoNoMunicipio, noTributacaoForaMunicipio, noIsencao, noImune, noSuspensaDecisaoJudicial, noSuspensaProcedimentoAdministrativo )
 case FNFSe.NaturezaOperacao of
  noTributacaoNoMunicipio   : rllNatOperacao.Caption := '1 - Tributação no município';
  noTributacaoForaMunicipio : rllNatOperacao.Caption := '2 - Tributação fora do município';
  noIsencao                 : rllNatOperacao.Caption := '3 - Isenção';
  noImune                   : rllNatOperacao.Caption := '4 - Imune';
  noSuspensaDecisaoJudicial : rllNatOperacao.Caption := '5 - Exigibilidade susp. por decisão judicial';
  noSuspensaProcedimentoAdministrativo : rllNatOperacao.Caption := '6 - Exigibilidade susp. por proced. adm.';
  noSimplesNacional59       : rllNatOperacao.Caption := '7 - Simples Nacional (Dentro Estado)';
  noSimplesNacional69       : rllNatOperacao.Caption := '8 - Simples Nacional (Fora Estado)';
  noTributacaoNoMunicipioSemISS52 : rllNatOperacao.Caption := '9 - Tributacao No Municipio Sem Retenção de ISS';
 end;

 // TnfseRegimeEspecialTributacao = ( retNenhum, retMicroempresaMunicipal, retEstimativa, retSociedadeProfissionais, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP )
 case FNFSe.RegimeEspecialTributacao of
  retNenhum                    : rllRegimeEspecial.Caption := '0 - Nenhum';
  retMicroempresaMunicipal     : rllRegimeEspecial.Caption := '1 - Microempresa municipal';
  retEstimativa                : rllRegimeEspecial.Caption := '2 - Estimativa';
  retSociedadeProfissionais    : rllRegimeEspecial.Caption := '3 - Sociendade de profissionais';
  retCooperativa               : rllRegimeEspecial.Caption := '4 - Cooperativa';
  retMicroempresarioIndividual : rllRegimeEspecial.Caption := '5 - Microempresário Individual (MEI)';
  retMicroempresarioEmpresaPP  : rllRegimeEspecial.Caption := '6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
 end;

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.OptanteSimplesNacional of
  snSim : rllOpcaoSimples.Caption := 'Sim';
  snNao : rllOpcaoSimples.Caption := 'Não';
 end;

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.IncentivadorCultural of
  snSim : rllIncentivador.Caption := 'Sim';
  snNao : rllIncentivador.Caption := 'Não';
 end;

 rllValorServicos2.Caption      := FormatFloat( FNFSe.Servico.Valores.ValorServicos );//Astrogildo em 13/12/12
 rllValorDeducoes.Caption       := FormatFloat( FNFSe.Servico.Valores.ValorDeducoes );//Astrogildo em 13/12/12
 rllDescIncondicionado2.Caption := FormatFloat( FNFSe.Servico.Valores.DescontoIncondicionado );//Astrogildo em 13/12/12
 rllBaseCalc.Caption            := FormatFloat( FNFSe.Servico.Valores.BaseCalculo );//Astrogildo em 13/12/12
 
 // thema precisa ser desta forma pois usa aliquota 2,5 => 0,025
 if (FNFSe.Servico.Valores.Aliquota > 0) and (FNFSe.Servico.Valores.Aliquota < 1) then
   rllAliquota.Caption := FormatFloat( FNFSe.Servico.Valores.Aliquota * 100 )
 else
   rllAliquota.Caption := FormatFloat( FNFSe.Servico.Valores.Aliquota );

 // rllAliquota.Caption            := FormatFloat( FNFSe.Servico.Valores.Aliquota );//Astrogildo em 13/12/12

 // TnfseSimNao = ( snSim, snNao )
 case FNFSe.Servico.Valores.IssRetido of
  stRetencao     : rllISSReter.Caption := 'Sim';//Astrogildo em 13/12/12
  stNormal       : rllISSReter.Caption := 'Não';//Astrogildo em 13/12/12
  stSubstituicao : rllISSReter.Caption := 'ST';//Astrogildo em 13/12/12
 end;

 case FNFSe.OptanteSimplesNacional of
 snSim : rllValorISS.Caption := FormatFloat(0);
 snNao : rllValorISS.Caption := FormatFloat(FNFSe.Servico.Valores.ValorIss);
 end;
// rllValorCredito.Caption := NotaUtil.FormatFloat( FNFSe.ValorCredito );

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

 (*if (FPrestLogo <> '') and FilesExists(FPrestLogo) then
 begin
   rliPrestLogo.Picture.LoadFromFile(FPrestLogo);
 end;*)

 // Alterado por Augusto Fontana - 18/09/2013
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


 rllPrestCNPJ.Caption := FormatarCNPJ( FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj );//Astrogildo em 13/12/12

 If FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual <> '' Then
   rllTomaInscEstadual.Caption := FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual
 Else rllTomaInscEstadual.Caption := FT_InscEstadual;

 If FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal <> '' Then
   rllPrestInscMunicipal.Caption := FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal
 Else rllPrestInscMunicipal.Caption := FInscMunicipal;

 If FNFSe.PrestadorServico.RazaoSocial <> '' Then
   rllPrestNome.Caption := FNFSe.PrestadorServico.RazaoSocial
 Else rllPrestNome.Caption := FRazaoSocial;

 If FNFSe.PrestadorServico.Endereco.Endereco <> '' Then Begin

   rllPrestEndereco.Caption := Trim( FNFSe.PrestadorServico.Endereco.Endereco )+', '+
                               Trim( FNFSe.PrestadorServico.Endereco.Numero )+' - '+
                               Trim( FNFSe.PrestadorServico.Endereco.Bairro )+
                               ' - CEP: '+
                               FormatarCEP( Poem_Zeros( FNFSe.PrestadorServico.Endereco.CEP, 8 ) );//Astrogildo em 13/12/12
 End Else Begin

   rllPrestEndereco.Caption := Trim(FEndereco);
 End;

 If FNFSe.PrestadorServico.Endereco.Complemento <> '' Then
   rllPrestComplemento.Caption := FNFSe.PrestadorServico.Endereco.Complemento
 Else rllPrestComplemento.Caption := FComplemento;

 If FNFSe.PrestadorServico.Contato.Telefone <> '' Then
   rllPrestTelefone.Caption := FormatarFone(FNFSe.PrestadorServico.Contato.Telefone)
 Else rllPrestTelefone.Caption := FormatarFone(FFone);

 If FNFSe.PrestadorServico.Endereco.xMunicipio <> '' Then
   rllPrestMunicipio.Caption := FNFSe.PrestadorServico.Endereco.CodigoMunicipio + ' - ' + FNFSe.PrestadorServico.Endereco.xMunicipio
 Else rllPrestMunicipio.Caption := FMunicipio;

 If FNFSe.PrestadorServico.Endereco.UF <> '' Then
   rllPrestUF.Caption := FNFSe.PrestadorServico.Endereco.UF
 Else rllPrestUF.Caption := FUF;

 If FNFSe.PrestadorServico.Contato.Email <> '' Then
   rllPrestEmail.Caption := FNFSe.PrestadorServico.Contato.Email
 Else rllPrestEmail.Caption := FEMail_Prestador;

 rllPrestNomeEnt.Caption := FRazaoSocial;
 rllNumNF0Ent.Caption    := FormatFloat('00000000000', StrToFloat(FNFSe.Numero));
 rllTomadorNomeEnt.Caption := 'Emissão:' + FormatDateTime('dd/mm/yy',FNFSe.DataEmissao) + '-Tomador:'+FNFSe.Tomador.RazaoSocial+'-Total:' + FormatFloat('##,##0.00',FNFSe.Servico.Valores.ValorLiquidoNfse) ;
end;

procedure TfrlDANFSeRLRetrato.rlbTomadorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;

 if Length(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj)<=11
  then rllTomaCNPJ.Caption := FormatarCPF( FNFSe.Tomador.IdentificacaoTomador.CpfCnpj ) //Astrogildo em 13/12/12
  else rllTomaCNPJ.Caption := FormatarCNPJ( FNFSe.Tomador.IdentificacaoTomador.CpfCnpj );//Astrogildo em 13/12/12

 If FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal <> '' Then
   rllTomaInscMunicipal.Caption := FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal
 Else rllTomaInscMunicipal.Caption := FT_InscMunicipal;

 rllTomaNome.Caption := FNFSe.Tomador.RazaoSocial;

 If FNFSe.Tomador.Endereco.Endereco <> '' Then Begin

   rllTomaEndereco.Caption := Trim(FNFSe.Tomador.Endereco.Endereco) + ', '  +
                              Trim(FNFSe.Tomador.Endereco.Numero )  + ' - ' +
                              Trim(FNFSe.Tomador.Endereco.Bairro )  + ' - CEP: ' +
                              FormatarCEP(Poem_Zeros(FNFSe.Tomador.Endereco.CEP, 8 ));
 End Else Begin

   rllTomaEndereco.Caption := Trim(FT_Endereco) + ' - CEP: ' +
                              FormatarCEP(Poem_Zeros(FNFSe.Tomador.Endereco.CEP, 8 ));
 End;

 If FNFSe.Tomador.Endereco.Complemento <> '' Then
   rllTomaComplemento.Caption := FNFSe.Tomador.Endereco.Complemento
 Else rllTomaComplemento.Caption := FT_Complemento;

 If FNFSe.Tomador.Contato.Telefone <> '' Then
   rllTomaTelefone.Caption := FormatarFone(FNFSe.Tomador.Contato.Telefone)
 Else rllTomaTelefone.Caption := FormatarFone(FT_Fone);

 rllTomaMunicipio.Caption := FNFSe.Tomador.Endereco.CodigoMunicipio + ' - ' + FNFSe.Tomador.Endereco.xMunicipio;
 rllTomaUF.Caption        := FNFSe.Tomador.Endereco.UF;

 If FNFSe.Tomador.Contato.Email <> '' Then
   rllTomaEmail.Caption := FNFSe.Tomador.Contato.Email
 Else rllTomaEmail.Caption := FT_Email;

 // Mensagem para modo Homologacao.
 rllMsgTeste.Visible := False;
 rllMsgTeste.Enabled := False;
 if FNFSe.NfseCancelamento.DataHora<>0
  then begin
   rllMsgTeste.Caption  := 'NFS-e CANCELADA';
   rllMsgTeste.Visible  := True;
   rllMsgTeste.Enabled := True;
  end;
 rllMsgTeste.Repaint;
(*
 if FNFSe.Ide.tpAmb = taHomologacao
  then begin
   rllMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
   rllMsgTeste.Enabled := True;
   rllMsgTeste.Visible := True;
  end
  else begin
   if FNFSe.procNFSe.cStat > 0
    then begin

     if FNFSe.procNFSe.cStat = 102
      then begin
       rllMsgTeste.Caption  := 'NFS-e DENEGADA';
       rllMsgTeste.Visible  := True;
       rllMsgTeste.Enabled := True;
      end;

     if not FNFSe.procNFSe.cStat in [101, 102, 100]
      then begin
       rllMsgTeste.Caption:=  FNFSe.procNFSe.xMotivo;
       rllMsgTeste.Visible := True;
       rllMsgTeste.Enabled := True;
      end;
    end
    else begin
     rllMsgTeste.Caption  := 'NF-E NÃO ENVIADA PARA SEFAZ';
     rllMsgTeste.Visible  := True;
     rllMsgTeste.Enabled  := True;
    end;
  end;

*)

end;

procedure TfrlDANFSeRLRetrato.RLNFSeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  inherited;
  Itens;

  RLNFSe.DataSource := dsItens;
  RLNFSe.Title := 'NFS-e: ' + FNFSe.Numero;

  RLNFSe.Margins.TopMargin    := FMargemSuperior * 10;
  RLNFSe.Margins.BottomMargin := FMargemInferior * 10;
  RLNFSe.Margins.LeftMargin   := FMargemEsquerda * 10;
  RLNFSe.Margins.RightMargin  := FMargemDireita * 10;

end;

end.
{$HINTS ON}
{$WARNINGS ON}
