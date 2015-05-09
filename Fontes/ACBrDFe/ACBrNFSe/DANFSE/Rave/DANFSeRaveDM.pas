unit DANFSeRaveDM;

interface

uses
  SysUtils, Classes, RpCon, RpRender, RpRenderPDF, RpRave, RpDefine, RpBase,
  RpSystem, ACBrNFSe, ACBrNFSeUtil, ACBrUtil,  pnfsConversao, RpConDS, pnfsNFSe,
  ACBrDFeUtil, IniFiles, ACBrNFSeDANFSeClass;

type
  TDANFSeDM = class(TDataModule)
    RvSystem1: TRvSystem;
    RvProject: TRvProject;
    RvRenderPDF1: TRvRenderPDF;
    DadosNFSE: TRvCustomConnection;
    DadosPrestador: TRvCustomConnection;
    DadosServico: TRvCustomConnection;
    DadosTomador: TRvCustomConnection;
    DadosPrefeitura: TRvCustomConnection;
    NFSeCancelada: TRvCustomConnection;
    procedure DadosNFSEOpen(Connection: TRvCustomConnection);
    procedure DadosNFSEGetCols(Connection: TRvCustomConnection);
    procedure DadosNFSEGetRow(Connection: TRvCustomConnection);
    procedure DadosPrestadorOpen(Connection: TRvCustomConnection);
    procedure DadosPrestadorGetCols(Connection: TRvCustomConnection);
    procedure DadosPrestadorGetRow(Connection: TRvCustomConnection);
    procedure DadosServicoOpen(Connection: TRvCustomConnection);
    procedure DadosServicoGetCols(Connection: TRvCustomConnection);
    procedure DadosServicoGetRow(Connection: TRvCustomConnection);
    procedure DadosTomadorOpen(Connection: TRvCustomConnection);
    procedure DadosTomadorGetCols(Connection: TRvCustomConnection);
    procedure DadosTomadorGetRow(Connection: TRvCustomConnection);
    procedure DadosPrefeituraOpen(Connection: TRvCustomConnection);
    procedure DadosPrefeituraGetCols(Connection: TRvCustomConnection);
    procedure DadosPrefeituraGetRow(Connection: TRvCustomConnection);
    procedure NFSeCanceladaOpen(Connection: TRvCustomConnection);
    procedure NFSeCanceladaGetCols(Connection: TRvCustomConnection);
    procedure NFSeCanceladaGetRow(Connection: TRvCustomConnection);
    constructor Create(AOwner: TComponent); override; //alterado Andre(Prodez) - 10/03/15
  private
    { Private declarations }
    FNFSE    : TNFSe ;
    FPref01  : String ;
    FDatHr   : String ;
    FImagem  : String ;
    FSistema : String ;
    FOutras  : String ;
    FLogoPr  : String ;
    FDANFSeClassOwner: TACBrNFSeDANFSeClass; //alterado Andre(Prodez) - 10/03/15
  public
    { Public declarations }
    property NFSe   : TNFSe  read FNFSe    write FNFSe;
    property nPref01: string read FPref01  write FPref01 ;
    property nDatHor: string read FDatHr   write FDatHr ;
    property nImagem: string read FImagem  write FImagem ;
    property nOutras: string read FOutras  write FOutras ;
    property Sistema: string read FSistema write FSistema ;
    property nLogoPr: string read FLogoPr  write FLogoPr ;
    property DANFSeClassOwner: TACBrNFSeDANFSeClass read FDANFSeClassOwner; //alterado Andre(Prodez) - 10/03/15
  end;

implementation

{$R *.dfm}

constructor TDANFSeDM.Create(AOwner: TComponent);
begin
  inherited;
  FDANFSeClassOwner := TACBrNFSeDANFSeClass(AOwner); //alterado Andre(Prodez) - 10/03/15
end;


procedure TDANFSeDM.DadosNFSEGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('Numero'                  , dtString  , 15, '', '');
  Connection.WriteField('DataEmissao'             , dtDateTime, 20, '', '');
  Connection.WriteField('NaturezaOperacao'        , dtString  , 80, '', '');
  Connection.WriteField('RegimeEspecialTributacao', dtString  , 80, '', '');
  Connection.WriteField('OptanteSimplesNacional'  , dtString  , 10, '', '');
  Connection.WriteField('IncentivadorCultural'    , dtString  , 10, '', '');
  Connection.WriteField('CodigoVerificacao'       , dtString  , 20, '', '');
  Connection.WriteField('Competencia'             , dtString  , 10, '', '');
  Connection.WriteField('NFSeSubstituida'         , dtString  , 15, '', '');
  Connection.WriteField('OutrasInformacoes'       , dtString  ,500, '', '');
  Connection.WriteField('ValorCrediro'            , dtCurrency, 15, '', '');
  Connection.WriteField('CodigoObra'              , dtString  , 15, '', '');
  Connection.WriteField('NumeroArt'               , dtString  , 20, '', '');
  Connection.WriteField('NumeroRps'               , dtString  , 15, '', '');
  Connection.WriteField('Serie'                   , dtString  ,  5, '', '');
  Connection.WriteField('LocalPrestacao'          , dtString  , 34, '', '');
end;

procedure TDANFSeDM.DadosNFSEGetRow(Connection: TRvCustomConnection);
begin
  // Dados da Nota Fiscal de Serviço

  with NFSe do
  begin

    Connection.WriteStrData('', Numero);
    Connection.WriteDateTimeData('', DataEmissao);
    case NaturezaOperacao of
      noTributacaoNoMunicipio   : Connection.WriteStrData('', '1 - Tributação no Município');
      noTributacaoForaMunicipio : Connection.WriteStrData('', '2 - Tributação fora do Município');
      noIsencao                 : Connection.WriteStrData('', '3 - Isenção');
      noImune                   : Connection.WriteStrData('', '4 - Imune');
      noSuspensaDecisaoJudicial : Connection.WriteStrData('', '5 - Suspensa por Decisão Judicial');
      noSuspensaProcedimentoAdministrativo : Connection.WriteStrData('', '6 - Suspensa por Procedimento Administrativo');
    end;

    case RegimeEspecialTributacao of
      retNenhum                   : Connection.WriteStrData('', '0 - NENHUM');
      retMicroempresaMunicipal    : Connection.WriteStrData('', '1 - MICROEMPRESA MUNICIPAL');
      retEstimativa               : Connection.WriteStrData('', '2 - ESTIMATIVA');
      retSociedadeProfissionais   : Connection.WriteStrData('', '3 - SOCIEDADE PROFICIONÁIS');
      retCooperativa              : Connection.WriteStrData('', '4 - COOPERATIVA');
      retMicroempresarioIndividual: Connection.WriteStrData('', '5 - MICROEMPRESÁRIO INDIVIDUAL');
      retMicroempresarioEmpresaPP : Connection.WriteStrData('', '6 - MICROEMPRESÁRIO (EPP)');
    end;

    case OptanteSimplesNacional of
      snSim: Connection.WriteStrData('', '1-Sim');
      snNao: Connection.WriteStrData('', '2-Não');
    end;

    case IncentivadorCultural of
      snSim: Connection.WriteStrData('', '1-Sim');
      snNao: Connection.WriteStrData('', '2-Não');
    end;
    if CodigoVerificacao = EmptyStr then
      nOutras := 'NOTA FISCAL SEM AUTORIZAÇÂO DE USO' ;

    Connection.WriteStrData('', CodigoVerificacao);
    Connection.WriteStrData('', Copy(Competencia,6,2)+'/'+Copy(Competencia,1,4));
    Connection.WriteStrData('', NfseSubstituida);
    Connection.WriteStrData('', OutrasInformacoes);
    Connection.WriteCurrData('', ValorCredito);
    Connection.WriteStrData('', ConstrucaoCivil.CodigoObra);
    Connection.WriteStrData('', ConstrucaoCivil.Art);
    Connection.WriteStrData('', IdentificacaoRps.Numero);
    Connection.WriteStrData('', IdentificacaoRps.Serie);
    if trim(NFSe.Servico.CodigoMunicipio) <> '' then
      Connection.WriteStrData('', CodCidadeToCidade(StrToInt(NFSe.Servico.CodigoMunicipio)))
    else
      Connection.WriteStrData('', NFSe.Tomador.Endereco.xMunicipio);
  end;
end;

procedure TDANFSeDM.DadosNFSEOpen(Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1 ;
  if not DANFSeClassOwner.ImprimeCanhoto then  //alterado Andre(Prodez) - 10/03/15
    RvProject.SetParam('ImprimeCanhoto', 'N');
  {
  para que funcione o tratamento da propriedade 'ImprimeCanhoto' do componente ACBrNFSeDANFSeRV:
    - criar o parametro "ImprimeCanhoto" no projeto do arquivo "DANFENFSE.rav" do Rave
      (na treeview clique em Reports; e no inspector va em Parameters)
    - no evento "OnBeforePrint" do band "bRecibo" adicionar o codigo:
        If DANFSE.GetParam('ImprimeCanhoto') = 'N' Then
         Self.Visible := False;
        Else
          Self.Visible := True;
        End If;
  }
end;

procedure TDANFSeDM.DadosPrefeituraGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('Texto'     , dtString, 260, '', '');
  Connection.WriteField('Imagem'    , dtString,  80, '', '');
  Connection.WriteField('LogoPrest' , dtString,  80, '', '');
  Connection.WriteField('DataHora'  , dtString,  20, '', '');
  Connection.WriteField('Sistema'   , dtString,  80, '', '');
  Connection.WriteField('OutrasInf' , dtString,  80, '', '');
end;

procedure TDANFSeDM.DadosPrefeituraGetRow(Connection: TRvCustomConnection);
begin
  Connection.WriteStrData('', nPref01);
  Connection.WriteStrData('', nImagem);
  Connection.WriteStrData('', nLogoPr);
  Connection.WriteStrData('', nDatHor);
  Connection.WriteStrData('', Sistema);
  Connection.WriteStrData('', nOutras);
end;

procedure TDANFSeDM.DadosPrefeituraOpen(Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1 ;
end;

procedure TDANFSeDM.DadosPrestadorGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('RAZAO'      , dtString , 50, '', '');
  Connection.WriteField('FANTASIA'   , dtString , 40, '', '');
  Connection.WriteField('ENDERECO'   , dtString , 50, '', '');
  Connection.WriteField('NUMERO'     , dtString , 15, '', '');
  Connection.WriteField('BAIRRO'     , dtString , 30, '', '');
  Connection.WriteField('CIDADE'     , dtString , 30, '', '');
  Connection.WriteField('CIDADE_IBGE', dtString , 10, '', '');
  Connection.WriteField('CEP'        , dtString , 10, '', '');
  Connection.WriteField('UF'         , dtString ,  2, '', '');
  Connection.WriteField('FONE'       , dtString , 26, '', '');
  Connection.WriteField('CNPJ'       , dtString , 20, '', '');
  Connection.WriteField('INSCMUNI'   , dtString , 20, '', '');
  Connection.WriteField('EMAIL'      , dtString,  80, '', '');
  Connection.WriteField('Complemento', dtString , 40, '', '');
end;

procedure TDANFSeDM.DadosPrestadorGetRow(Connection: TRvCustomConnection);
begin
  //  Dados do Prestador de Serviços
  with NFSe.PrestadorServico do
  begin
    Connection.WriteStrData('', RazaoSocial);
    Connection.WriteStrData('', NomeFantasia);
    with Endereco do
    begin
      Connection.WriteStrData('', Endereco);
      Connection.WriteStrData('', Numero);
      Connection.WriteStrData('', Bairro);
      Connection.WriteStrData('', xMunicipio);
      Connection.WriteStrData('', CodigoMunicipio);
      Connection.WriteStrData('', DfeUtil.FormatarCEP(CEP));
      Connection.WriteStrData('', Uf);
    end;
    Connection.WriteStrData('', DFEUtil.FormatarFone(Contato.Telefone));
    Connection.WriteStrData('', DfeUtil.FormatarCNPJ(IdentificacaoPrestador.Cnpj)) ;
    Connection.WriteStrData('', IdentificacaoPrestador.InscricaoMunicipal);
    Connection.WriteStrData('', Contato.Email);
    Connection.WriteStrData('', Endereco.Complemento);
  end;
end;

procedure TDANFSeDM.DadosPrestadorOpen(Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1 ;
end;

procedure TDANFSeDM.DadosServicoGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('ItemListaServico'         , dtString  ,    5, '', '');
  Connection.WriteField('CodigoTributacaoMunicipio', dtString  ,   20, '', '');
  Connection.WriteField('Discriminacao'            , dtString  , 4000, '', '');
  Connection.WriteField('ValorServicos'            , dtCurrency,   15, '', '');
  Connection.WriteField('ValorDeducoes'            , dtCurrency,   15, '', '');
  Connection.WriteField('ValorPis'                 , dtCurrency,   15, '', '');
  Connection.WriteField('ValorCofins'              , dtCurrency,   15, '', '');
  Connection.WriteField('ValorIr'                  , dtCurrency,   15, '', '');
  Connection.WriteField('ValorInss'                , dtCurrency,   15, '', '');
  Connection.WriteField('ValorCsll'                , dtCurrency,   15, '', '');
  Connection.WriteField('ValorIss'                 , dtCurrency,   15, '', '');
  Connection.WriteField('OutrasDeducoes'           , dtCurrency,   15, '', '');
  Connection.WriteField('BaseCalculo'              , dtCurrency,   15, '', '');
  Connection.WriteField('Aliquota'                 , dtCurrency,    9, '', '');
  Connection.WriteField('ValorLiquidoNfse'         , dtCurrency,   15, '', '');
  Connection.WriteField('ValorIssRetido'           , dtCurrency,   15, '', '');
  Connection.WriteField('DescontoCondicionado'     , dtCurrency,   15, '', '');
  Connection.WriteField('DescontoIncondicionado'   , dtCurrency,   15, '', '');
  Connection.WriteField('RetencoesFederais'        , dtCurrency,   15, '', '');
  Connection.WriteField('IssRetido'                , dtString  ,   10, '', '');
  Connection.WriteField('CodigoMunicipio'          , dtString ,    10, '', '');
  Connection.WriteField('CodigoCnae'               , dtString ,    10, '', '');
  Connection.WriteField('DescricaoServico'         , dtString ,   192, '', '');
end;

procedure TDANFSeDM.DadosServicoGetRow(Connection: TRvCustomConnection);
begin
  // Dados da Nota Fiscal de Serviço
  with NFSe do
  begin
    with Servico do
    begin
      Connection.WriteStrData('', ItemListaServico);
      Connection.WriteStrData('', CodigoTributacaoMunicipio);
      //Connection.WriteStrData('', UpperCase(Discriminacao) ) ;
      Connection.WriteStrData('', StringReplace(Discriminacao, //alterado Andre(Prodez) - 10/03/15
                                                TACBrNFSe(DANFSeClassOwner.ACBrNFSe).Configuracoes.WebServices.QuebradeLinha, #13, [rfReplaceAll, rfIgnoreCase]));
      with Valores do
      begin
        Connection.WriteCurrData('', ValorServicos);
        Connection.WriteCurrData('', ValorDeducoes);
        Connection.WriteCurrData('', ValorPis);
        Connection.WriteCurrData('', ValorCofins);
        Connection.WriteCurrData('', ValorIr);
        Connection.WriteCurrData('', ValorInss);
        Connection.WriteCurrData('', ValorCsll);
        Connection.WriteCurrData('', ValorIss);
        Connection.WriteCurrData('', OutrasRetencoes);
        Connection.WriteCurrData('', BaseCalculo);
        if FormatFloat('00',Aliquota) = '00' then
          Connection.WriteCurrData('', Aliquota * 100)
        else
          Connection.WriteCurrData('', Aliquota) ;
        Connection.WriteCurrData('', ValorLiquidoNfse);
        Connection.WriteCurrData('', ValorIssRetido);
        Connection.WriteCurrData('', DescontoCondicionado);
        Connection.WriteCurrData('', DescontoInCondicionado);
        Connection.WriteCurrData('', ValorPis+ValorCofins+ValorInss+ValorIr+ValorCsll);
        case IssRetido of
          stRetencao: Connection.WriteStrData('', '1-Sim') ;
          stNormal  : Connection.WriteStrData('', '2-Não') ;
        end;
      end;
      Connection.WriteStrData('', CodigoMunicipio);
      Connection.WriteStrData('', CodigoCnae) ;
      Connection.WriteStrData('', Copy(ItemListaServico+' - '+trim(CodigoTributacaoMunicipio)+' '+
                                  UpperCase(xItemListaServico),1,192) ) ;
    end;
  end;
end;

procedure TDANFSeDM.DadosServicoOpen(Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1 ;
end;

procedure TDANFSeDM.DadosTomadorGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('Razao'             , dtString , 50, '', '');
  Connection.WriteField('CNPJCPF'           , dtString , 20, '', '');
  Connection.WriteField('Fantasia'          , dtString , 40, '', '');
  Connection.WriteField('Endereco'          , dtString , 50, '', '');
  Connection.WriteField('Numero'            , dtString , 15, '', '');
  Connection.WriteField('Complemento'       , dtString , 40, '', '');
  Connection.WriteField('Bairro'            , dtString , 30, '', '');
  Connection.WriteField('Cidade'            , dtString , 30, '', '');
  Connection.WriteField('UF'                , dtString ,  2, '', '');
  Connection.WriteField('CEP'               , dtString , 10, '', '');
  Connection.WriteField('Telefone'          , dtString , 20, '', '');
  Connection.WriteField('InscricaoMunicipal', dtString , 10, '', '');
  Connection.WriteField('cMunicipio'        , dtString , 10, '', '');
  Connection.WriteField('Email'             , dtString , 80, '', '');
end;

procedure TDANFSeDM.DadosTomadorGetRow(Connection: TRvCustomConnection);
begin
  // Dados do Tomador do Serviço
  with NFSe.Tomador do
  begin
    Connection.WriteStrData('', RazaoSocial);
    if Length(IdentificacaoTomador.CPFCNPJ) > 11 then
      Connection.WriteStrData('', DfeUtil.FormatarCNPJ(IdentificacaoTomador.CPFCNPJ))
    else
      Connection.WriteStrData('', DfeUtil.FormatarCPF(IdentificacaoTomador.CPFCNPJ)) ;
    Connection.WriteStrData('', '');
    with Endereco do
    begin
      Connection.WriteStrData('', Endereco);
      Connection.WriteStrData('', Numero);
      Connection.WriteStrData('', Complemento);
      Connection.WriteStrData('', Bairro);
      Connection.WriteStrData('', xMunicipio);
      Connection.WriteStrData('', Uf);
      Connection.WriteStrData('', DfeUtil.FormatarCEP(CEP));
    end;
    Connection.WriteStrData('', DfeUtil.FormatarFone(Contato.Telefone));
    Connection.WriteStrData('', IdentificacaoTomador.InscricaoMunicipal);
    Connection.WriteStrData('', Endereco.CodigoMunicipio);
    Connection.WriteStrData('', Contato.Email);
  end;
end;

procedure TDANFSeDM.DadosTomadorOpen(Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1 ;
end;

procedure TDANFSeDM.NFSeCanceladaGetCols(Connection: TRvCustomConnection);
begin
  Connection.WriteField('DataHora', dtDateTime, 20, '', '');
  Connection.WriteField('Motivo'  , dtString  , 80, '', '');
end;

procedure TDANFSeDM.NFSeCanceladaGetRow(Connection: TRvCustomConnection);
begin
  if NFSe.NfseCancelamento.DataHora > 0 then
  begin
    Connection.WriteDateTimeData('', NFSe.NfseCancelamento.DataHora);
    Connection.WriteStrData     ('', '');
  end;
end;

procedure TDANFSeDM.NFSeCanceladaOpen(Connection: TRvCustomConnection);
begin
  if NFSe.NfseCancelamento.DataHora > 0 then
    Connection.DataRows := 1
  else
    Connection.DataRows := 0
end;

end.
