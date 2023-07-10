unit ACBrMonitorTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, IniFiles,
  {$ifdef FPC}
  LConvEncoding,
  {$endif}
  ACBrTests.Util, CmdUnit, ACBrMonitorTestConsts;

type

{ TACBrCmdTest }

TACBrCmdTest = class(TTestCase)
private
  FCmd: TACBrCmd;
  Comand: TStringList;
  Ts: String;
public
  procedure SetUp;override;
  procedure TearDown; override;
published
  procedure ComandoRecebendoCaminhoIni;
  procedure ComandoRecebendoConteudoIni;
end;

{ TACBrMonitor1Test }

TACBrMonitor1Test = class(TTestCase)
private
public
published
  procedure Processar_NFSeAdicionarRPS_PassandoCaminho;
  procedure Processar_NFSeAdicionarRPS_PassandoConteudo;
end;


implementation

{ TACBrMonitor1Test }

procedure TACBrMonitor1Test.Processar_NFSeAdicionarRPS_PassandoCaminho;
var
  fsProcessar: TStringList;
  Original: TStringList;
  Objeto, Linha: String;
begin
  fsProcessar := TStringList.Create;
  Original := TStringList.Create;
  try
    fsProcessar.Add('NFSE.AdicionarRPS("'+NFSEINI+'", 1)');
    Original.Add('NFSE.AdicionarRPS("'+NFSEINI+'", 1)');

    while fsProcessar.Count > 0 do
    begin
      Objeto := TrimLeft(fsProcessar[0]);

      if (UpperCase(Copy(Objeto, 1, 6)) = 'BOLETO') or
         (UpperCase(Copy(Objeto, 1, 5)) = 'EMAIL')  or
         (UpperCase(Copy(Objeto, 1, 3)) = 'NFE')  or
         (UpperCase(Copy(Objeto, 1, 3)) = 'SAT') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'MDFE') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'GNRE') or
         (UpperCase(Copy(Objeto, 1, 7)) = 'ESOCIAL') or
         (UpperCase(Copy(Objeto, 1, 5)) = 'REINF') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'BPE') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'CTE') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'CNPJ') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'NFSE') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'CPF') then
      begin
        Linha := Trim(fsProcessar.Text);
        if Copy(Linha, 1, 3) = UTF8BOM then
          Linha := copy(Linha, 4, Length(Linha) );

        fsProcessar.Clear;
      end
      else
      begin
        Linha := Objeto;
        fsProcessar.Delete(0);
      end;
    end;

    Check(Linha = Trim(Original.Text),
                'Recebeu:'+ Trim(Original.Text) + sLineBreak +
                '|Objeto:'+ Objeto + sLineBreak +
                '|Linha:'+ Linha);


  finally
    FreeAndNil(fsProcessar);
    FreeAndNil(Original);
  end;
end;

procedure TACBrMonitor1Test.Processar_NFSeAdicionarRPS_PassandoConteudo;
var
  fsProcessar: TStringList;
  ConteudoIni: TStringList;
  Original: TStringList;
  Objeto, Linha: String;
begin
  fsProcessar := TStringList.Create;
  ConteudoIni := TStringList.Create;
  Original := TStringList.Create;
  try
    ConteudoIni.LoadFromFile(NFSEINI);
    fsProcessar.Add('NFSE.AdicionarRPS("'+ConteudoIni.Text+'", 1)');
    Original.Add('NFSE.AdicionarRPS("'+ConteudoIni.Text+'", 1)');
    while fsProcessar.Count > 0 do
    begin
      Objeto := TrimLeft(fsProcessar[0]);

      if (UpperCase(Copy(Objeto, 1, 6)) = 'BOLETO') or
         (UpperCase(Copy(Objeto, 1, 5)) = 'EMAIL')  or
         (UpperCase(Copy(Objeto, 1, 3)) = 'NFE')  or
         (UpperCase(Copy(Objeto, 1, 3)) = 'SAT') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'MDFE') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'GNRE') or
         (UpperCase(Copy(Objeto, 1, 7)) = 'ESOCIAL') or
         (UpperCase(Copy(Objeto, 1, 5)) = 'REINF') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'BPE') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'CTE') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'CNPJ') or
         (UpperCase(Copy(Objeto, 1, 4)) = 'NFSE') or
         (UpperCase(Copy(Objeto, 1, 3)) = 'CPF') then
      begin
        Linha := Trim(fsProcessar.Text);
        if Copy(Linha, 1, 3) = UTF8BOM then
          Linha := copy(Linha, 4, Length(Linha) );

        fsProcessar.Clear;
      end
      else
      begin
        Linha := Objeto;
        fsProcessar.Delete(0);
      end;

    end;

    Check(Linha = Trim(Original.Text),
                'Recebeu:'+ Trim(Original.Text) + sLineBreak +
                '|Objeto:'+ Objeto + sLineBreak +
                '|Linha:'+ Linha);


  finally
    FreeAndNil(fsProcessar);
    FreeAndNil(ConteudoIni);
    FreeAndNil(Original);
  end;
end;



{ TACBrCmdTest }

procedure TACBrCmdTest.SetUp;
begin
  inherited SetUp;
  FCmd := TACBrCmd.Create;
  Comand := TStringList.Create;
end;

procedure TACBrCmdTest.TearDown;
begin
  inherited TearDown;
  FCmd.Free;
  Comand.Free;
end;

procedure TACBrCmdTest.ComandoRecebendoCaminhoIni;
var
  Parametros: TStringList;
begin
  Parametros := TStringList.Create;
  try
    Parametros.Clear;
    Parametros.Add(CTEINI);
    Parametros.Add('1');
    Comand.Text := 'CTe.AdicionarCTe("'+Parametros.Strings[0]+'",'+Parametros.Strings[1]+')';
    FCmd.Comando := Comand.Text;
    Check(FCmd.Params(0) = Parametros.Strings[0], 'Params(0) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[0] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(0));
    Check(FCmd.Params(1) = Parametros.Strings[1], 'Params(1) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[1] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(1));
    Check(FCmd.Metodo = 'adicionarcte', 'Metodo esperado:adicionarcte|Metodo lido:'+FCmd.Metodo);

    Parametros.Clear;
    Parametros.Add(NFSEINI);
    Parametros.Add('1');
    Comand.Text := 'NFSe.AdicionarRPS("'+Parametros.Strings[0]+'",'+Parametros.Strings[1]+')';
    FCmd.Comando := Comand.Text;
    Check(FCmd.Params(0) = Parametros.Strings[0], 'Params(0) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[0] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(0));
    Check(FCmd.Params(1) = Parametros.Strings[1], 'Params(1) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[1] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(1));
    Check(FCmd.Metodo = 'adicionarrps', 'Metodo esperado:adicionarrps|Metodo lido:'+FCmd.Metodo);
  finally
    Parametros.Free;
  end;
end;

procedure TACBrCmdTest.ComandoRecebendoConteudoIni;
var
  Parametros: TStringList;
begin
  Parametros := TStringList.Create;
  try
    Parametros.Clear;
    Parametros.Add('[ide]'
                  +'cCT=75454483'
                  +'CFOP=6932'
                  +'natOp=PRESTACAO SERVICO'
                  +'forPag=1'
                  +'mod=57'
                  +'serie=1'
                  +'nCT=1'
                  +'dhEmi=26/06/2023'
                  +'tpImp=1'
                  +'tpemis=1'
                  +'procEmi=0'
                  +'verProc=3.0'
                  +'dhCont=30/12/1899'
                  +'xJust='
                  +'tpCTe=0'
                  +'refCTe='
                  +'cMunEnv=3554003'
                  +'xMunEnv=TATUI'
                  +'UFEnv=SP'
                  +'modal=01'
                  +'tpServ=0'
                  +'cMunIni=3119401'
                  +'xMunIni=CORONEL FABRICIANO'
                  +'UFIni=MG'
                  +'cMunFim=2900207'
                  +'xMunFim=ABARE'
                  +'UFFim=BA'
                  +'retira=0'
                  +'xDetRetira='
                  +'indGlobalizado=0'
                  +'indIEToma=1'
                  +'cUF=35'
                  +'[toma4]'
                  +'toma=4'
                  +'CNPJCPF=10242141000174'
                  +'IE=0010834420031'
                  +'xNome=ACOUGUE E SUPERMERCADO SOUZA LTDA'
                  +'xFant='
                  +'fone='
                  +'xLgr=RUA BELO HORIZONTE'
                  +'nro=614'
                  +'xCpl=N D'
                  +'xBairro=CALADINA'
                  +'cMun=3119401'
                  +'xMun=CORONEL FABRICIANO'
                  +'CEP=35171167'
                  +'UF=MG'
                  +'cPais=1058'
                  +'xPais=BRASIL'
                  +'email='
                  +'[compl]'
                  +'xCaracAd=Carac Adic'
                  +'xCaracSer=Carac Adicionais do Serviço'
                  +'xEmi=Nome do Emitente'
                  +'tpPer=0'
                  +'tpHor=0'
                  +'origCalc=Sao Paulo'
                  +'destCalc=Campinas'
                  +'xObs=Observação livre'
                  +'[emit]'
                  +'CNPJ=18760540000139'
                  +'IE=030068545752'
                  +'xNome=RAZAO SOCIAL DE TESTE'
                  +'xFant=FANTASIA DE TESTE'
                  +'CRT=3'
                  +'xLgr=Logradouro'
                  +'nro=1'
                  +'xCpl=Complemento'
                  +'xBairro=Bairro'
                  +'cMun=3554003'
                  +'xMun=TATUI'
                  +'CEP=17250000'
                  +'UF=SP'
                  +'fone='
                  +'[rem]'
                  +'CNPJCPF=05481336000137'
                  +'IE=687138770110'
                  +'xNome=Nome do Remetente'
                  +'xFant=Nome Fantasia'
                  +'fone=33445566'
                  +'xLgr=Rua 1'
                  +'nro=200'
                  +'xCpl='
                  +'xBairro=Centro'
                  +'cMun=3554003'
                  +'xMun=Nome do Municipio'
                  +'CEP=14123456'
                  +'UF=SP'
                  +'PaisCod=1058'
                  +'Pais=BRASIL'
                  +'Email='
                  +'[Dest]'
                  +'CNPJCPF=05481336000137'
                  +'IE=687138770110'
                  +'xNome=Nome do Destinatário'
                  +'fone=33445566'
                  +'xLgr=Rua 1'
                  +'nro=200'
                  +'xCpl='
                  +'xBairro=Centro'
                  +'cMun=3554003'
                  +'xMun=Nome do Municipio'
                  +'CEP=14123456'
                  +'UF=SP'
                  +'cPais=1058'
                  +'xPais=BRASIL'
                  +'[Exped]'
                  +'CNPJCPF=05481336000137'
                  +'IE=687138770110'
                  +'xNome=Nome do Destinatário'
                  +'fone=33445566'
                  +'xLgr=Rua 1'
                  +'nro=200'
                  +'xCpl='
                  +'xBairro=Centro'
                  +'cMun=3554003'
                  +'xMun=Nome do Municipio'
                  +'CEP=14123456'
                  +'UF=SP'
                  +'cPais=1058'
                  +'xPais=BRASIL'
                  +'[Receb]'
                  +'CNPJCPF=05481336000137'
                  +'IE=687138770110'
                  +'xNome=Nome do Destinatário'
                  +'fone=33445566'
                  +'xLgr=Rua 1'
                  +'nro=200'
                  +'xCpl='
                  +'xBairro=Centro'
                  +'cMun=3554003'
                  +'xMun=Nome do Municipio'
                  +'CEP=14123456'
                  +'UF=SP'
                  +'cPais=1058'
                  +'xPais=BRASIL'
                  +'[vPrest]'
                  +'vTPrest=100'
                  +'vRec=100'
                  +'[Imp]'
                  +'vTotTrib=17'
                  +'infAdFisco=Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT'
                  +'[Comp001]'
                  +'xNome=DFRNER KRTJ'
                  +'vComp=100'
                  +'[ICMS00]'
                  +'CST=00'
                  +'vBC=0'
                  +'pICMS=0'
                  +'vICMS=0'
                  +'[ICMS90]'
                  +'CST=90'
                  +'pRedBC=10'
                  +'vBC=100'
                  +'pICMS=7'
                  +'vICMS=7'
                  +'vCred=0'
                  +'[ICMSUFFim]'
                  +'vBCUFFim=0'
                  +'pFCPUFFim=0'
                  +'pICMSUFFim=0'
                  +'pICMSInter=0'
                  +'pICMSInterPart=0'
                  +'vFCPUFFim=0'
                  +'vICMSUFFim=0'
                  +'vICMSUFIni=0'
                  +'[infCarga]'
                  +'vCarga=5000'
                  +'proPred=Produto Predominante'
                  +'xOutCat=Pacotes'
                  +'vCargaAverb=5000'
                  +'[infQ001]'
                  +'cUnid=01'
                  +'tpMed=Kg'
                  +'qCarga=10'
                  +'[infQ002]'
                  +'cUnid=03'
                  +'tpMed=Caixa'
                  +'qCarga=5'
                  +'[infQ003]'
                  +'cUnid=00'
                  +'tpMed=Volume'
                  +'qCarga=10'
                  +'[infQ004]'
                  +'cUnid=02'
                  +'tpMed=Toneladas'
                  +'qCarga=1'
                  +'[infQ005]'
                  +'cUnid=04'
                  +'tpMed=Litros'
                  +'qCarga=10'
                  +'[infQ006]'
                  +'cUnid=04'
                  +'tpMed=Litros2'
                  +'qCarga=10'
                  +'[infQ007]'
                  +'cUnid=04'
                  +'tpMed=Litros3'
                  +'qCarga=10'
                  +'[infQ008]'
                  +'cUnid=04'
                  +'tpMed=Litros4'
                  +'qCarga=10'
                  +'[infQ009]'
                  +'cUnid=04'
                  +'tpMed=Litros5'
                  +'qCarga=10'
                  +'[infNFe001]'
                  +'chave=42210117089484000190550110000091001371413248'
                  +'PIN='
                  +'[Rodo]'
                  +'RNTRC=12345678'
                  +'dPrev=30/12/1899'
                  +'lota=0'
                  +'[cobr]'
                  +'nFat=123'
                  +'vOrig=100'
                  +'vDesc=0'
                  +'vLiq=100'
                  +'[dup001]'
                  +'nDup=123'
                  +'dVenc=26/07/2023'
                  +'vLiq=100'
                  +'[infRespTec]'
                  +'CNPJ='
                  +'xContato='
                  +'email='
                  +'fone='
                  +'[procCTe]'
                  +'tpAmb=2'
                  +'verAplic='
                  +'chCTe='
                  +'dhRecbto=30/12/1899'
                  +'nProt='
                  +'digVal='
                  +'cStat=0'
                  +'xMotivo=');
    Parametros.Add('1');
    Comand.Text := 'CTe.AdicionarCTe("'+ Parametros.Strings[0] + '",' + Parametros.Strings[1]+')';
    FCmd.Comando := Comand.Text;
    Ts := Comand.Text;
    Check(FCmd.Params(0) = Parametros.Strings[0], 'Params(0) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[0] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(0));
    Check(FCmd.Params(1) = Parametros.Strings[1], 'Params(1) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[1] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(1));
    Check(FCmd.Metodo='adicionarcte', 'Metodo esperado:adicionarcte|Metodo lido:'+FCmd.Metodo);

    Parametros.Clear;
    Parametros.Add('[IdentificacaoNFSe]'
                  +'Numero=1'
                  +'[IdentificacaoRps]'
                  +'SituacaoTrib=tp'
                  +'Producao=2'
                  +'Status=1'
                  +'OutrasInformacoes=Pagamento a Vista'
                  +'SeriePrestacao=1'
                  +'Numero=1'
                  +'Serie=85'
                  +'Tipo=1'
                  +'DataEmissao=15/05/2023'
                  +'Competencia=15/05/2023'
                  +'NaturezaOperacao=2'
                  +'PercentualCargaTributaria=0'
                  +'ValorCargaTributaria=0'
                  +'PercentualCargaTributariaMunicipal=0'
                  +'ValorCargaTributariaMunicipal=0'
                  +'PercentualCargaTributariaEstadual=0'
                  +'ValorCargaTributariaEstadual=0'
                  +'[RpsSubstituido]'
                  +'Numero='
                  +'Serie='
                  +'Tipo=1'
                  +'[Prestador]'
                  +'Regime=1'
                  +'OptanteSN=1'
                  +'IncentivadorCultural=2'
                  +'CNPJ=11111111111111'
                  +'InscricaoMunicipal=12345678'
                  +'RazaoSocial=RAZAO SOCIAL'
                  +'NomeFantasia=RAZAO SOCIAL'
                  +'Logradouro=LOGRADOURO'
                  +'Numero=1'
                  +'Bairro=BAIRRO'
                  +'CodigoMunicipio=3554003'
                  +'UF=SP'
                  +'CodigoPais=1058'
                  +'xPais=BRASIL'
                  +'CEP=00000000'
                  +'Telefone=1633224455'
                  +'Email=nome@provedor.com.br'
                  +'[Tomador]'
                  +'Tipo=2'
                  +'CNPJCPF=12345678901'
                  +'InscricaoMunicipal=12345678'
                  +'InscricaoEstadual=12345678'
                  +'RazaoSocial=INSCRICAO DE TESTE E TESTE'
                  +'TipoLogradouro=RUA'
                  +'Logradouro=RUA PRINCIPAL'
                  +'Numero=100'
                  +'Complemento=APTO 11'
                  +'Bairro=CENTRO'
                  +'CodigoMunicipio=3554003'
                  +'UF=SP'
                  +'CodigoPais=1058'
                  +'CEP=14800000'
                  +'xPais=BRASIL'
                  +'Telefone=1622223333'
                  +'Email=nome@provedor.com.br'
                  +'AtualizaTomador=2'
                  +'TomadorExterior=2'
                  +'[Intermediario]'
                  +'CNPJCPF='
                  +'InscricaoMunicipal='
                  +'RazaoSocial='
                  +'[ConstrucaoCivil]'
                  +'CodigoObra='
                  +'Art='
                  +'[Servico]'
                  +'ItemListaServico=09.01'
                  +'CodigoCnae=6203100'
                  +'CodigoTributacaoMunicipio=17.19'
                  +'Discriminacao=discriminacao I; discriminacao II'
                  +'CodigoMunicipio=3554003'
                  +'CodigoPais=1058'
                  +'ExigibilidadeISS=1'
                  +'MunicipioIncidencia=3554003'
                  +'UFPrestacao=SP'
                  +'ResponsavelRetencao=1'
                  +'[Valores]'
                  +'ValorServicos=10,35'
                  +'ValorDeducoes=0'
                  +'ValorPis=0'
                  +'AliquotaPis=0'
                  +'ValorCofins=0,21'
                  +'AliquotaCofins=2'
                  +'ValorInss=0'
                  +'ValorIr=0'
                  +'ValorCsll=0'
                  +'ISSRetido=1'
                  +'OutrasRetencoes=0'
                  +'DescontoIncondicionado=0'
                  +'DescontoCondicionado=0'
                  +'BaseCalculo=10,35'
                  +'Aliquota=2'
                  +'ValorIss=0,21'
                  +'ValorIssRetido=0'
                  +'ValorLiquidoNfse=10,14'
                  +'[CondicaoPagamento]'
                  +'QtdParcela=0'
                  +'Condicao=A_VISTA');
    Parametros.Add('1');
    Comand.Text := 'NFSe.AdicionarRPS("'+Parametros.Strings[0] + '",' + Parametros.Strings[1]+')';
    FCmd.Comando := Comand.Text;
    Ts := Comand.Text;
    Check(FCmd.Params(0) = Parametros.Strings[0], 'Params(0) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[0] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(0));
    Check(FCmd.Params(1) = Parametros.Strings[1], 'Params(1) diferente do esperado!' +
                           sLineBreak + 'Esperado:' +
                           sLineBreak + Parametros.Strings[1] +
                           sLineBreak + 'Recebido:' +
                           sLineBreak + FCmd.Params(1));
    Check(FCmd.Metodo = 'adicionarrps', 'Metodo esperado:adicionarrps|Metodo lido:'+FCmd.Metodo);


  finally
    Parametros.Free;
  end;
end;

initialization
  _RegisterTest('ACBrMonitor.CmdUnit', TACBrCmdTest);
  _RegisterTest('ACBrMonitor.ACBrMonitor1Unit', TACBrMonitor1Test);

end.
