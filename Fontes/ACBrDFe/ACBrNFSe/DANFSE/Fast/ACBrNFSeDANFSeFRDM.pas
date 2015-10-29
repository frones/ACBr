unit ACBrNFSeDANFSeFRDM;

interface

uses
  SysUtils, Classes, frxClass, frxExportPDF, DB, DBClient, frxDBSet,
  ACBrNFSeDANFSeClass, pnfsNFSe, pnfsConversao, frxBarcode,
  ACBrUtil, ACBrValidador;

type
  TdmACBrNFSeFR = class(TDataModule)
    frxReport: TfrxReport;
    frxPDFExport: TfrxPDFExport;
    cdsIdentificacao: TClientDataSet;
    cdsPrestador: TClientDataSet;
    cdsServicos: TClientDataSet;
    frxIdentificacao: TfrxDBDataset;
    frxPrestador: TfrxDBDataset;
    frxTomador: TfrxDBDataset;
    frxServicos: TfrxDBDataset;
    cdsParametros: TClientDataSet;
    frxParametros: TfrxDBDataset;
    cdsIdentificacaoid: TStringField;
    cdsIdentificacaoNumero: TStringField;
    cdsIdentificacaoSerie: TStringField;
    cdsIdentificacaoTipo: TStringField;
    cdsIdentificacaoCompetencia: TStringField;
    cdsIdentificacaoNFSeSubstituida: TStringField;
    cdsIdentificacaoDataEmissao: TStringField;
    cdsIdentificacaoCodigoVerificacao: TStringField;
    cdsServicosItemListaServico: TStringField;
    cdsServicosCodigoCnae: TStringField;
    cdsServicosCodigoTributacaoMunicipio: TStringField;
    cdsServicosDiscriminacao: TStringField;
    cdsServicosCodigoPais: TStringField;
    cdsServicosNumeroProcesso: TStringField;
    cdsServicosxItemListaServico: TStringField;
    cdsServicosResponsavelRetencao: TStringField;
    cdsServicosDescricao: TStringField;
    cdsServicosValorServicos: TCurrencyField;
    cdsServicosValorDeducoes: TCurrencyField;
    cdsServicosValorPis: TCurrencyField;
    cdsServicosValorCofins: TCurrencyField;
    cdsServicosValorInss: TCurrencyField;
    cdsServicosValorIr: TCurrencyField;
    cdsServicosValorCsll: TCurrencyField;
    cdsServicosValorIss: TCurrencyField;
    cdsServicosOutrasRetencoes: TCurrencyField;
    cdsServicosBaseCalculo: TCurrencyField;
    cdsServicosAliquota: TCurrencyField;
    cdsServicosIssRetido: TStringField;
    cdsServicosValorLiquidoNfse: TCurrencyField;
    cdsServicosValorIssRetido: TCurrencyField;
    cdsServicosDescontoCondicionado: TCurrencyField;
    cdsServicosDescontoIncondicionado: TCurrencyField;
    cdsPrestadorCnpj: TStringField;
    cdsPrestadorInscricaoMunicipal: TStringField;
    cdsPrestadorRazaoSocial: TStringField;
    cdsPrestadorNomeFantasia: TStringField;
    cdsPrestadorEndereco: TStringField;
    cdsPrestadorNumero: TStringField;
    cdsPrestadorComplemento: TStringField;
    cdsPrestadorBairro: TStringField;
    cdsPrestadorCodigoMunicipio: TStringField;
    cdsPrestadorUF: TStringField;
    cdsPrestadorCEP: TStringField;
    cdsPrestadorxMunicipio: TStringField;
    cdsPrestadorCodigoPais: TStringField;
    cdsPrestadorTelefone: TStringField;
    cdsPrestadorEmail: TStringField;
    cdsTomador: TClientDataSet;
    StringField2: TStringField;
    StringField3: TStringField;
    StringField4: TStringField;
    StringField5: TStringField;
    StringField6: TStringField;
    StringField7: TStringField;
    StringField8: TStringField;
    StringField9: TStringField;
    StringField10: TStringField;
    StringField11: TStringField;
    StringField12: TStringField;
    StringField13: TStringField;
    StringField14: TStringField;
    StringField15: TStringField;
    cdsTomadorCpfCnpj: TStringField;
    cdsIdentificacaoNumeroNFSe: TStringField;
    cdsParametrosExigibilidadeISS: TStringField;
    cdsParametrosCodigoMunicipio: TStringField;
    cdsParametrosMunicipioIncidencia: TStringField;
    cdsParametrosLogoExpandido: TStringField;
    cdsParametrosImagem: TStringField;
    cdsParametrosLogoCarregado: TBlobField;
    cdsServicosTotalNota: TFloatField;
    cdsServicosTotalServicos: TFloatField;
    cdsParametrosimgPrefeitura: TStringField;
    cdsParametrosLogoPrefExpandido: TStringField;
    cdsParametrosLogoPrefCarregado: TBlobField;
    cdsParametrosNome_Prefeitura: TStringField;
    cdsParametrosMensagem0: TStringField;
    cdsParametrosSistema: TStringField;
    cdsItensServico: TClientDataSet;
    frxItensServico: TfrxDBDataset;
    cdsItensServicoDiscriminacaoServico: TStringField;
    cdsItensServicoQuantidade: TFloatField;
    cdsItensServicoValorUnitario: TFloatField;
    cdsItensServicoValorTotal: TFloatField;
    cdsItensServicoTributavel: TStringField;
    cdsParametrosUsuario: TStringField;
    cdsParametrosNaturezaOperacao: TStringField;
    cdsParametrosRegimeEspecialTributacao: TStringField;
    cdsParametrosOptanteSimplesNacional: TStringField;
    cdsParametrosIncentivadorCultural: TStringField;
    constructor Create(AOwner: TComponent); override;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
  private
    FDANFSeClassOwner: TACBrNFSeDANFSeClass;
    FNFSe: TNFSe;
    procedure CarregaIdentificacao;
    procedure CarregaPrestador;
    procedure CarregaTomador;
    procedure CarregaServicos;
    procedure CarregaItensServico;
    procedure CarregaParametros;
    procedure CarregaaLogoPrefeitura;
    procedure CarregaaImagemPrestadora;
    function ManterDocumento(sCpfCnpj: String): String;
  public
    property NFSe: TNFSe read FNFSe write FNFSe;
    property DANFSeClassOwner: TACBrNFSeDANFSeClass read FDANFSeClassOwner;
    procedure CarregaDados;
  end;

var
  dmACBrNFSeFR: TdmACBrNFSeFR;

implementation

uses ACBrNFSe, ACBrDFeUtil, StrUtils, Math;

{$R *.dfm}

{ TdmACBrNFSeFR }

procedure TdmACBrNFSeFR.CarregaDados;
begin
  CarregaIdentificacao;
  CarregaPrestador;
  CarregaTomador;
  CarregaServicos;
  CarregaItensServico;
  CarregaParametros;
end;

procedure TdmACBrNFSeFR.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFSe do
    begin
      FieldByName('Id').AsString                := IdentificacaoRps.Numero + IdentificacaoRps.Serie;
      FieldByName('Numero').AsString            := FormatarNumeroDocumentoFiscalNFSe( IdentificacaoRps.Numero );
      FieldByName('Competencia').AsString       := Competencia;
      FieldByName('NFSeSubstituida').AsString   := FormatarNumeroDocumentoFiscalNFSe( NfseSubstituida);
      FieldByName('NumeroNFSe').AsString        := FormatarNumeroDocumentoFiscalNFSe( Numero );
      FieldByName('DataEmissao').AsString       := FormatDateBr( DataEmissao );
      FieldByName('CodigoVerificacao').AsString := CodigoVerificacao;
    end;
    Post;
  end;
end;

procedure TdmACBrNFSeFR.CarregaItensServico;
var
  i: Integer;
begin
  with cdsItensServico do
  begin
    Close;
    CreateDataSet;

    for i := 0 to FNFSe.Servico.ItemServico.Count -1 do
      with FNFSe.Servico.ItemServico.Items[i] do
      begin
        Append;
        cdsItensServicoDiscriminacaoServico.AsString := Descricao;
        cdsItensServicoQuantidade.AsFloat            := Quantidade;
        cdsItensServicoValorUnitario.AsFloat         := ValorUnitario;
        cdsItensServicoValorTotal.AsFloat            := ValorTotal;
        cdsItensServicoTributavel.AsString           := SimNaoToStr(Tributavel);
        Post;
      end;
  end;
end;

procedure TdmACBrNFSeFR.CarregaParametros;
begin
  with cdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFse  do
    begin
      FieldByName('OutrasInformacoes').AsString			    := OutrasInformacoes;
      FieldByName('NaturezaOperacao').AsString          := NaturezaOperacaoDescricao( NaturezaOperacao );
      FieldByName('RegimeEspecialTributacao').AsString  := nfseRegimeEspecialTributacaoDescricao( RegimeEspecialTributacao );
      FieldByName('OptanteSimplesNacional').AsString    := SimNao( Integer ( OptanteSimplesNacional ) );
      FieldByName('IncentivadorCultural').AsString      := SimNao( Integer ( IncentivadorCultural ) );

      with Servico do
      begin
        FieldByName('CodigoMunicipio').AsString         := IfThen( CodigoMunicipio <> '', CodCidadeToCidade(StrToInt(CodigoMunicipio)) , '')  ;
        FieldByName('ExigibilidadeISS').AsString        := ExigibilidadeISSDescricao( ExigibilidadeISS );
        FieldByName('MunicipioIncidencia').AsString     := CodCidadeToCidade(StrToIntDef( CodigoMunicipio ,0));
      end;

      with ConstrucaoCivil do
      begin
        FieldByName('CodigoObra').AsString				      := CodigoObra;
        FieldByName('Art').AsString					    	      := Art;
      end;
    end;

    CarregaaLogoPrefeitura;
    CarregaaImagemPrestadora;

    FieldByName('Sistema').AsString   := DANFSeClassOwner.Sistema;
    FieldByName('Usuario').AsString   := DANFSeClassOwner.Usuario;
    FieldByName('Mensagem0').AsString := IfThen( DANFSeClassOwner.NFSeCancelada, 'NFSe CANCELADA' ,'');

    Post;
  end;
end;

procedure TdmACBrNFSeFR.CarregaPrestador;
begin
  with cdsPrestador do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFSe.PrestadorServico do
    begin
      FieldByName('RazaoSocial').AsString           := RazaoSocial;
      FieldByName('NomeFantasia').AsString          := NomeFantasia;

      with IdentificacaoPrestador do
      begin
        FieldByName('Cnpj').AsString                := FormatarCNPJ(Cnpj);
        FieldByName('InscricaoMunicipal').AsString  := InscricaoMunicipal;
      end;

      with Endereco do
      begin
        FieldByName('Endereco').AsString            := Endereco;
        FieldByName('Numero').AsString              := Numero;
        FieldByName('Complemento').AsString         := Complemento;
        FieldByName('Bairro').AsString              := Bairro;
        FieldByName('CodigoMunicipio').AsString     := CodigoMunicipio;
        FieldByName('UF').AsString                  := UF;
        FieldByName('CEP').AsString                 := FormatarCEP(CEP);
        FieldByName('xMunicipio').AsString          := xMunicipio;
        FieldByName('CodigoPais').AsString          := IntToStr(CodigoPais);
      end;

      with Contato do
      begin
        FieldByName('Telefone').AsString            := FormatarFone(Telefone);
        FieldByName('Email').AsString               := Email;
      end;
    end;
    Post;
  end;
end;

procedure TdmACBrNFSeFR.CarregaServicos;
var
  i: Integer;
begin
  with cdsServicos do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFSe.Servico do
    begin

      FieldByName('ItemListaServico').AsString          := ItemListaServico;
      FieldByName('xItemListaServico').AsString         := xItemListaServico;
      FieldByName('CodigoCnae').AsString                := CodigoCnae;
      FieldByName('CodigoTributacaoMunicipio').AsString := CodigoTributacaoMunicipio;
	    FieldByName('Discriminacao').AsString             := StringReplace(Discriminacao, TACBrNFSe(DANFSeClassOwner.ACBrNFSe).Configuracoes.WebServices.QuebradeLinha, #13, [rfReplaceAll, rfIgnoreCase]);
      FieldByName('CodigoPais').AsString                := IntToStr(CodigoPais);
      FieldByName('NumeroProcesso').AsString            := NumeroProcesso;
      FieldByName('Descricao').AsString                 := Descricao;

      with Valores do
      begin
        FieldByName('ValorServicos').AsFloat            := ValorServicos;
        FieldByName('ValorDeducoes').AsFloat            := ValorDeducoes;
        FieldByName('ValorPis').AsFloat                 := ValorPis;
        FieldByName('ValorCofins').AsFloat              := ValorCofins;
        FieldByName('ValorInss').AsFloat                := ValorInss;
        FieldByName('ValorIr').AsFloat                  := ValorIr;
        FieldByName('ValorCsll').AsFloat                := ValorCsll;
        FieldByName('IssRetido').AsString               := SituacaoTributariaDescricao( IssRetido );
        FieldByName('ValorIss').AsFloat                 := ValorIss;
        FieldByName('OutrasRetencoes').AsFloat          := OutrasRetencoes;
        FieldByName('BaseCalculo').AsFloat              := BaseCalculo;
        FieldByName('Aliquota').AsFloat                 := Aliquota;
        FieldByName('ValorLiquidoNfse').AsFloat         := ValorLiquidoNfse;
        FieldByName('ValorIssRetido').AsFloat           := ValorIssRetido;
        FieldByName('DescontoCondicionado').AsFloat     := DescontoCondicionado;
        FieldByName('DescontoIncondicionado').AsFloat   := DescontoIncondicionado;
      end;
    end;
    Post;
  end;
end;

procedure TdmACBrNFSeFR.CarregaTomador;
begin
  with cdsTomador do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFSe.Tomador do
    begin

      FieldByName('RazaoSocial').AsString         := RazaoSocial;
      FieldByName('CpfCnpj').AsString             := ManterDocumento( IdentificacaoTomador.CpfCnpj);
      FieldByName('InscricaoMunicipal').AsString  := IdentificacaoTomador.InscricaoMunicipal;

      with Endereco do
      begin
        FieldByName('Endereco').AsString          := Endereco;
        FieldByName('Numero').AsString            := Numero;
        FieldByName('Complemento').AsString       := Complemento;
        FieldByName('Bairro').AsString            := Bairro;
        FieldByName('CodigoMunicipio').AsString   := CodigoMunicipio;
        FieldByName('UF').AsString                := UF;
        FieldByName('CEP').AsString               := FormatarCEP(CEP);
        FieldByName('xMunicipio').AsString        := xMunicipio;
        FieldByName('CodigoPais').AsString        := IntToStr(CodigoPais);
      end;

      with Contato do
      begin
        FieldByName('Telefone').AsString          := FormatarFone(Telefone);
        FieldByName('Email').AsString             := Email;
      end;

    end;
    Post;
  end;
end;

constructor TdmACBrNFSeFR.Create(AOwner: TComponent);
begin
  inherited;
  FDANFSeClassOwner := TACBrNFSeDANFSeClass(AOwner);
end;

procedure TdmACBrNFSeFR.frxReportBeforePrint(Sender: TfrxReportComponent);
begin
  frxReport.FindObject('Memo23').Visible := DANFSeClassOwner.ImprimeCanhoto;
  frxReport.FindObject('Memo75').Visible := DANFSeClassOwner.ImprimeCanhoto;
  frxReport.FindObject('Memo77').Visible := DANFSeClassOwner.ImprimeCanhoto;
  frxReport.FindObject('Memo68').Visible := DANFSeClassOwner.ImprimeCanhoto;;
  frxReport.FindObject('Memo73').Visible := DANFSeClassOwner.ImprimeCanhoto;
end;

procedure TdmACBrNFSeFR.CarregaaLogoPrefeitura;
var
  vStream: TMemoryStream;
  vStringStream: TStringStream;
begin
  With DANFSeClassOwner do
  begin

    cdsParametros.FieldByName('LogoPrefExpandido').AsString := IfThen( ExpandirLogoMarca ,'0','1'); // Prefeitura

    if NaoEstaVazio(DANFSeClassOwner.Logo) then
    begin
      cdsParametros.FieldByName('Nome_Prefeitura').AsString := Prefeitura;
      cdsParametros.FieldByName('imgPrefeitura').AsString   := Logo;

      vStream := TMemoryStream.Create;
      try
        if FileExists( Logo ) then
          vStream.LoadFromFile( Logo )
        else
        begin
          vStringStream := TStringStream.Create( Logo );
          try
            vStream.LoadFromStream(vStringStream);
          finally
            vStringStream.Free;
          end;
        end;
        vStream.Position := 0;
        cdsParametrosLogoPrefCarregado.LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;
  end;
end;

procedure TdmACBrNFSeFR.CarregaaImagemPrestadora;
var
  vStream: TMemoryStream;
  vStringStream: TStringStream;

begin

  With DANFSeClassOwner do
  begin

    cdsParametros.FieldByName('LogoExpandido').AsString := IfThen( ExpandirLogoMarca ,'0','1'); // Prestador

    if NaoEstaVazio(PrestLogo) then
    begin
      cdsParametros.FieldByName('Imagem').AsString := PrestLogo;

      vStream := TMemoryStream.Create;
      try
        if FileExists( PrestLogo ) then
          vStream.LoadFromFile( PrestLogo)
        else
        begin
          vStringStream := TStringStream.Create( PrestLogo);
          try
            vStream.LoadFromStream(vStringStream);
          finally
            vStringStream.Free;
          end;
        end;
        vStream.Position := 0;
        cdsParametrosLogoCarregado.LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;
  end;
end;

Function TdmACBrNFSeFR.ManterDocumento( sCpfCnpj : String ) : String;
begin
  Result := sCpfCnpj;
  if NaoEstaVazio( Result ) then
  begin
    if Length(result) > 11 then
      result := FormatarCNPJ(result)
    else
      result := FormatarCPF(result);
  end;
end;


end.
