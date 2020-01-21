unit ACBrrBoletoTest;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrBoleto,
  ACBrBancoCaixa, ACBrBancoUnicredRS, ACBrBancoUnicredES,
  typinfo,
  {$ifdef FPC}
  fpcunit, testregistry, LConvEncoding
  {$else}
    TestFramework
  {$endif};

type

  TTipoArquivo = (Remessa, Retorno);

  {$REGION - Classes Banco do Brasil}

  { CalcularDigitoVerificador_BB_Test }

  CalcularDigitoVerificador_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularDigitoVerificador_BancoBrasil;
    procedure CalcularDigitoVerificador_BancoBrasil_X;
    procedure CalcularDigitoVerificador_BancoBrasil_0;
    procedure CalcularDigitoVerificador_BancoBrasil_1;
    procedure CalcularDigitoVerificador_BancoBrasil_2;
    procedure CalcularDigitoVerificador_BancoBrasil_3;
    procedure CalcularDigitoVerificador_BancoBrasil_4;
    procedure CalcularDigitoVerificador_BancoBrasil_5;
    procedure CalcularDigitoVerificador_BancoBrasil_6;
    procedure CalcularDigitoVerificador_BancoBrasil_7;
    procedure CalcularDigitoVerificador_BancoBrasil_8;
    procedure CalcularDigitoVerificador_BancoBrasil_9;
    procedure CalcularDigitoVerificador_BancoBrasil_Alfa;
    procedure CalcularDigitoVerificador_BancoBrasil_Nulo;
    procedure CalcularDigitoVerificador_BancoBrasil_NumInvalido;
  end;

  { CalcularTamMaximoNossoNumero_BB_Test}

  CalcularTamMaximoNossoNumero_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Tam5;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Tam7;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Tam10;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Tam11;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Tam17;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Padrao;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Alfa;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_Nulo;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_CarteiraNulo;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_ConvenioNulo;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_NossoNumInvalido;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_CarteiraInvalido;
    procedure CalcularTamMaximoNossoNumero_BancoBrasil_ConvenioInvalido;
  end;

  { MontarCampoCodigoCedente_BB_Test }

  MontarCampoCodigoCedente_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoCodigoCedente_BancoBrasil_Padrao;
    procedure MontarCampoCodigoCedente_BancoBrasil_Invalido;
    procedure MontarCampoCodigoCedente_BancoBrasil_Alfa;
  end;

  { MontarCampoNossoNumero_BB_Test }

  MontarCampoNossoNumero_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoNossoNumero_BancoBrasil_17Digitos;
    procedure MontarCampoNossoNumero_BancoBrasil_11Digitos;
    procedure MontarCampoNossoNumero_BancoBrasil_SemConvenio;
    procedure MontarCampoNossoNumero_BancoBrasil_SemCarteira;
    procedure MontarCampoNossoNumero_BancoBrasil_SemNossoNumero;
    procedure MontarCampoNossoNumero_BancoBrasil_Alfa;
    procedure MontarCampoNossoNumero_BancoBrasil_NossoNumeroInvalido;
    procedure MontarCampoNossoNumero_BancoBrasil_ConvenioInvalido;
    procedure MontarCampoNossoNumero_BancoBrasil_CarteiraInvalido;
    procedure MontarCampoNossoNumero_BancoBrasil_ConvenioAte4Dig;
    procedure MontarCampoNossoNumero_BancoBrasil_Convenio5Dig;
    procedure MontarCampoNossoNumero_BancoBrasil_Convenio6Dig;
    procedure MontarCampoNossoNumero_BancoBrasil_Convenio7Dig;
    procedure MontarCampoNossoNumero_BancoBrasil_ConvenioMaior7Dig;

  end;

  { MontarCodigoBarras_BB_Test }

  MontarCodigoBarras_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCodigoBarras_BancoBrasil_cod17;
    procedure MontarCodigoBarras_BancoBrasil_cod11;
    procedure MontarCodigoBarras_BancoBrasil_Alfa;
    procedure MontarCodigoBarras_BancoBrasil_CodIncompleto;
    procedure MontarCodigoBarras_BancoBrasil_DataNula;
    procedure MontarCodigoBarras_BancoBrasil_valorZero;
    procedure MontarCodigoBarras_BancoBrasil_SemConvenio;
    procedure MontarCodigoBarras_BancoBrasil_SemCarteira;
    procedure MontarCodigoBarras_BancoBrasil_SemNossoNumero;

  end;

  { MontarCampoCarteira_BB_Test }

  MontarCampoCarteira_BB_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoCarteira_BancoBrasil_Padrao;
    procedure MontarCampoCarteira_BancoBrasil_Modalidade;
  end;

  { GerarRemessa_BB_Test }

  GerarRemessa_BB_Test= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarRemessa_BancoBrasil400;
    procedure GerarRemessa_BancoBrasil240;
  end;

   { GerarRetorno_BB_Test }

  GerarRetorno_BB_Test= class(TTestCase)
  protected
  published
    procedure GerarRetorno_BB400;
    procedure GerarRetorno_BB240;
  end;

  {$ENDREGION}

  {$REGION - Classes Banco Caixa Economica}

   { CalcularDigitoVerificador_Caixa_Test }

  CalcularDigitoVerificador_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularDigitoVerificador_Caixa_padrao;
    procedure CalcularDigitoVerificador_Caixa_0;
    procedure CalcularDigitoVerificador_Caixa_1;
    procedure CalcularDigitoVerificador_Caixa_2;
    procedure CalcularDigitoVerificador_Caixa_3;
    procedure CalcularDigitoVerificador_Caixa_4;
    procedure CalcularDigitoVerificador_Caixa_5;
    procedure CalcularDigitoVerificador_Caixa_6;
    procedure CalcularDigitoVerificador_Caixa_7;
    procedure CalcularDigitoVerificador_Caixa_8;
    procedure CalcularDigitoVerificador_Caixa_9;
    procedure CalcularDigitoVerificador_Caixa_Alfa;
    procedure CalcularDigitoVerificador_Caixa_Nulo;
    procedure CalcularDigitoVerificador_Caixa_NumInvalido;
    procedure CalcularDigitoVerificador_Caixa_CarteiraNulo;
    procedure CalcularDigitoVerificador_Caixa_CarteiraInvalido;
  end;

   { CalcularTamMaximoNossoNumero_Caixa_Test}

  CalcularTamMaximoNossoNumero_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularTamMaximoNossoNumero_Caixa_Padrao15;
  end;

  { MontarCampoCodigoCedente_Caixa_Test }

  MontarCampoCodigoCedente_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoCodigoCedente_Caixa_Padrao;
    procedure MontarCampoCodigoCedente_Caixa_Invalido;
    procedure MontarCampoCodigoCedente_Caixa_Alfa;
  end;

  { MontarCampoNossoNumero_Caixa_Test }

  MontarCampoNossoNumero_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoNossoNumero_Caixa_Padrao;
    procedure MontarCampoNossoNumero_Caixa_SemNossoNumero;
    procedure MontarCampoNossoNumero_Caixa_Alfa;
    procedure MontarCampoNossoNumero_Caixa_NossoNumeroInvalido;
    procedure MontarCampoNossoNumero_Caixa_CarteiraInvalido;
    procedure MontarCampoNossoNumero_Caixa_ModalidadeInvalido;

  end;

  { MontarCodigoBarras_Caixa_Test }

  MontarCodigoBarras_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCodigoBarras_Caixa_Padrao;
    procedure MontarCodigoBarras_Caixa_CampoLivreDig0;
    procedure MontarCodigoBarras_Caixa_Alfa;
    procedure MontarCodigoBarras_Caixa_NossoNumeroInvalido;
    procedure MontarCodigoBarras_Caixa_CarteiraInvalida;
    procedure MontarCodigoBarras_Caixa_SemNossoNumero;
    procedure MontarCodigoBarras_Caixa_CodCedenteInvalido;
    procedure MontarCodigoBarras_Caixa_TipoCarteira;
  end;

  { CalcularDVCedente_Caixa_Test }

  CalcularDVCedente_Caixa_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
    Caixa  : TACBrCaixaEconomica;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularDVCedente_Caixa_Padrao;
    procedure CalcularDVCedente_Caixa_Dig0;
    procedure CalcularDVCedente_Caixa_Invalido;
  end;

  { GerarRemessa_Caixa_Test }

  GerarRemessa_Caixa_Test= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarRemessa_Caixa400;
    procedure GerarRemessa_Caixa240;
  end;

 { GerarRetorno_Caixa_Test }

  GerarRetorno_Caixa_Test= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarRetorno_Caixa400;
    procedure GerarRetorno_Caixa240;
  end;

  {$ENDREGION}

  {$REGION - Classes Banco Bradesco}

  { CalcularDigitoVerificador_Bradesco_Test }

    CalcularDigitoVerificador_Bradesco_Test= class(TTestCase)
    private
      Boleto : TACBrBoleto;
      Titulo : TACBrTitulo;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure CalcularDigitoVerificador_Bradesco_P;
      procedure CalcularDigitoVerificador_Bradesco_0;
      procedure CalcularDigitoVerificador_Bradesco_1;
      procedure CalcularDigitoVerificador_Bradesco_2;
      procedure CalcularDigitoVerificador_Bradesco_3;
      procedure CalcularDigitoVerificador_Bradesco_4;
      procedure CalcularDigitoVerificador_Bradesco_5;
      procedure CalcularDigitoVerificador_Bradesco_6;
      procedure CalcularDigitoVerificador_Bradesco_7;
      procedure CalcularDigitoVerificador_Bradesco_8;
      procedure CalcularDigitoVerificador_Bradesco_9;
      procedure CalcularDigitoVerificador_Bradesco_Alfa;
      procedure CalcularDigitoVerificador_Bradesco_Nulo;
      procedure CalcularDigitoVerificador_Bradesco_NumInvalido;
      procedure CalcularDigitoVerificador_Bradesco_CarteiraNulo;
      procedure CalcularDigitoVerificador_Bradesco_CarteiraInvalido;
    end;

  { CalcularTamMaximoNossoNumero_Bradesco_Test }

  CalcularTamMaximoNossoNumero_Bradesco_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularTamMaximoNossoNumero_Bradesco_Padrao11;
  end;

  { MontarCampoCodigoCedente_Bradesco_Test }

  MontarCampoCodigoCedente_Bradesco_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoCodigoCedente_Bradesco_Padrao;
    procedure MontarCampoCodigoCedente_Bradesco_Invalido;
    procedure MontarCampoCodigoCedente_Bradesco_Alfa;
  end;

  { MontarCampoNossoNumero_Bradesco_Test }

  MontarCampoNossoNumero_Bradesco_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoNossoNumero_Bradesco_Padrao;
    procedure MontarCampoNossoNumero_Bradesco_SemNossoNumero;
    procedure MontarCampoNossoNumero_Bradesco_Alfa;
    procedure MontarCampoNossoNumero_Bradesco_NossoNumeroInvalido;
    procedure MontarCampoNossoNumero_Bradesco_CarteiraInvalido;

  end;

  { MontarCodigoBarras_Bradesco_Test }

  MontarCodigoBarras_Bradesco_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCodigoBarras_Bradesco_Padrao;
    procedure MontarCodigoBarras_Bradesco_Alfa;
    procedure MontarCodigoBarras_Bradesco_NossoNumeroInvalido;
    procedure MontarCodigoBarras_Bradesco_CarteiraInvalida;
    procedure MontarCodigoBarras_Bradesco_SemNossoNumero;
    procedure MontarCodigoBarras_Bradesco_ContaAgenciaInvalida;
    procedure MontarCodigoBarras_Bradesco_VencimentoNulo;
  end;

  {$ENDREGION}

  {$REGION - Classes Unicred}

  { CalcularDigitoVerificador_Unicred_Test }

  CalcularDigitoVerificador_Unicred_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularDigitoVerificador_Unicred;
    procedure CalcularDigitoVerificador_Unicred_0;
    procedure CalcularDigitoVerificador_Unicred_1;
    procedure CalcularDigitoVerificador_Unicred_2;
    procedure CalcularDigitoVerificador_Unicred_3;
    procedure CalcularDigitoVerificador_Unicred_4;
    procedure CalcularDigitoVerificador_Unicred_5;
    procedure CalcularDigitoVerificador_Unicred_6;
    procedure CalcularDigitoVerificador_Unicred_7;
    procedure CalcularDigitoVerificador_Unicred_8;
    procedure CalcularDigitoVerificador_Unicred_9;
    procedure CalcularDigitoVerificador_Unicred_Alfa;
    procedure CalcularDigitoVerificador_Unicred_Nulo;
    procedure CalcularDigitoVerificador_Unicred_NumInvalido;
    procedure CalcularDigitoVerificador_Unicred_CarteiraNulo;
    procedure CalcularDigitoVerificador_Unicred_CarteiraInvalido;
  end;

  { CalcularTamMaximoNossoNumero_Unicred_Test}

  CalcularTamMaximoNossoNumero_Unicred_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CalcularTamMaximoNossoNumero_Unicred;
  end;

  { MontarCampoCodigoCedente_Unicred_Test }

  MontarCampoCodigoCedente_Unicred_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoCodigoCedente_Unicred_Padrao;
    procedure MontarCampoCodigoCedente_Unicred_Invalido;
    procedure MontarCampoCodigoCedente_Unicred_Alfa;
  end;

  { MontarCampoNossoNumero_Unicred_Test }

  MontarCampoNossoNumero_Unicred_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoNossoNumero_Unicred_Padrao;
    procedure MontarCampoNossoNumero_Unicred_SemNossoNumero;
    procedure MontarCampoNossoNumero_Unicred_Alfa;
    procedure MontarCampoNossoNumero_Unicred_NossoNumeroInvalido;
    procedure MontarCampoNossoNumero_Unicred_CarteiraInvalido;

  end;

  { MontarCodigoBarras_Unicred_Test }

  MontarCodigoBarras_Unicred_Test= class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCodigoBarras_Unicred_Padrao;
    procedure MontarCodigoBarras_Unicred_Alfa;
    procedure MontarCodigoBarras_Unicred_NossoNumeroInvalido;
    procedure MontarCodigoBarras_Unicred_SemNossoNumero;
    procedure MontarCodigoBarras_Unicred_ContaAgenciaInvalida;
    procedure MontarCodigoBarras_Unicred_VencimentoNulo;
  end;

  { GerarRemessa_Unicred_Test }

  GerarRemessa_Unicred_Test= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarRemessa_Unicred400;
  end;

  { GerarRetorno_Unicred_Test }

  GerarRetorno_Unicred_Test= class(TTestCase)
  protected
  published
    procedure GerarRetorno_Unicred400;
  end;

  {$ENDREGION}

  {$REGION - Classes UnicredES}

  { MontarCampoNossoNumero_UnicredES_Test}

  MontarCampoNossoNumero_UnicredES_Test = class(TTestCase)
  private
    Boleto : TACBrBoleto;
    Titulo : TACBrTitulo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MontarCampoNossoNumero_UnicredES_Padrao;
    procedure MontarCampoNossoNumero_UnicredES_SemNossoNumero;
    procedure MontarCampoNossoNumero_UnicredES_Alfa;
    procedure MontarCampoNossoNumero_UnicredES_NossoNumeroInvalido;

  end;

  {$ENDREGION}

procedure CriaBoletoPadrao(NomeArquivo: String; Diretorio: TTipoArquivo;
                           CNAB: TACBrLayoutRemessa; Banco: TACBrTipoCobranca);
function GravaRetorno(NomeArquivo: String; Boleto: TACBrBoleto): String;
function CarregarArquivos(NomeArquivo: String; Diretorio: TTipoArquivo ): String;
function AlteraDataArquivo(Str, Valor: String; Posicao: Integer): String;


implementation

uses
  dateutils, ACBrUtil, forms;

{ MontarCampoNossoNumero_UnicredES_Test }

procedure MontarCampoNossoNumero_UnicredES_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoNossoNumero_UnicredES_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoNossoNumero_UnicredES_Test.MontarCampoNossoNumero_UnicredES_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredES;
  Titulo.NossoNumero          := '1234567890';

  CheckEquals('1234567890-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));

end;

procedure MontarCampoNossoNumero_UnicredES_Test.MontarCampoNossoNumero_UnicredES_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredES;
  Titulo.NossoNumero          := '';

  CheckEquals('0000000000-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_UnicredES_Test.MontarCampoNossoNumero_UnicredES_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredES;
  Titulo.NossoNumero          := '12435A552x';

  CheckEquals('0012435552-8', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_UnicredES_Test.MontarCampoNossoNumero_UnicredES_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 10';
begin
  try
    Boleto.Banco.TipoCobranca   := cobUnicredES;
    Titulo.NossoNumero          := '12423423422335A552x';

    CheckEquals('0012435552-9', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  except on e: Exception do
    CheckEquals( MSG, e.Message );
  end;
end;

  {$REGION - Implementação Classes Banco Unicred }

{ GerarRetorno_Unicred_Test }

procedure GerarRetorno_Unicred_Test.GerarRetorno_Unicred400;
var
  SLArqGerado, SLArqValido: TStringList;
begin
  try
    SLArqGerado := TStringList.Create;
    SLArqValido := TStringList.Create;

    //Cria Componente boleto padrão apenas dados Banco e Cedente e Lê o retorno
    CriaBoletoPadrao( 'RetUnicred400.ret', Retorno, c400, cobUnicredRS );

    //Carrega Arquivo de Retorno Gerado para comparar dados
    SLArqGerado.LoadFromFile( CarregarArquivos('ResultRetUnicred400.ret', Retorno) );

    //Carrega Arquivo Válido
    SLArqValido.LoadFromFile( CarregarArquivos('ResultRetUnicred400Valido.ret', Retorno) );

    CheckEquals( SLArqValido.Text, SLArqGerado.Text );

  finally
    FreeAndNil(SLArqValido);
    FreeAndNil(SLArqGerado);
  end;
end;

{ GerarRemessa_Unicred_Test }

procedure GerarRemessa_Unicred_Test.SetUp;
begin
  inherited;
end;

procedure GerarRemessa_Unicred_Test.TearDown;
begin
  inherited;
end;

procedure GerarRemessa_Unicred_Test.GerarRemessa_Unicred400;
var
  lArqGerado, lArqValido: TStringList;
begin
  try

    lArqGerado := TStringList.Create;
    lArqValido := TStringList.Create;

    CriaBoletoPadrao('RemessaUnicred400.rem', Remessa, c400, cobUnicredRS);

    //Carrega Arquivo Gerado
    lArqGerado.LoadFromFile( CarregarArquivos('RemessaUnicred400.rem', Remessa) );

    //Altera a Data Atual gerada pela Data do Arquivo Válido
    lArqGerado.Text:= StringReplace(lArqGerado.Text,
                      FormatDateTime('ddmmyy',now),'160120',[rfReplaceAll]);

    //Carrega Arquivo Válido
    lArqValido.LoadFromFile( CarregarArquivos('RemessaUnicred400Valido.rem', Remessa) );

    CheckEquals(lArqValido.Text, lArqGerado.Text);

  finally
    FreeAndNil(lArqValido);
    FreeAndNil(lArqGerado);
  end;

end;

{ MontarCodigoBarras_Unicred_Test }

procedure MontarCodigoBarras_Unicred_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCodigoBarras_Unicred_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '5526';
  Boleto.Cedente.Conta        := '2145';
  Boleto.Cedente.ContaDigito  := '1';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '003456789';

  CheckEquals('09191720600000300005526000002145100034567897', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '5526';
  Boleto.Cedente.Conta        := '2145';
  Boleto.Cedente.ContaDigito  := '0';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '0034SS56789';

  CheckEquals('09196720600000300005526000002145000034567891', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 10';
begin
  try
    Boleto.Banco.TipoCobranca   := cobUnicredRS;
    Boleto.Cedente.Agencia      := '5526';
    Boleto.Cedente.Conta        := '2145';
    Boleto.Cedente.ContaDigito  := '0';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 300;
    Titulo.Carteira             := '17';
    Titulo.NossoNumero          := '111111111111111111111111111';

    Boleto.Banco.MontarCodigoBarras(Titulo);

  except on e: Exception do
    CheckEquals( MSG, e.Message )
  end;
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '5526';
  Boleto.Cedente.Conta        := '2145';
  Boleto.Cedente.ContaDigito  := '0';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '';

  CheckEquals('09191720600000300005526000002145000000000000', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_ContaAgenciaInvalida;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '';
  Boleto.Cedente.Conta        := '';
  Boleto.Cedente.ContaDigito  := '';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '123456789';

  CheckEquals('09197720600000300000000000000000001234567892', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Unicred_Test.MontarCodigoBarras_Unicred_VencimentoNulo;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '1254';
  Boleto.Cedente.Conta        := '12353';
  Boleto.Cedente.ContaDigito  := '1';
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '123456789';

  CheckEquals('09195000000000300001254000012353101234567892', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

{ MontarCampoNossoNumero_Unicred_Test }

procedure MontarCampoNossoNumero_Unicred_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoNossoNumero_Unicred_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoNossoNumero_Unicred_Test.MontarCampoNossoNumero_Unicred_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1234567890';

  CheckEquals('17/1234567890-9', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Unicred_Test.MontarCampoNossoNumero_Unicred_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '';

  CheckEquals('17/0000000000-6', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Unicred_Test.MontarCampoNossoNumero_Unicred_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := 'A12345x';

  CheckEquals('RG/0000012345-5', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Unicred_Test.MontarCampoNossoNumero_Unicred_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 10';
begin
  try
    Boleto.Banco.TipoCobranca   := cobUnicredRS;
    Titulo.Carteira             := '17';
    Titulo.NossoNumero          := '5421X5524424155211565225';
    Boleto.Banco.MontarCampoNossoNumero(Titulo);

  except on e: Exception do
    CheckEquals( MSG, e.Message );

  end;
end;

procedure MontarCampoNossoNumero_Unicred_Test.MontarCampoNossoNumero_Unicred_CarteiraInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '';
  Titulo.NossoNumero          := '123';

  CheckEquals('00/0000000123-6', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

{ MontarCampoCodigoCedente_Unicred_Test }

procedure MontarCampoCodigoCedente_Unicred_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoCodigoCedente_Unicred_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoCodigoCedente_Unicred_Test.MontarCampoCodigoCedente_Unicred_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '1552';
  Boleto.Cedente.AgenciaDigito:= '1';
  Boleto.Cedente.Conta        := '5425525';
  Boleto.Cedente.ContaDigito  := '3';

  CheckEquals('1552-1/5425525-3', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Unicred_Test.MontarCampoCodigoCedente_Unicred_Invalido;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '';
  Boleto.Cedente.AgenciaDigito:= '1';
  Boleto.Cedente.Conta        := '';
  Boleto.Cedente.ContaDigito  := '3';

  CheckEquals('-1/-3', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Unicred_Test.MontarCampoCodigoCedente_Unicred_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Boleto.Cedente.Agencia      := '1552';
  Boleto.Cedente.AgenciaDigito:= 'x';
  Boleto.Cedente.Conta        := '5425x525';
  Boleto.Cedente.ContaDigito  := 'x';

  CheckEquals('1552-x/5425x525-x', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

{ CalcularTamMaximoNossoNumero_Unicred_Test }

procedure CalcularTamMaximoNossoNumero_Unicred_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularTamMaximoNossoNumero_Unicred_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularTamMaximoNossoNumero_Unicred_Test.CalcularTamMaximoNossoNumero_Unicred;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Check(Boleto.Banco.TamanhoMaximoNossoNum = 10);
end;

{ CalcularDigitoVerificador_Unicred_Test }

procedure CalcularDigitoVerificador_Unicred_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularDigitoVerificador_Unicred_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '12345';

  CheckEquals('5', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_0;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '12345678';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));

end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_1;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1238';

  CheckEquals('1', Boleto.Banco.CalcularDigitoVerificador(Titulo));

end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_2;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1232';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_3;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1237';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_4;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1231';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_5;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1236';

  CheckEquals('5', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_6;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '123456';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_7;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1235';

  CheckEquals('7', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_8;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '02';
  Titulo.NossoNumero          := '1';

  CheckEquals('8', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_9;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '1234';

  CheckEquals('9', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1234cd';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_Nulo;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_NumInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 10';
begin
  try
    Boleto.Banco.TipoCobranca   := cobUnicredRS;
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '9999999999999999999999999999999';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;

end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_CarteiraNulo;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '';
  Titulo.NossoNumero          := '';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Unicred_Test.CalcularDigitoVerificador_Unicred_CarteiraInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobUnicredRS;
  Titulo.Carteira             := '1565';
  Titulo.NossoNumero          := '123';

  CheckEquals('1', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

  {$ENDREGION}

  {$REGION - Implementação Classes Banco Bradesco}

{ MontarCodigoBarras_Bradesco_Test }

procedure MontarCodigoBarras_Bradesco_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCodigoBarras_Bradesco_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '5526';
  Boleto.Cedente.Conta        := '2145';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '003456789';

  CheckEquals('23797720600000300005526RG0000345678900021450', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '5545';
  Boleto.Cedente.Conta        := '214554';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 300;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '003AVB456774';

  CheckEquals('23791720600000300005545RG0000345677402145540', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 11';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBradesco;
    Boleto.Cedente.Agencia      := '5526';
    Boleto.Cedente.Conta        := '2145';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 300;
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '555555555554545554542';
    Boleto.Banco.MontarCodigoBarras(Titulo);

 except on e: Exception do
   CheckEquals(MSG, e.Message);
 end;

end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_CarteiraInvalida;
begin
    Boleto.Banco.TipoCobranca   := cobBradesco;
    Boleto.Cedente.Agencia      := '5545';
    Boleto.Cedente.Conta        := '214554';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 100.00;
    Titulo.Carteira             := '';
    Titulo.NossoNumero          := '24565551';

    CheckEquals('237927206000001000055450002456555102145540', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '5545';
  Boleto.Cedente.Conta        := '214554';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.80;
  Titulo.Carteira             := '15';
  Titulo.NossoNumero          := '';

  CheckEquals('23799720600000100805545150000000000002145540', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_ContaAgenciaInvalida;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.80;
  Titulo.Carteira             := '15';
  Titulo.NossoNumero          := '';

  CheckEquals('23794720600000100800000150000000000000000000', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Bradesco_Test.MontarCodigoBarras_Bradesco_VencimentoNulo;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.ValorDocumento       := 100.80;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '5595485544';

  CheckEquals('23792000000000100800000RG0559548554400000000', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

{ MontarCampoNossoNumero_Bradesco_Test }

procedure MontarCampoNossoNumero_Bradesco_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoNossoNumero_Bradesco_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoNossoNumero_Bradesco_Test.MontarCampoNossoNumero_Bradesco_Padrao;
begin
  Boleto.banco.TipoCobranca := cobBradesco;
  Titulo.NossoNumero        := '5214524';
  Titulo.Carteira           := 'RG';

  CheckEquals('RG/00005214524-4', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Bradesco_Test.MontarCampoNossoNumero_Bradesco_SemNossoNumero;
begin
  Boleto.banco.TipoCobranca := cobBradesco;
  Titulo.NossoNumero        := '';
  Titulo.Carteira           := 'RG';

  CheckEquals('RG/00000000000-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Bradesco_Test.MontarCampoNossoNumero_Bradesco_Alfa;
begin
  Boleto.banco.TipoCobranca := cobBradesco;
  Titulo.NossoNumero        := '23DER234';
  Titulo.Carteira           := '18';

  CheckEquals('18/00000023234-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Bradesco_Test.MontarCampoNossoNumero_Bradesco_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 11';
begin
  try
    Boleto.banco.TipoCobranca := cobBradesco;
    Titulo.NossoNumero        := '16454654514525844';
    Titulo.Carteira           := '18';
    Boleto.Banco.MontarCampoNossoNumero(Titulo);

  except on e: Exception do
    CheckEquals(MSG,e.Message);
  end;

end;

procedure MontarCampoNossoNumero_Bradesco_Test.MontarCampoNossoNumero_Bradesco_CarteiraInvalido;
begin
  Boleto.banco.TipoCobranca := cobBradesco;
  Titulo.NossoNumero        := '44745884';
  Titulo.Carteira           := '';

  CheckEquals('/00044745884-5', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

{ MontarCampoCodigoCedente_Bradesco_Test }

procedure MontarCampoCodigoCedente_Bradesco_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoCodigoCedente_Bradesco_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoCodigoCedente_Bradesco_Test.MontarCampoCodigoCedente_Bradesco_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '1552';
  Boleto.Cedente.AgenciaDigito:= '1';
  Boleto.Cedente.Conta        := '5425525';
  Boleto.Cedente.ContaDigito  := '3';

  CheckEquals('1552-1/5425525-3', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Bradesco_Test.MontarCampoCodigoCedente_Bradesco_Invalido;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '';
  Boleto.Cedente.AgenciaDigito:= '1';
  Boleto.Cedente.Conta        := '5425525';
  Boleto.Cedente.ContaDigito  := '';

  CheckEquals('-1/5425525-', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Bradesco_Test.MontarCampoCodigoCedente_Bradesco_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Boleto.Cedente.Agencia      := '11A5';
  Boleto.Cedente.AgenciaDigito:= 'X';
  Boleto.Cedente.Conta        := '542XX55';
  Boleto.Cedente.ContaDigito  := 'x';

  CheckEquals('11A5-X/542XX55-x', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

{ CalcularTamMaximoNossoNumero_Bradesco_Test }

procedure CalcularTamMaximoNossoNumero_Bradesco_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularTamMaximoNossoNumero_Bradesco_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularTamMaximoNossoNumero_Bradesco_Test.CalcularTamMaximoNossoNumero_Bradesco_Padrao11;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Check(Boleto.Banco.TamanhoMaximoNossoNum = 11);
end;

{ CalcularDigitoVerificador_Bradesco_Test }

procedure CalcularDigitoVerificador_Bradesco_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularDigitoVerificador_Bradesco_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_P;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '21552';

  CheckEquals('P', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_0;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231239';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_1;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1000000001';

  CheckEquals('1', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_2;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231238';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_3;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231246';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_4;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '1234567';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_5;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231245';

  CheckEquals('5', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_6;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231236';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_7;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231244';

  CheckEquals('7', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_8;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231235';

  CheckEquals('8', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_9;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231257';

  CheckEquals('9', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := 'ASC123x';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_Nulo;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '12';
  Titulo.NossoNumero          := '';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_NumInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 11';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBradesco;
    Titulo.Carteira             := '12';
    Titulo.NossoNumero          := '99999999999999999999999';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;

end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_CarteiraNulo;
begin
  Boleto.Banco.TipoCobranca   := cobBradesco;
  Titulo.Carteira             := '';
  Titulo.NossoNumero          := '52172552552';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Bradesco_Test.CalcularDigitoVerificador_Bradesco_CarteiraInvalido;
begin
  Boleto.Banco.TipoCobranca  := cobBradesco;
  Titulo.Carteira             := '45241ABC';
  Titulo.NossoNumero          := '52172552552';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

  {$ENDREGION}

  {$REGION - Implementação Classes Banco Caixa Economica}

{ CalcularDVCedente_Caixa_Test }

procedure CalcularDVCedente_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
  Caixa  := TACBrCaixaEconomica.Create(nil);
end;

procedure CalcularDVCedente_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
  FreeAndNil(Caixa);
end;

procedure CalcularDVCedente_Caixa_Test.CalcularDVCedente_Caixa_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '6544';
  CheckEquals('7', Caixa.CalcularDVCedente(Titulo));
end;

procedure CalcularDVCedente_Caixa_Test.CalcularDVCedente_Caixa_Dig0;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '2145868';
  CheckEquals('0', Caixa.CalcularDVCedente(Titulo));
end;

procedure CalcularDVCedente_Caixa_Test.CalcularDVCedente_Caixa_Invalido;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '';
  CheckEquals('0', Caixa.CalcularDVCedente(Titulo));
end;

{ MontarCodigoBarras_Caixa_Test }

procedure MontarCodigoBarras_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCodigoBarras_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '5526544';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 520.60;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '003456789';
  CheckEquals('10491720600000520605526544000100040034567897', Boleto.Banco.MontarCodigoBarras(Titulo));


  Boleto.Cedente.CodigoCedente:= '588';
  Titulo.Vencimento           := 0;
  Titulo.ValorDocumento       := 0;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '12036';
  CheckEquals('10496000000000000000005886000100040000120360', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_CampoLivreDig0;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= 'AD45';
  Titulo.Vencimento           := 0;
  Titulo.ValorDocumento       := 0;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '5148852';
  CheckEquals('104920000000000000000AD450000100040051488520', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '2514';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 520.60;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := 'AD58F471x';
  CheckEquals('10493720600000520600025143000100040000584717', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 15';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Boleto.Cedente.CodigoCedente:= '2514';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 520.60;
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '2222222222222222222222';
    Boleto.Banco.MontarCodigoBarras(Titulo);

  except on e: Exception do
    CheckEquals(MSG,e.Message);
  end;
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_CarteiraInvalida;
const
  MSG = 'Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Boleto.Cedente.CodigoCedente:= '2514';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 520.60;
    Titulo.Carteira             := '18';
    Titulo.NossoNumero          := '1234567';
    Boleto.Banco.MontarCodigoBarras(Titulo);

  except on e: Exception do
    CheckEquals(MSG,e.Message);
  end;
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '2514';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 520.60;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '';
  CheckEquals('10491720600000520600025143000200040000000000', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_CodCedenteInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 520.60;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '1002';
  CheckEquals('10494720600000520600000000000200040000010029', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_Caixa_Test.MontarCodigoBarras_Caixa_TipoCarteira;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.CodigoCedente:= '';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 520.60;
  Titulo.CarteiraEnvio        := tceBanco;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '100252';
  CheckEquals('10495720600000520600000000000200010001002524', Boleto.Banco.MontarCodigoBarras(Titulo));

  Titulo.CarteiraEnvio        := tceBanco;
  Titulo.Carteira             := 'RG';
  CheckEquals('10491720600000520600000000000100010001002520', Boleto.Banco.MontarCodigoBarras(Titulo));

  Titulo.CarteiraEnvio        := tceCedente;
  Titulo.Carteira             := 'RG';
  CheckEquals('10495720600000520600000000000100040001002522', Boleto.Banco.MontarCodigoBarras(Titulo));

  Titulo.CarteiraEnvio        := tceCedente;
  Titulo.Carteira             := 'SR';
  CheckEquals('10491720600000520600000000000200040001002526', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

{ MontarCampoNossoNumero_Caixa_Test }

procedure MontarCampoNossoNumero_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoNossoNumero_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '8547125';

  CheckEquals('14000000008547125-4', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '';

  CheckEquals('14000000000000000-6', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := 'FF4543#33FGS56';

  CheckEquals('14000000045433356-6', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 15';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '5251485258626585584485588558';
    Boleto.Banco.MontarCampoNossoNumero(Titulo);

  except on e: Exception do
    CheckEquals(MSG,e.Message);
  end;
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_CarteiraInvalido;
const
  MSG = 'Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Titulo.Carteira             := '';
    Titulo.NossoNumero          := '944745547';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;
end;

procedure MontarCampoNossoNumero_Caixa_Test.MontarCampoNossoNumero_Caixa_ModalidadeInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.Modalidade   := '';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '541552';

  CheckEquals('14000000000541552-2', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

{ MontarCampoCodigoCedente_Caixa_Test }

procedure MontarCampoCodigoCedente_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoCodigoCedente_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoCodigoCedente_Caixa_Test.MontarCampoCodigoCedente_Caixa_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.Agencia      := '1552';
  Boleto.Cedente.CodigoCedente:= '542552555';

  CheckEquals('1552/542552555-1', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Caixa_Test.MontarCampoCodigoCedente_Caixa_Invalido;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.Agencia      := '1552325';
  Boleto.Cedente.CodigoCedente:= '';

  CheckEquals('5523/-0', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_Caixa_Test.MontarCampoCodigoCedente_Caixa_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Boleto.Cedente.Agencia      := '1AA25';
  Boleto.Cedente.CodigoCedente:= 'DDCD';

  CheckEquals('AA25/DDCD-0', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

{ CalcularTamMaximoNossoNumero_Caixa_Test }

procedure CalcularTamMaximoNossoNumero_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularTamMaximoNossoNumero_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularTamMaximoNossoNumero_Caixa_Test.CalcularTamMaximoNossoNumero_Caixa_Padrao15;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Check(Boleto.Banco.TamanhoMaximoNossoNum = 15);
end;

{ CalcularDigitoVerificador_Caixa_Test }

procedure CalcularDigitoVerificador_Caixa_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularDigitoVerificador_Caixa_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_padrao;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '08547125';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_0;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '002526645';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_1;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '4712544';

  CheckEquals('1', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_2;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '9933345';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_3;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '525471';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_4;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '19933345';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_5;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1995';

  CheckEquals('5', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_6;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'SR';
  Titulo.NossoNumero          := '185471';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_7;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '6645';

  CheckEquals('7', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_8;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1999';

  CheckEquals('8', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_9;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1993';

  CheckEquals('9', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '199ABC3x';

  CheckEquals('9', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_Nulo;
begin
  Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_NumInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 15';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '9999999999999999999999999999999';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;

end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_CarteiraNulo;
const
  MSG = 'Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Titulo.Carteira             := '';
    Titulo.NossoNumero          := '99999999';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;
end;

procedure CalcularDigitoVerificador_Caixa_Test.CalcularDigitoVerificador_Caixa_CarteiraInvalido;
const
  MSG = 'Carteira Inválida.'+sLineBreak+'Utilize "RG" ou "SR"';
begin
  try
    Boleto.Banco.TipoCobranca   := cobCaixaEconomica;
    Titulo.Carteira             := '12';
    Titulo.NossoNumero          := '999999999';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;
end;

{ GerarRemessa_Caixa_Test }

procedure GerarRemessa_Caixa_Test.SetUp;
begin
  inherited;
end;

procedure GerarRemessa_Caixa_Test.TearDown;
begin
  inherited;
end;

procedure GerarRemessa_Caixa_Test.GerarRemessa_Caixa400;
var
  lArqGerado, lArqValido: TStringList;
begin
  try

    lArqGerado := TStringList.Create;
    lArqValido := TStringList.Create;

    CriaBoletoPadrao('RemessaCaixa400.rem', Remessa, c400, cobCaixaEconomica);

    //Carrega Arquivo Gerado
    lArqGerado.LoadFromFile( CarregarArquivos('RemessaCaixa400.rem', Remessa) );

    //Altera a Data Atual gerada pela Data do Arquivo Válido
    lArqGerado.Text:= StringReplace(lArqGerado.Text,
                      FormatDateTime('ddmmyy',now),'021219',[rfReplaceAll]);

    //Carrega Arquivo Válido
    lArqValido.LoadFromFile( CarregarArquivos('RemessaCaixa400Valido.rem', Remessa) );

    CheckEquals(lArqValido.Text, lArqGerado.Text);

  finally
    FreeAndNil(lArqValido);
    FreeAndNil(lArqGerado);
  end;

end;

procedure GerarRemessa_Caixa_Test.GerarRemessa_Caixa240;
var
  lArqGerado, lArqValido: TStringList;
begin
  try

    lArqGerado := TStringList.Create;
    lArqValido := TStringList.Create;

    CriaBoletoPadrao('RemessaCaixa240.rem', Remessa, c240, cobCaixaEconomica);

    //Carrega Arquivo Gerado
    lArqGerado.LoadFromFile( CarregarArquivos('RemessaCaixa240.rem', Remessa) );

    //Altera a Data Atual gerada pela Data do Arquivo Válido ('ddmmyyyyhhnnss')
    lArqGerado.Strings[0]:= AlteraDataArquivo(lArqGerado.Strings[0], '02122019000000', 144);

    lArqGerado.Text:= StringReplace(lArqGerado.Text,
                      FormatDateTime('ddmmyyyy',now),'02122019',[rfReplaceAll]);

    //Carrega Arquivo Válido
    lArqValido.LoadFromFile( CarregarArquivos('RemessaCaixa240Valido.rem', Remessa) );

    CheckEquals(lArqValido.Text, lArqGerado.Text);

  finally
    FreeAndNil(lArqValido);
    FreeAndNil(lArqGerado);
  end;

end;

  { GerarRetorno_Caixa_Test }

procedure GerarRetorno_Caixa_Test.SetUp;
begin
  inherited;
end;

procedure GerarRetorno_Caixa_Test.TearDown;
begin
  inherited;
end;

procedure GerarRetorno_Caixa_Test.GerarRetorno_Caixa400;
begin

end;

procedure GerarRetorno_Caixa_Test.GerarRetorno_Caixa240;
var
  SLArqGerado, SLArqValido: TStringList;
begin
  try
    SLArqGerado := TStringList.Create;
    SLArqValido := TStringList.Create;

    //Cria Componente boleto padrão apenas dados Banco e Cedente e Lê o retorno
    CriaBoletoPadrao( 'RetCaixa240.ret', Retorno, c240, cobCaixaEconomica );

    //Carrega Arquivo de Retorno Gerado para comparar dados
    SLArqGerado.LoadFromFile( CarregarArquivos('ResultRetCaixa240.ret', Retorno) );

    //Carrega Arquivo Válido
    SLArqValido.LoadFromFile( CarregarArquivos('ResultRetCaixa240Valido.ret', Retorno) );

    CheckEquals( SLArqValido.Text, SLArqGerado.Text );

  finally
    FreeAndNil(SLArqValido);
    FreeAndNil(SLArqGerado);
  end;

end;

{$ENDREGION}

  {$REGION - Implementação Classes Banco do Brasil}

  { GerarRemessa_BB_Test }

procedure GerarRemessa_BB_Test.SetUp;
begin
  inherited;
end;

procedure GerarRemessa_BB_Test.TearDown;
begin
  inherited;
end;

  {Cria Objeto Boleto Padrão ccom campos para Geração de Remessa e Dados Cabeçalho para Retorno}
procedure CriaBoletoPadrao(NomeArquivo: String; Diretorio: TTipoArquivo;
                               CNAB: TACBrLayoutRemessa; Banco: TACBrTipoCobranca);
var
  I: Integer;
  Boleto: TACBrBoleto;
  Titulo: TACBrTitulo;
begin
  try
    Boleto := TACBrBoleto.Create(nil);

    if (Diretorio = Remessa) then
      Boleto.DirArqRemessa      := ExtractFilePath(Application.ExeName) + 'Remessa\'
    else
      Boleto.DirArqRetorno      := ExtractFilePath(Application.ExeName) + 'Retorno\';
    Boleto.NomeArqRemessa       := NomeArquivo;
    Boleto.NomeArqRetorno       := NomeArquivo;
    Boleto.LayoutRemessa        := CNAB;
    Boleto.Banco.TipoCobranca   := banco;
    Boleto.Cedente.Agencia      := '2145';
    Boleto.Cedente.AgenciaDigito:= '1';
    Boleto.Cedente.Conta        := '545144';
    Boleto.Cedente.ContaDigito  := '2';
    Boleto.Cedente.Convenio     := '2354187';
    Boleto.Cedente.Bairro       := 'Centro';
    Boleto.Cedente.CEP          := '18460000';
    Boleto.Cedente.Cidade       := 'Tatuí';
    Boleto.Cedente.CNPJCPF      := '18760540000139';
    Boleto.Cedente.Logradouro   := 'Rua São Pedro, 124';
    Boleto.Cedente.Nome         := 'Nome do Cedente Completo';
    Boleto.Cedente.NumeroRes    := '524';
    Boleto.Cedente.UF           := 'SP';
    Boleto.Cedente.ResponEmissao:= tbCliEmite;
    Boleto.Cedente.Modalidade   := '019';
    Boleto.Cedente.TipoInscricao:= pJuridica;
    Boleto.Cedente.TipoCarteira := tctSimples;
    Boleto.Cedente.TipoDocumento:= Escritural;
    Boleto.Cedente.CaracTitulo  := tcSimples;
    Boleto.LeCedenteRetorno     := True;

    //Para Retorno Lê o arquivo modelo e Grava Resultado para comparar
    if (Diretorio = Retorno) then
    begin
      Boleto.LerRetorno();
      GravaRetorno('Result' + NomeArquivo, Boleto);

    end
    else
    begin //Para Remessa Gera Arquivo Remessa para comparar com arquivo modelo
      for I := 1 to 3 do
      begin
        Titulo:= Boleto.CriarTituloNaLista;
        with Titulo do
        begin
           LocalPagamento    := 'Pagar preferêncialmente nas agências do '+ Boleto.Banco.Nome;
           Vencimento        := IncMonth(EncodeDate(2017,07,10),I);
           DataDocumento     := EncodeDate(2017,07,10);
           if ( Banco = cobCaixaEconomica ) then
             Carteira        := 'RG'
           else
             Carteira        := '15';
           NumeroDocumento   := PadRight(IntToStr(I),6,'0');
           EspecieDoc        := 'DM';
           Aceite            := atSim;
           DataProcessamento := EncodeDate(2017,07,20);
           NossoNumero       := PadLeft( IntToStr(I), 7, '0');
           ValorDocumento    := 121.15 * (I+0.5);
           Sacado.NomeSacado := 'Jose da Silva ' + IntToStr(I) ;
           Sacado.CNPJCPF    := '1234567890' + IntToStr(I);
           Sacado.Logradouro := 'Rua da Esperança';
           Sacado.Numero     := '100';
           Sacado.Bairro     := 'Centro';
           Sacado.Cidade     := 'Tatui';
           Sacado.UF         := 'SP';
           Sacado.CEP        := '18270000';
           ValorAbatimento   := 1.10;
           DataAbatimento    := Vencimento-5;
           ValorMoraJuros    := 0.20;
           ValorDesconto     := 2.30;
           DataMoraJuros     := Vencimento+5;
           DataDesconto      := Vencimento-1;
           DataProtesto      := Vencimento+10;
           DataMulta         := Vencimento+1;
           PercentualMulta   := 5;
           Mensagem.Text     := 'Teste Mensagem 1'+ sLineBreak +'Teste Mensagem 2'+sLineBreak+'Teste Mensagem 1' ;
           OcorrenciaOriginal.Tipo  := toRemessaRegistrar;
           Instrucao1        := 'teste de instrução 01';
           Instrucao2        := 'teste de instrução 02';
           sacado.SacadoAvalista.NomeAvalista:= 'xxxxxxxx';
           sacado.SacadoAvalista.CNPJCPF:='99999999999';
           sacado.SacadoAvalista.Logradouro:='xxxxxxx xxxxxxxxx xxxxxxxxxx';
        end;
      end;
      Boleto.GerarRemessa(1);

    end;

  finally
    FreeAndNil(Boleto);
  end;

end;

function GravaRetorno(NomeArquivo: String; Boleto: TACBrBoleto): String;
var
  SLRetorno: TStringList;
  NomeArq:   String;
  I, J: Integer;
begin
  try
    result := '';
    with Boleto do
    begin
      if (ListadeBoletos.Count < 1) then
        raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));

      NomeArq := DirArqRetorno + NomeArquivo;
      SLRetorno := TStringList.Create;

      with SLRetorno do
        begin
        add(Cedente.Nome);
        add(Cedente.CNPJCPF);
        add(Cedente.CodigoCedente);
        add(Cedente.Modalidade);
        add(Cedente.CodigoTransmissao);
        add(Cedente.Convenio);
        add(inttostr(Banco.Numero));
        add(GetEnumName( TypeInfo(TACBrTipoCobranca), Integer(Banco.TipoCobranca) ));
        add(Cedente.Conta);
        add(Cedente.ContaDigito);
        add(Cedente.Agencia);
        add(Cedente.AgenciaDigito);
        add(GetEnumName( TypeInfo(TACBrPessoaCedente), Integer(Cedente.TipoInscricao) ));
        for I:= 0 to ListadeBoletos.Count - 1 do
        begin
          add(ListadeBoletos[I].Sacado.NomeSacado);
          add(ListadeBoletos[I].Sacado.CNPJCPF);
          add(DateToStr(ListadeBoletos[I].Vencimento));
          add(DateToStr(ListadeBoletos[I].DataDocumento));
          add(ListadeBoletos[I].NumeroDocumento);
          add(ListadeBoletos[I].NossoNumero);
          add(ListadeBoletos[I].Carteira);
          add(ListadeBoletos[I].CodigoLiquidacao);
          add(ListadeBoletos[I].CodigoLiquidacaoDescricao);
          add(inttostr(ListadeBoletos[I].Liquidacao.Banco));
          add(ListadeBoletos[I].Liquidacao.Agencia);
          add(ListadeBoletos[I].Liquidacao.Origem);
          add(ListadeBoletos[I].Liquidacao.FormaPagto);
          add(FloatToStr(ListadeBoletos[I].ValorDocumento));
          add(DateToStr(ListadeBoletos[I].DataOcorrencia));
          add(DateToStr(ListadeBoletos[I].DataCredito));
          add(DateToStr(ListadeBoletos[I].DataBaixa));
          add(FloatToStr(ListadeBoletos[I].ValorDespesaCobranca));
          add(FloatToStr(ListadeBoletos[I].ValorAbatimento));
          add(FloatToStr(ListadeBoletos[I].ValorDesconto));
          add(FloatToStr(ListadeBoletos[I].ValorMoraJuros));
          add(FloatToStr(ListadeBoletos[I].ValorIOF));
          add(FloatToStr(ListadeBoletos[I].ValorOutrasDespesas));
          add(FloatToStr(ListadeBoletos[I].ValorOutrosCreditos));
          add(FloatToStr(ListadeBoletos[I].ValorRecebido));
          add(FloatToStr(ListadeBoletos[I].ValorPago));
          add(ListadeBoletos[I].SeuNumero);
          add(GetEnumName( TypeInfo(TACBrTipoOcorrencia),
                       Integer(ListadeBoletos[I].OcorrenciaOriginal.Tipo)));
          add(ListadeBoletos[I].OcorrenciaOriginal.Descricao);

          for J:= 0 to ListadeBoletos[I].DescricaoMotivoRejeicaoComando.Count-1 do
          begin
             add(ListadeBoletos[I].MotivoRejeicaoComando[J]);
             add(ListadeBoletos[I].DescricaoMotivoRejeicaoComando[J]);
          end;

        end;
      end;

      SLRetorno.SaveToFile( NomeArq );
      Result:= NomeArquivo;
    end;

  finally
    FreeAndNil(SLRetorno);
  end;
end;

 {Carrega Arquivos Conforme Parametros do Diretório e Nome do Arquivo}
function CarregarArquivos(NomeArquivo: String;
  Diretorio: TTipoArquivo):String;
var
  tpArq : String;
begin
   Result := '';
   case Diretorio of
     Remessa : tpArq := 'Remessa\';
     Retorno : tpArq := 'Retorno\';
   end;

   if not FilesExists( ExtractFilePath(Application.ExeName) + tpArq + NomeArquivo ) then
      Raise Exception.Create( ACBrStr('Arquivo para Validação não Encontrado em: ' +
                            ExtractFilePath(Application.ExeName) + tpArq + NomeArquivo   ) )
   else
     Result := ExtractFilePath(Application.ExeName) + tpArq + NomeArquivo;

end;

function AlteraDataArquivo(Str, Valor: String; Posicao: Integer): String;
begin
  Delete(str, posicao, Length(Valor) );
  Insert(Valor, Str, Posicao);
  Result:= Str;
end;

procedure GerarRemessa_BB_Test.GerarRemessa_BancoBrasil400;
var
  lArqGerado, lArqValido: TStringList;
begin
  try

    lArqGerado := TStringList.Create;
    lArqValido := TStringList.Create;

    CriaBoletoPadrao('RemessaBB400.rem', Remessa, c400, cobBancoDoBrasil);

    //Carrega Arquivo Gerado
    lArqGerado.LoadFromFile( CarregarArquivos('RemessaBB400.rem', Remessa) );

    //Altera a Data Atual gerada pela Data do Arquivo Válido
    lArqGerado.Text:= StringReplace(lArqGerado.Text,
                      FormatDateTime('ddmmyy',now),'291119',[rfReplaceAll]);

    //Carrega Arquivo Válido
    lArqValido.LoadFromFile( CarregarArquivos('RemessaBB400Valido.rem', Remessa) );

    CheckEquals(lArqValido.Text, lArqGerado.Text);

  finally
    FreeAndNil(lArqValido);
    FreeAndNil(lArqGerado);
  end;

end;

procedure GerarRemessa_BB_Test.GerarRemessa_BancoBrasil240;
var
  lArqGerado, lArqValido: TStringList;
begin
  try

    lArqGerado := TStringList.Create;
    lArqValido := TStringList.Create;

    CriaBoletoPadrao('RemessaBB240.rem', Remessa, c240, cobBancoDoBrasil);

    //Carrega Arquivo Gerado
    lArqGerado.LoadFromFile( CarregarArquivos('RemessaBB240.rem', Remessa) );

    //Altera a Data Atual gerada pela Data do Arquivo Válido
    lArqGerado.Strings[0]:= AlteraDataArquivo(lArqGerado.Strings[0], '29112019000000', 144);

    lArqGerado.Text:= StringReplace(lArqGerado.Text,
                      FormatDateTime('ddmmyyyy',now),'29112019',[rfReplaceAll]);

    //Carrega Arquivo Válido
    lArqValido.LoadFromFile( CarregarArquivos('RemessaBB240Valido.rem', Remessa) );

    CheckEquals(lArqValido.Text, lArqGerado.Text);

  finally
    FreeAndNil(lArqValido);
    FreeAndNil(lArqGerado);
  end;

end;

{ GerarRetorno_BB_Test }

procedure GerarRetorno_BB_Test.GerarRetorno_BB400;
var
  SLArqGerado, SLArqValido: TStringList;
begin
  try
    SLArqGerado := TStringList.Create;
    SLArqValido := TStringList.Create;

    //Cria Componente boleto padrão apenas dados Banco e Cedente e Lê o retorno
    CriaBoletoPadrao( 'RetBB400.ret', Retorno, c400, cobBancoDoBrasil );

    //Carrega Arquivo de Retorno Gerado para comparar dados
    SLArqGerado.LoadFromFile( CarregarArquivos('ResultRetBB400.ret', Retorno) );

    //Carrega Arquivo Válido
    SLArqValido.LoadFromFile( CarregarArquivos('ResultRetBB400Valido.ret', Retorno) );

    CheckEquals( SLArqValido.Text, SLArqGerado.Text );

  finally
    FreeAndNil(SLArqValido);
    FreeAndNil(SLArqGerado);
  end;
end;

procedure GerarRetorno_BB_Test.GerarRetorno_BB240;
begin

end;


{ MontarCampoCarteira_BB_Test }

procedure MontarCampoCarteira_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoCarteira_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoCarteira_BB_Test.MontarCampoCarteira_BancoBrasil_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Titulo.Carteira             := '16';
  CheckEquals('16', Boleto.Banco.MontarCampoCarteira(Titulo));
end;

procedure MontarCampoCarteira_BB_Test.MontarCampoCarteira_BancoBrasil_Modalidade;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Modalidade   := '21';
  Titulo.Carteira             := '16';
  CheckEquals('16/21', Boleto.Banco.MontarCampoCarteira(Titulo));
end;

{ MontarCodigoBarras_BB_Test }

procedure MontarCodigoBarras_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCodigoBarras_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_cod17;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '0452148';
  Boleto.Cedente.Convenio     := '123456';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.60;
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '00345678901234567';
  CheckEquals('00193720600000100601234560034567890123456721', Boleto.Banco.MontarCodigoBarras(Titulo));

  Titulo.Carteira             := '18';
  CheckEquals('00193720600000100601234560034567890123456721', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_cod11;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '0452148';
  Boleto.Cedente.Convenio     := '1234';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.60;
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '1234567';
  CheckEquals('00191720600000100601234123456723510045214816', Boleto.Banco.MontarCodigoBarras(Titulo));

  Boleto.Cedente.Agencia      := '';
  Boleto.Cedente.Conta        := '';
  Titulo.Carteira             := '21';
  CheckEquals('00198720600000100601234123456700000000000021', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '045214X';
  Boleto.Cedente.Convenio     := '1234';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.60;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '12ABC34567X';
  CheckEquals('001997206000001006012341234567235100045214RG', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_CodIncompleto;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '045214';
  Boleto.Cedente.Convenio     := '1234';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100;
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '11512';
  CheckEquals('001987206000001000012340011512235100045214RG', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_DataNula;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '045214';
  Boleto.Cedente.Convenio     := '1234';
  Titulo.ValorDocumento       := 100;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1';
  CheckEquals('00192000000000100001234000000123510004521418', Boleto.Banco.MontarCodigoBarras(Titulo));
end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_valorZero;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '045214';
  Boleto.Cedente.Convenio     := '1234';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.Carteira             := '28';
  Titulo.NossoNumero          := '45211';
  CheckEquals('00195720600000000001234004521123510004521428', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_SemConvenio;
const
  MSG = 'Banco do Brasil requer que o Convênio do Cedente seja informado.';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Banco.Numero         := 1;
    Boleto.Cedente.Agencia      := '2351';
    Boleto.Cedente.Conta        := '0452148';
    Boleto.Cedente.Convenio     := '';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 100.00;
    Titulo.Carteira             := '16';
    Titulo.NossoNumero          := '1234567';
    Boleto.Banco.MontarCodigoBarras(Titulo);

  except on e: Exception do
    CheckEquals(MSG,e.Message);
  end;

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_SemCarteira;
const
  MSG = 'Banco do Brasil requer que a carteira seja informada antes do Nosso Número.';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Banco.Numero         := 1;
    Boleto.Cedente.Agencia      := '2351';
    Boleto.Cedente.Conta        := '0452148';
    Boleto.Cedente.Convenio     := '4521';
    Titulo.Vencimento           := EncodeDate(2017,06,30);
    Titulo.ValorDocumento       := 100.00;
    Titulo.Carteira             := '';
    Titulo.NossoNumero          := '1234567';
    Boleto.Banco.MontarCodigoBarras(Titulo);

  except on e: Exception do
    CheckEquals(MSG, e.Message);
  end;

end;

procedure MontarCodigoBarras_BB_Test.MontarCodigoBarras_BancoBrasil_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Banco.Numero         := 1;
  Boleto.Cedente.Agencia      := '2351';
  Boleto.Cedente.Conta        := '0452148';
  Boleto.Cedente.Convenio     := '4521';
  Titulo.Vencimento           := EncodeDate(2017,06,30);
  Titulo.ValorDocumento       := 100.00;
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '';
  CheckEquals('00191720600000100004521000000023510045214818', Boleto.Banco.MontarCodigoBarras(Titulo));

end;

{ MontarCampoNossoNumero_BB_Test }

procedure MontarCampoNossoNumero_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;

end;

procedure MontarCampoNossoNumero_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_17Digitos;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '123456';
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '00345678901234567';
  CheckEquals('00345678901234567', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '18';
  CheckEquals('00345678901234567', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := 'RG';
  Boleto.Cedente.Convenio     := '1234567';
  CheckEquals('12345678901234567', Boleto.Banco.MontarCampoNossoNumero(Titulo));

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_11Digitos;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '1233';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1234567';
  CheckEquals('12331234567-X', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Boleto.Cedente.Convenio     := '12335';
  CheckEquals('01233512345-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_SemConvenio;
const
  MSG = 'Banco do Brasil requer que o Convênio do Cedente seja informado.';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '';
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '1254213';
    Boleto.Banco.MontarCampoNossoNumero(Titulo);

  except on e: Exception do
    CheckEquals( MSG, e.Message );

  end;

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_SemCarteira;
const
  MSG = 'Banco do Brasil requer que a carteira seja informada antes do Nosso Número.';
begin
    try
      Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
      Boleto.Cedente.Convenio     := '55414';
      Titulo.Carteira             := '';
      Titulo.NossoNumero          := '1254213';
      Boleto.Banco.MontarCampoNossoNumero(Titulo);

    except on e: Exception do
      CheckEquals( MSG, e.Message );

    end;
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_SemNossoNumero;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '14526';
  Titulo.Carteira             := '0';
  Titulo.NossoNumero          := '';
  CheckEquals('01452600000-7', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '1452X';
  Titulo.Carteira             := '0';
  Titulo.NossoNumero          := '54214X';
  CheckEquals('01452X54214-2', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_NossoNumeroInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 5';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '1452X';
    Titulo.Carteira             := 'RG';
    Titulo.NossoNumero          := '5421X55244241555';
    Boleto.Banco.MontarCampoNossoNumero(Titulo);

  except on e: Exception do
    CheckEquals( MSG, e.Message );

  end;
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_ConvenioInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '145244142555214471';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '554225';

  CheckEquals('0000554225-1', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_CarteiraInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '14526';
  Titulo.Carteira             := 'Zz';
  Titulo.NossoNumero          := '54225';

  CheckEquals('01452654225-X', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_ConvenioAte4Dig;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '525';
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '5415256';
  CheckEquals('05255415256-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_Convenio5Dig;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '52585';
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '54151';
  CheckEquals('05258554151-8', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_Convenio6Dig;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '521475';
  Titulo.Carteira             := '16';
  Titulo.NossoNumero          := '25415214526';
  CheckEquals('00000025415214526', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '25851414599';
  CheckEquals('00000025851414599', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '12';
  Titulo.NossoNumero          := '25851';
  CheckEquals('52147525851-1', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '15';
  Titulo.NossoNumero          := '25858';
  CheckEquals('52147525858-9', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '17';
  Titulo.NossoNumero          := '25859';
  CheckEquals('52147525859-7', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '85451';
  CheckEquals('52147585451-3', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '87411478714';
  CheckEquals('52147587411-5', Boleto.Banco.MontarCampoNossoNumero(Titulo));

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_Convenio7Dig;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '5214758';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '15415214523';
  CheckEquals('52147585415214523', Boleto.Banco.MontarCampoNossoNumero(Titulo));

  Titulo.Carteira             := '12';
  Titulo.NossoNumero          := '25857';
  CheckEquals('52147580000025857', Boleto.Banco.MontarCampoNossoNumero(Titulo));

end;

procedure MontarCampoNossoNumero_BB_Test.MontarCampoNossoNumero_BancoBrasil_ConvenioMaior7Dig;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '85215214758';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '4115415';
  CheckEquals('0004115415-0', Boleto.Banco.MontarCampoNossoNumero(Titulo));
end;

{ MontarCampoCodigoCedente_BB_Test }

procedure MontarCampoCodigoCedente_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure MontarCampoCodigoCedente_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure MontarCampoCodigoCedente_BB_Test.MontarCampoCodigoCedente_BancoBrasil_Padrao;
begin
  Boleto.Banco.TipoCobranca      := cobBancoDoBrasil;
  Boleto.Cedente.Agencia         := '2541';
  Boleto.Cedente.AgenciaDigito   := '1';
  Titulo.ACBrBoleto.Cedente.Conta:= '0523218';
  Boleto.Cedente.ContaDigito     := '4';

  CheckEquals('2541-1/523218-4', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_BB_Test.MontarCampoCodigoCedente_BancoBrasil_Invalido;
begin
  Boleto.Banco.TipoCobranca      := cobBancoDoBrasil;
  Boleto.Cedente.Agencia         := '5214';
  Boleto.Cedente.AgenciaDigito   := '1';
  Titulo.ACBrBoleto.Cedente.Conta:= '0523218';
  Boleto.Cedente.ContaDigito     := '4';

  CheckNotEquals('5214-1/0523218-4', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

procedure MontarCampoCodigoCedente_BB_Test.MontarCampoCodigoCedente_BancoBrasil_Alfa;
begin
  Boleto.Banco.TipoCobranca      := cobBancoDoBrasil;
  Boleto.Cedente.Agencia         := '5214';
  Boleto.Cedente.AgenciaDigito   := '1';
  Titulo.ACBrBoleto.Cedente.Conta:= '0523218';
  Boleto.Cedente.ContaDigito     := 'X';

  CheckEquals('5214-1/523218-X', Boleto.Banco.MontarCampoCodigoCedente(Titulo));
end;

{ CalcularDigitoVerificador_BB_Test }

procedure CalcularDigitoVerificador_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularDigitoVerificador_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '12345';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '12345';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_X;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '1233';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1234567';

  CheckEquals('X', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_0;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '8547125';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_1;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '521475';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '25851';

  CheckEquals('1', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_2;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185551';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '84145';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_3;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '521475';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '85451';

  CheckEquals('3', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_4;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185444';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '90257919';

  CheckEquals('4', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_5;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185444';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '50257112';

  CheckEquals('5', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_6;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185444';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '7016252';

  CheckEquals('6', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_7;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '0';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '2541572';

  CheckEquals('7', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_8;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185551';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '8114145';

  CheckEquals('8', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_9;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '185444';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '00257478';

  CheckEquals('9', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '0C12345A';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '1B0055D';

  CheckEquals('2', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_Nulo;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '0C12345A';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '';

  CheckEquals('0', Boleto.Banco.CalcularDigitoVerificador(Titulo));
end;

procedure CalcularDigitoVerificador_BB_Test.CalcularDigitoVerificador_BancoBrasil_NumInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 10';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '000000000000000';
    Titulo.Carteira             := 'XX';
    Titulo.NossoNumero          := '323232322323232323232232';
    Boleto.Banco.CalcularDigitoVerificador(Titulo);

  except on e: Exception do
     CheckEquals( MSG,e.Message );

  end;

end;

{ CalcularTamMaximoNossoNumero_BB_Test }

procedure CalcularTamMaximoNossoNumero_BB_Test.SetUp;
begin
  Boleto := TACBrBoleto.Create(nil);
  Titulo := Boleto.CriarTituloNaLista;
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.TearDown;
begin
  FreeAndNil(Boleto);
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Tam5;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '65465';
  Titulo.Carteira             := '12';
  Titulo.NossoNumero          := '12312';
  CheckEquals(5, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));

  Titulo.Carteira             := '15';
  CheckEquals(5, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));

  Titulo.Carteira             := '17';
  CheckEquals(5, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));

  Titulo.Carteira             := '18';
  CheckEquals(5, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Tam7;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231234';

  CheckEquals(7, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Tam10;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '6548549';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '12312342';

  CheckEquals(10, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Tam11;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654854';
  Titulo.Carteira             := 'RG';
  Titulo.NossoNumero          := '12312342';

  CheckEquals(11, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Tam17;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '6543217';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '123456789101';
  CheckEquals(17, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));

  Boleto.Cedente.Convenio     := '654321';
  Titulo.Carteira             := '16';
  CheckEquals(17, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));

  Boleto.Cedente.Convenio     := '654545';
  Titulo.Carteira             := '18';
  CheckEquals(17, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Padrao;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654155155';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '5214123122';

  CheckEquals(10, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                               Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Alfa;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '1231RG234';

  CheckEquals(7, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_Nulo;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654';
  Titulo.Carteira             := '18';
  Titulo.NossoNumero          := '';

  CheckEquals(7, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                             Boleto.Cedente.Convenio));
end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_CarteiraNulo;
const
  MSG = 'Banco do Brasil requer que a carteira seja informada antes do Nosso Número.';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '654';
    Titulo.Carteira             := '';
    Titulo.NossoNumero          := '1231234';
    Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                               Boleto.Cedente.Convenio);
  except on e: exception do
      CheckEquals(MSG, e.Message);

  end;

end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_ConvenioNulo;
const
  MSG = 'Banco do Brasil requer que o Convênio do Cedente seja informado.';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '';
    Titulo.Carteira             := '18';
    Titulo.NossoNumero          := '1231234';
    Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                               Boleto.Cedente.Convenio);
  except on e: exception do
    CheckEquals(MSG, e.Message);

  end;

end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_NossoNumInvalido;
const
  MSG = 'Tamanho Máximo do Nosso Número é: 7';
begin
  try
    Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
    Boleto.Cedente.Convenio     := '654';
    Titulo.Carteira             := '18';
    Titulo.NossoNumero          := '45644444522541156633252';
    Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                               Boleto.Cedente.Convenio);

  except on e: exception do
    CheckEquals(MSG, e.Message);

  end;

end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_CarteiraInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '654';
  Titulo.Carteira             := 'zZ';
  Titulo.NossoNumero          := '4414452';

  CheckEquals(7, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                            Boleto.Cedente.Convenio));

end;

procedure CalcularTamMaximoNossoNumero_BB_Test.CalcularTamMaximoNossoNumero_BancoBrasil_ConvenioInvalido;
begin
  Boleto.Banco.TipoCobranca   := cobBancoDoBrasil;
  Boleto.Cedente.Convenio     := '65456252552400';
  Titulo.Carteira             := 'zZzzz';
  Titulo.NossoNumero          := '84414';

  CheckEquals(10, Boleto.Banco.CalcularTamMaximoNossoNumero( Titulo.Carteira, Titulo.NossoNumero,
                                                            Boleto.Cedente.Convenio));
end;

{$ENDREGION}


initialization

  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularDigitoVerificador_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularTamMaximoNossoNumero_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoCodigoCedente_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoNossoNumero_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCodigoBarras_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoCarteira_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRemessa_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRetorno_BB_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularDigitoVerificador_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularTamMaximoNossoNumero_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoCodigoCedente_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoNossoNumero_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCodigoBarras_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularDVCedente_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRemessa_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRetorno_Caixa_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularDigitoVerificador_Bradesco_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularTamMaximoNossoNumero_Bradesco_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoCodigoCedente_Bradesco_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoNossoNumero_Bradesco_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCodigoBarras_Bradesco_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularDigitoVerificador_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', CalcularTamMaximoNossoNumero_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoCodigoCedente_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoNossoNumero_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCodigoBarras_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRemessa_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', GerarRetorno_Unicred_Test{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrBoleto.ACBrBoleto', MontarCampoNossoNumero_UnicredES_Test{$ifndef FPC}.Suite{$endif});


end.

