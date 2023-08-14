{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBancoCecred;
//Banco passou por reestruturação de marca agora chama Ailos

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

const
  CACBrBancoCecred_Versao = '0.0.1';

type
  { TACBrBancoCecred}

  TACBrBancoCecred = class(TACBrBancoClass)
  private
    fValorTotalDocs : Double;
    fDataProtestoNegativacao : TDateTime;
    fDiasProtestoNegativacao : String;
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
  protected
    fCountRegR:Integer;
    procedure DefineDataProtestoNegativacao(const ACBrTitulo: TACBrTitulo);
    function DefineCodigoProtesto(const ACBrTitulo: TACBrTitulo): String; override;
    procedure EhObrigatorioContaDV; override;
    function GetLocalPagamento: String; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;

    function GerarRegistroHeader240(NumeroRemessa : Integer): String;    override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;

    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;

    function GerarRegistroTrailler240(ARemessa:TStringList): String;  override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    procedure LerRetorno240(ARetorno:TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function CodMotivoRejeicaoToDescricao(
      const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;

    function CalcularTamMaximoNossoNumero(const Carteira : String; const NossoNumero : String = ''; const Convenio: String = ''): Integer; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;

    property DataProtestoNegativacao : TDateTime read  fDataProtestoNegativacao ;
    property DiasProtestoNegativacao : String read fDiasProtestoNegativacao ;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  StrUtils, Variants, ACBrValidador, ACBrUtil.Base, Math, ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrBancoCecred.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fDataProtestoNegativacao:=0;
   fDiasProtestoNegativacao:='';
   fpDigito                := 1;
   fpNome                  := 'Banco Cecred';
   fpNumero                := 085;
   fpTamanhoMaximoNossoNum := 17;
   fpTamanhoConta          := 8;
   fpTamanhoAgencia        := 4;
   fpTamanhoCarteira       := 2;
   fValorTotalDocs         := 0;
   fpLayoutVersaoArquivo   := 87;
   fpLayoutVersaoLote      := 45;

end;

function TACBrBancoCecred.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
begin
   Result := '0';

   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorInicial := 9;
   Modulo.Documento := FormataNossoNumero(ACBrTitulo);
   Modulo.Calcular;

   if Modulo.ModuloFinal >= 10 then
      Result:= '1'
   else
      Result:= IntToStr(Modulo.ModuloFinal);
end;

function TACBrBancoCecred.CalcularTamMaximoNossoNumero(
  const Carteira: String; const NossoNumero : String = ''; const Convenio: String = ''): Integer;
begin
   Result := 17;

   if (ACBrBanco.ACBrBoleto.Cedente.Conta = '') then
      raise Exception.Create(ACBrStr('Banco Cecred requer que a Conta '+
                                     'seja informada.'));

end;

function TACBrBancoCecred.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero : String;
  AConta : String;
begin


   with ACBrTitulo do
   begin


      AConta    := IntToStr(StrToInt64(OnlyNumber(ACBrBoleto.Cedente.Conta + ACBrBoleto.Cedente.ContaDigito)));
      ANossoNumero := IntToStr(StrToInt64(OnlyNumber(NossoNumero)));

      ANossoNumero := PadLeft(AConta, 8, '0') + PadLeft(ANossoNumero, 9, '0')
   end;
   Result := ANossoNumero;
end;

procedure TACBrBancoCecred.DefineDataProtestoNegativacao(
  const ACBrTitulo: TACBrTitulo);
var
  ACodProtesto: String;
begin

  with ACBrTitulo do
  begin
    ACodProtesto :=  DefineCodigoProtesto(ACBrTitulo);
    if ACodProtesto = '1' then
    begin
      fDataProtestoNegativacao := DataProtesto;
      fDiasProtestoNegativacao := IntToStr(DiasDeProtesto);
    end
    else
    if ACodProtesto = '2' then
    begin
      fDataProtestoNegativacao := DataNegativacao;
      fDiasProtestoNegativacao := IntToStr(DiasDeNegativacao);
    end
    else
    begin
      fDataProtestoNegativacao := 0;
      fDiasProtestoNegativacao := '3';
    end;

  end;

end;

procedure TACBrBancoCecred.EhObrigatorioContaDV;
begin
  //validação
end;

function TACBrBancoCecred.DefineCodigoProtesto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case CodigoNegativacao of
       cnProtestarCorrido, cnProtestarUteis : Result := '1';
       cnNegativar                          : Result := '2';
       cnNaoProtestar,cnNaoNegativar        : Result := '3';
      else
       Result := '3';
    end;

  end;
end;


function TACBrBancoCecred.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras: String;
  FatorVencimento: String;
  DigitoCodBarras: String;
  ANossoNumero: String;
  AConvenio: String;
begin
   AConvenio    := IntToStr(StrToInt64Def(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Convenio),0));
   ANossoNumero := IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.NossoNumero),0),9);

   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
      CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                      '9' +
                      FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                      PadLeft(AConvenio, 6, '0') +
                      IntToStrZero(StrToIntDef(OnlyNumber(Cedente.Conta + Cedente.ContaDigito),0),8) +
                      ANossoNumero +
                      PadRight(ACBrTitulo.Carteira, fpTamanhoCarteira, '0');

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44) ;
end;

function TACBrBancoCecred.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
             +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
             +'/'+
             IntToStr(StrToInt64Def(ACBrTitulo.ACBrBoleto.Cedente.Conta,0))
             +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoCecred.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var
  ANossoNumero: String;
begin
   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   Result:= ANossoNumero

end;

function TACBrBancoCecred.GerarRegistroHeader240(
  NumeroRemessa: Integer): String;
var
  ATipoInscricao, aConta, aAgencia: String;

begin
  fCountRegR := 0;

  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case TipoInscricao of
      pFisica  : ATipoInscricao := '1';
      pJuridica: ATipoInscricao := '2';
    else
      ATipoInscricao := '1';
    end;

    aAgencia    := PadLeft(OnlyNumber(Agencia), 5, '0');
    aConta      := PadLeft(OnlyNumber(Conta), 12, '0');

    // GERAR REGISTRO-HEADER DO ARQUIVO
    Result:= IntToStrZero(ACBrBanco.Numero, 3)               + // 1 a 3 - Código da cooperativa
             '0000'                                          + // 4 a 7 - Lote de serviço
             '0'                                             + // 8 - Tipo de registro - Registro header de arquivo
             StringOfChar(' ', 9)                            + // 9 a 17 Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                                  + // 18 - Tipo de inscrição do cedente
             PadLeft(OnlyNumber(CNPJCPF), 14, '0')           + // 19 a 32 -Número de inscrição do cedente
             PadRight(Convenio, 20, ' ')                     + // 33 a 52 - Código do convênio
             aAgencia                                        + // 53 a 57 - Código da agência do cedente
             PadRight(AgenciaDigito, 1 , '0')                + // 58 - Dígito da agência do cedente
             aConta                                          + // 59 a 70 - Número da conta do cedente
             PadRight(ContaDigito, 1, '0')                   + // 71 - Dígito da conta do cedente
             PadRight(DigitoVerificadorAgenciaConta, 1, ' ') + // 72 - Dígito verificador da agência / conta
             TiraAcentos(UpperCase(PadRight(Nome, 30, ' '))) + // 73 a 102 - Nome do cedente
             PadRight('CEDRED', 30, ' ')                     + // 103 a 132 - Nome da cooperativa
             StringOfChar(' ', 10)                           + // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
             '1'                                             + // 143 - Código de Remessa (1) / Retorno (2)
             FormatDateTime('ddmmyyyy', Now)                 + // 144 a 151 - Data do de geração do arquivo
             FormatDateTime('hhmmss', Now)                   + // 152 a 157 - Hora de geração do arquivo
             PadLeft(IntToStr(NumeroRemessa), 6, '0')        + // 158 a 163 - Número seqüencial do arquivo
             PadLeft(IntToStr(fpLayoutVersaoArquivo) , 3, '0')+ // 164 a 166 - Número da versão do layout do arquivo
             StringOfChar('0', 5)                            + // 167 a 171 - Densidade de gravação do arquivo (BPI)
             StringOfChar(' ', 20)                           + // 172 a 191 - Uso reservado da cooperativa
             StringOfChar(' ', 20)                           + // 192 a 211 - Uso reservado da empresa
             StringOfChar(' ', 29);                            // 212 a 240 - Uso exclusivo FEBRABAN/CNAB

    // GERAR REGISTRO HEADER DO LOTE
    Result:= Result + #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)               + // 1 a 3 - Código da cooperativa
             '0001'                                          + // 4 a 7 - Lote de serviço
             '1'                                             + // 8 - Tipo de registro - Registro Header de Lote
             'R'                                             + // 9 - Tipo de operação: R (Remessa) ou T (Retorno)
             '01'                                            + // 10 a 11 - Tipo de serviço: 01 (Cobrança)
             '  '                                            + // 12 a 13 - Uso exclusivo FEBRABAN/CNAB
             PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0')   + // 14 a 16 - Número da versão do layout do lote
             ' '                                             + // 17 - Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                                  + // 18 - Tipo de inscrição do cedente
             PadLeft(OnlyNumber(CNPJCPF), 15, '0')           + // 19 a 33 -Número de inscrição do cedente
             PadRight(Convenio, 20, ' ')                     + // 34 a 43 - Código do convênio
             aAgencia                                        + // 54 a 58 - Código da agência do cedente
             PadRight(AgenciaDigito, 1 , '0')                + // 59 - Dígito da agência do cedente
             aConta                                          + // 60 a 71 - Número da conta do cedente
             PadRight(ContaDigito, 1, '0')                   + // 72 - Dígito da conta do cedente
             PadRight(DigitoVerificadorAgenciaConta, 1, ' ') + // 73 - Dígito verificador da agência / conta
             PadRight(Nome, 30, ' ')                         + // 74 a 103 - Nome do cedente
             StringOfChar(' ', 40)                           + // 104 a 143 - Mensagem 1 para todos os boletos do lote
             StringOfChar(' ', 40)                           + // 144 a 183 - Mensagem 2 para todos os boletos do lote
             PadLeft(IntToStr(NumeroRemessa), 8, '0')        + // 184 a 191 - Número do arquivo
             FormatDateTime('ddmmyyyy', Now)                 + // 192 a 199 - Data de geração do arquivo
             StringOfChar('0', 8)                            + // 200 a 207 - Data do crédito - Só para arquivo retorno
             StringOfChar(' ', 33);                            // 208 a 240 - Uso exclusivo FEBRABAN/CNAB
  end;
end;


procedure TACBrBancoCecred.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa:TStringList);
var
  aAgencia: String;
  aConta: String;
  aConvenio : String;
  wLinha: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      aAgencia:= IntToStrZero(StrToIntDef(OnlyNumber(Agencia),0),4);
      aConta  := IntToStrZero(StrToIntDef(OnlyNumber(Conta),0),8);
      aConvenio := IntToStrZero(StrToIntDef(OnlyNumber(Convenio),0),7);

      wLinha:= '0'                            + // ID do Registro
               '1'                            + // ID do Arquivo( 1 - Remessa)
               'REMESSA'                      + // Literal de Remessa
               '01'                           + // Código do Tipo de Serviço
               'COBRANCA'                     + // Descrição do tipo de serviço
               Space(7)                       + // Brancos
               aAgencia                       + // Prefixo da agência/ onde esta cadastrado o convenente lider do cedente
               PadRight( AgenciaDigito, 1, ' ')   + // DV-prefixo da agência
               aConta                         + // Codigo do cedente/nr. da conta corrente que está cadastro o convenio lider do cedente
               PadRight( ContaDigito, 1, ' ')     + // DV-código do cedente
               '000000'                       + // Complemento
               PadRight( Nome, 30)                + // Nome da Empresa
               PadRight( '085CECRED',18,' ')      + // Identificador do Banco
               FormatDateTime('ddmmyy',Now)   + // Data de geração do arquivo
               IntToStrZero(NumeroRemessa,7)  + // Numero Remessa
               Space(22)                      + // Brancos
               aConvenio                      + // Nr. Convênio
               space(258)                     + // Brancos
               IntToStrZero(1,6);               // Nr. Sequencial do registro-informar 000001

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

function TACBrBancoCecred.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
var
  ATipoOcorrencia, ATipoBoleto : String;
  ADataMoraJuros, ADataDesconto: String;
  ANossoNumero, ATipoAceite    : String;
  aAgencia, aConta             : String;
  aCodJuros, aCodDesc          : String;
  aCodMulta, aValorMulta       : String;
  ACodigoNegativacao           : String;
  ACaracTitulo  : Char;
begin
  with ACBrTitulo do
  begin
    ANossoNumero := FormataNossoNumero(ACBrTitulo);
    aAgencia     := PadLeft(ACBrBoleto.Cedente.Agencia, 5, '0');
    aConta       := PadLeft(ACBrBoleto.Cedente.Conta, 12, '0');

    // Zerando o valor/percentual de multa, se tiver será calculado depois
    aValorMulta  := PadRight('', 15, '0');

   //Pegando o Tipo de Ocorrencia
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                         : ATipoOcorrencia := '02';   // Pedido de Baixa
      toRemessaConcederAbatimento             : ATipoOcorrencia := '04';   // Concessão de Abatimento
      toRemessaCancelarAbatimento             : ATipoOcorrencia := '05';   // Cancelamento de Abatimento
      toRemessaAlterarVencimento              : ATipoOcorrencia := '06';   // Alteração de Vencimento
      toRemessaConcederDesconto               : ATipoOcorrencia := '07';   // Concessão de Desconto
      toRemessaCancelarDesconto               : ATipoOcorrencia := '08';   // Cancelamento de Desconto
      toRemessaProtestar                      : ATipoOcorrencia := '09';   // Protesto
      toRemessaCancelarInstrucaoProtestoBaixa : ATipoOcorrencia := '10';   // Sustar Protesto e Baixar Título
      toRemessaCancelarInstrucaoProtesto      : ATipoOcorrencia := '11';   // Sustar Protesto e Manter em Carteira
      toRemessaAlterarNomeEnderecoSacado      : ATipoOcorrencia := '31';   // Alteração de outros dados (Somente endereço do pagador)
                                                                           //‘41’ = Cancelar instrução automática de protesto
                                                                           //‘90’ = Alterar tipo de emissão - Cooperativa/EE
      toRemessaNegativacaoSerasa              : ATipoOcorrencia := '93';   // Inclusão Negativação via Serasa
      toRemessaExcluirNegativacaoSerasa       : ATipoOcorrencia := '94';   // Exclusão Negativação via Serasa
    else
      ATipoOcorrencia := '01';                                             // Entrada de Títulos
    end;

    // Pegando o tipo de EspecieDoc
    if EspecieDoc = 'DM' then
      EspecieDoc   := '02'
    else
      EspecieDoc   := '04'; //DS

    // Pegando o Aceite do Titulo
    case Aceite of
      atSim :  ATipoAceite := 'A';
      atNao :  ATipoAceite := 'N';
    else
      ATipoAceite := 'N';
    end;

    if CodigoMoraJuros = cjValorDia then
      aCodJuros := '1'
    else if CodigoMoraJuros = cjTaxaMensal then
      aCodJuros := '2'
    else
      aCodJuros := '3';// isento

    if CodigoDesconto = cdSemDesconto then
      aCodDesc := '0'
    else
      aCodDesc := '1'; // Valor Fixo Até a Data Informada

    {Código de Protesto/Negativação }
    ACodigoNegativacao := DefineCodigoProtesto(ACBrTitulo);
    DefineDataProtestoNegativacao(ACBrTitulo);

    if CodigoMulta = cmValorFixo then
    begin
      aCodMulta := '1';
      if PercentualMulta > 0 then
        aValorMulta := IntToStrZero(Round(ValorDocumento*(PercentualMulta/100)*100), 15);
    end
    else
    begin
      aCodMulta := '2'; //Percentual
      if PercentualMulta > 0 then
        aValorMulta := IntToStrZero(Round(PercentualMulta * 100), 15);
    end;

    //Pegando Tipo de Boleto
    case ACBrBoleto.Cedente.ResponEmissao of
      tbCliEmite        : ATipoBoleto := '2' + '2';
      tbBancoEmite      : ATipoBoleto := '1' + '1';
      tbBancoReemite    : ATipoBoleto := '4' + '1';
      tbBancoNaoReemite : ATipoBoleto := '5' + '2';
    end;

    ACaracTitulo := '1';
    case CaracTitulo of
      tcSimples     : ACaracTitulo  := '1';
      tcVinculada   : ACaracTitulo  := '2';
      tcCaucionada  : ACaracTitulo  := '3';
      tcDescontada  : ACaracTitulo  := '4';
    end;

    //Mora Juros
    if (ValorMoraJuros > 0) and (DataMoraJuros > 0) then
      ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
    else
    begin
      aCodJuros := '3';// isento ;
      ADataMoraJuros := PadRight('', 8, '0');
    end;

    //Descontos
    if (ValorDesconto > 0) and (DataDesconto > 0) then
      ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
    else
      ADataDesconto := PadRight('', 8, '0');

    fValorTotalDocs:= fValorTotalDocs  + ValorDocumento;

    //SEGMENTO P
    Result:= IntToStrZero(ACBrBanco.Numero, 3)                                         + // 1 a 3 - Código do banco
             '0001'                                                                    + // 4 a 7 - Lote de serviço
             '3'                                                                       + // 8 - Tipo do registro: Registro detalhe
             IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 1 , 5) + // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
             'P'                                                                       + // 14 - Código do segmento do registro detalhe
             ' '                                                                       + // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
             ATipoOcorrencia                                                           + // 16 a 17 - Código de movimento
             aAgencia                                                                  + // 18 a 22 - Agência mantenedora da conta
             PadRight(ACBrBoleto.Cedente.AgenciaDigito, 1 , '0')                       + // 23 -Dígito verificador da agência
             aConta                                                                    + // 24 a 35 - Número da conta corrente
             PadRight(ACBrBoleto.Cedente.ContaDigito, 1, '0')                          + // 36 - Dígito verificador da conta
             ' '                                                                       + // 37 - Dígito verificador da agência / conta
             PadRight(ANossoNumero, 20, ' ')                                           + // 38 a 57 - Identificação do título na cooperativa
             ACaracTitulo                                                              + // 58 - Codigo carteira fixo 1
             '1'                                                                       + // 59 - Forma de cadastramento do título no banco: com cadastramento fixo 1
             '1'                                                                       + // 60 - Tipo de documento: Tradicional fixo 1
             ATipoBoleto                                                               + // 61 a 62 - Quem emite e quem distribui o boleto?
             PadRight(NumeroDocumento, 15, ' ')                                        + // 63 a 77 - Número que identifica o título na empresa
             FormatDateTime('ddmmyyyy', Vencimento)                                    + // 78 a 85 - Data de vencimento do título
             IntToStrZero( round( ValorDocumento * 100), 15)                           + // 86 a 100 - Valor nominal do título
             '00000 '                                                                  + // 101 a 106 - Agência cobradora + dv
             PadRight(EspecieDoc, 2)                                                   + // 107 a 108 - Espécie do documento
             ATipoAceite                                                               + // 109 - Identificação de título Aceito / Não aceito
             FormatDateTime('ddmmyyyy', DataDocumento)                                 + // 110 a 117 - Data da emissão do documento
             aCodJuros                                                                 + // 118 - Código de juros de mora: Valor por dia   ('1' = Valor por Dia  '2' = Taxa Mensal '3' = Isento)
             ADataMoraJuros                                                            + // 119 a 126 - Data a partir da qual serão cobrados juros
             IfThen(ValorMoraJuros > 0,
                    IntToStrZero(round(ValorMoraJuros * 100), 15),
                    PadRight('', 15, '0'))                                             + // 127 a 141 - Valor de juros de mora por dia
             aCodDesc                                                                  + // 142 - Código de desconto: 1 - Valor fixo até a data informada  0 - Sem desconto
             IfThen(ValorDesconto > 0,
                    IfThen(DataDesconto > 0, ADataDesconto,'00000000'), '00000000')    + // 143 a 150 - Data do desconto
             IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 15),
                    PadRight('', 15, '0'))                                             + // 151 a 165 - Valor do desconto a ser concedido
             IntToStrZero( round(ValorIOF * 100), 15)                                  + // 166 a 180 - Valor do IOF a ser recolhido
             IntToStrZero( round(ValorAbatimento * 100), 15)                           + // 181 a 195 - Valor do abatimento
             PadRight(SeuNumero, 25, ' ')                                              + // 196 a 220 - Identificação do título na empresa
             ACodigoNegativacao                                                        + // 221 - Código para negativacao
             IfThen((DataProtestoNegativacao <> 0) and
                       (DataProtestoNegativacao > Vencimento),
                        PadLeft(DiasProtestoNegativacao , 2, '0'), '00')               + //222 a 223 - Prazo para protesto
             '2'                                                                       + // 224 - Codigo para Baixa/Devolucao
             StringOfChar(' ', 3)                                                      + // 225 a 227 - Número de dias para Baixa/Devolucao
             '09'                                                                      + // 228 a 229 - Código da moeda: Real
             StringOfChar('0', 10)                                                     + // 230 a 239 - Numero do contrato da op. de credito
             ' ';                                                                        // 240 - Uso exclusivo FEBRABAN/CNAB

    //SEGMENTO Q
    Result:= Result + #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)                                        + // 1 a 3 - Código do banco
             '0001'                                                                   + // 4 a 7 - Número do lote
             '3'                                                                      + // 8 - Tipo do registro: Registro detalhe
             IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 2 ,5) + // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
             'Q'                                                                      + // 14 - Código do segmento do registro detalhe
             ' '                                                                      + // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
             ATipoOcorrencia                                                          + // 16 a 17 - Codigo de movimento remessa
             IfThen(Sacado.Pessoa = pJuridica,'2','1')                                + // 18 - Tipo inscricao
             PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                             + // 19 a 33 - Número de Inscrição
             PadRight(Sacado.NomeSacado, 40, ' ')                                     + // 34 a 73 - Nome sacado
             PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' '+
                      Sacado.Complemento , 40, ' ')                                   + // 74 a 113 - Endereco sacado
             PadRight(Sacado.Bairro, 15, ' ')                                         + // 114 a 118 - Bairro sacado
             PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                  + // 129 a 136 - Cep sacado
             PadRight(Sacado.Cidade, 15, ' ')                                         + // 137 a 151 - Cidade sacado
             PadRight(Sacado.UF, 2, ' ')                                              + // 152 a 153 - Unidade da Federação
             '0'                                                                      + // 154 - Tipo de inscrição: Não informado
             StringOfChar('0', 15)                                                    + // 155 a 169 - Número de inscrição
             StringOfChar(' ', 40)                                                    + // 170 a 209 - Nome do sacador/avalista
             StringOfChar('0', 3)                                                     + // 210 a 212 - Cód. Bco. Corresp. na Compensação
             StringOfChar(' ', 20)                                                    + // 213 a 232 - Nosso N° no banco correspondente
             StringOfChar(' ', 8);                                                      // 233 a 240 - Uso exclusivo FEBRABAN/CNAB

    //SEGMENTO R
    if (PercentualMulta > 0) then
    begin
      Inc(fCountRegR);
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                       + // 1 a 3 - Código do banco
               '0001'                                                                  + // 4 a 7 - Número do lote
               '3'                                                                     + // 8 a 8 - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 3 ,5) + // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'R'                                                                     + // 14 a 14 - Código do segmento do registro detalhe
               ' '                                                                     + // 15 a 15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                         + // 16 a 17 - Tipo Ocorrencia
               '0'                                                                     + // 18 - Cód deconto 2
               StringOfChar('0', 8)                                                    + // 19 a 26 - Data do desconto 2
               StringOfChar('0', 15)                                                   + // 27 a 41 - Valor/Percentual de desconto 2
               '0'                                                                     + // 42 - Cód deconto 3
               StringOfChar('0', 8)                                                    + // 43 a 50 - Data do desconto 3
               StringOfChar('0', 15)                                                   + // 51 a 65 - Valor/Percentual de desconto 3
               aCodMulta                                                               + // 66 a 66 - Codigo Multa (1-Valor fixo / 2-Percentual)
               IfThen((PercentualMulta > 0),
                       FormatDateTime('ddmmyyyy', DataMoraJuros), '00000000')          + // 67 a 74 - Data Multa (se nao informar será a partir do vcto)
               aValorMulta                                                             + // 75 a 89 - valor/Percentual de multa dependando do cod da multa. Informar zeros se não cobrar
               StringOfChar(' ', 10)                                                   + // 90 a 99 - Informacao ao sacado
               StringOfChar(' ', 40)                                                   + // 100 a 139 - Mensagem 3
               StringOfChar(' ', 40)                                                   + // 140 a 179 - Mensagem 4
               StringOfChar(' ', 20)                                                   + // 180 a 199 - Uso exclusivo FEBRABAN/CNAB: Branco
               StringOfChar('0', 8)                                                    + // 200 - 207 - Cód ocor. do sacado
               StringOfChar('0', 3)                                                    + // 208 a 210 - Cód do Banco na Conta do Débito
               StringOfChar('0', 5)                                                    + // 211 a 215 - Cód da Agência do Débito
               ' '                                                                     + // 216 - Digito Conta Agência do Débito
               StringOfChar('0', 12)                                                   + // 217 a 228 - Conta Corrente para Débito
               ' '                                                                     + // 229 - Digito Conta Débito
               ' '                                                                     + // 230 - Digito ag/conta debito
               '0'                                                                     + // 231 - Aviso para débito automático
               StringOfChar(' ', 9);                                                     // 232 a 240 - Uso exclusivo FEBRABAN/CNAB: Branco
    end;
  end;
end;


procedure TACBrBancoCecred.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ANossoNumero: String;
  ATipoOcorrencia: String;
  AInstrucao: String;
  ATipoSacado: String;
  ATipoCendente: String;
  ATipoAceite: String;
  ATipoEspecieDoc: String;
  AMensagem: String;
  DiasProtesto: String;
  aAgencia: String;
  aConta: String;
  aModalidade: String;
  wLinha: String;
  aTipoCobranca: String;
begin

   with ACBrTitulo do
   begin
      ANossoNumero := FormataNossoNumero(ACBrTitulo);
      aAgencia:= IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia),0),4);
      aConta  := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta),0),8);
      aModalidade := IntToStrZero(StrToIntDef(trim(ACBrBoleto.Cedente.Modalidade),0),3);
       
      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : ATipoOcorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : ATipoOcorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : ATipoOcorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : ATipoOcorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarControleParticipante    : ATipoOcorrencia := '07'; {Alteração do número de controle do participante}
         toRemessaAlterarNumeroControle          : ATipoOcorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : ATipoOcorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : ATipoOcorrencia := '10'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : ATipoOcorrencia := '10'; {Sustar protesto e manter na carteira}
         toRemessaDispensarJuros                 : ATipoOcorrencia := '11'; {Instrução para dispensar juros}
         toRemessaAlterarNomeEnderecoSacado      : ATipoOcorrencia := '12'; {Alteração de nome e endereço do Sacado}
         toRemessaOutrasOcorrencias              : ATipoOcorrencia := '31'; {Conceder desconto}
         toRemessaCancelarDesconto               : ATipoOcorrencia := '32'; {Não conceder desconto}
      else
         ATipoOcorrencia := '01';                                           {Registro de títulos}
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      { Pegando o tipo de EspecieDoc }
      if EspecieDoc = 'DM' then
         ATipoEspecieDoc   := '01'
      else if EspecieDoc = 'NP' then
         ATipoEspecieDoc   := '02'
      else if EspecieDoc = 'NS' then
         ATipoEspecieDoc   := '03'
      else if EspecieDoc = 'RC' then
         ATipoEspecieDoc   := '05'
      else if EspecieDoc = 'LC' then 
         ATipoEspecieDoc   := '08'
      else if EspecieDoc = 'DS' then
         ATipoEspecieDoc   := '12'
      else if EspecieDoc = 'ND' then
         ATipoEspecieDoc   := '13'; 

      { Pegando Tipo de Cobrança}
      case StrToInt(ACBrTitulo.Carteira) of
        11,17 :
          case CaracTitulo of
            tcSimples: aTipoCobranca:='     ';
            tcDescontada: aTipoCobranca:='04DSC';
            tcVendor: aTipoCobranca:='08VDR';
            tcVinculada: aTipoCobranca:='02VIN';
          else
            aTipoCobranca:='     ';
          end;
      else
        aTipoCobranca:='     ';
      end;

      {Intruções - Protesto}
      if (aTipoOcorrencia = '01') or (aTipoOcorrencia = '09') then
      begin
        if (DataProtesto > 0) and (DataProtesto > Vencimento) then
        begin
          AInstrucao   := '06'; // Protestar em xx dias corridos
          DiasProtesto := IntToStr(DaysBetween(DataProtesto, Vencimento))  
        end;        
      end
      else if ATipoOcorrencia = '02' then // 02-Pedido de Baixa      
        AInstrucao := '44';


      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : ATipoSacado := '01';
         pJuridica : ATipoSacado := '02';
      else
         ATipoSacado := '00';
      end;

      {Pegando Tipo de Cedente}
      case ACBrBoleto.Cedente.TipoInscricao of
         pFisica   : ATipoCendente := '01';
         pJuridica : ATipoCendente := '02';
      end;

      AMensagem   := '';
      if Mensagem.Text <> '' then
         AMensagem   := Mensagem.Strings[0];


      with ACBrBoleto do
      begin
         wLinha:= '7' +                                                         // 001 a 001 - ID Registro
                  ATipoCendente +                                               // 002 a 003 - Tipo de inscrição da empresa 01-CPF / 02-CNPJ
                  PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0') +                 // 004 a 017 - Inscrição da empresa
                  aAgencia +                                                    // 018 a 021 - Prefixo da agência
                  PadRight( Cedente.AgenciaDigito, 1)  +                        // 022 a 022 - DV-prefixo da agencia
                  aConta +                                                      // 023 a 030 - Código do cendete/nr. conta corrente da empresa
                  PadRight( Cedente.ContaDigito, 1)  +                          // 031 a 031 - DV-código do cedente
                  PadLeft(OnlyNumber(Cedente.Convenio),7,'0') +                 // 032 a 038 - Número do convênio
                  PadRight( SeuNumero, 25 ) +                                   // 039 a 063 - Número de Controle do Participante
                  PadLeft( ANossoNumero, 17, '0') +                             // 064 a 080 - Nosso número
                  '0000' +                                                      // 081 a 084 - Zeros
                  '   ' +                                                       // 085 a 087 - Complemento do Registro: “Brancos”
                  ' ' +                                                         // 088 a 088 - Indic. Mensagem ou Sac.Avalista
                  '   ' +                                                       // 089 a 091 - Prefixo do título “Brancos”
                  aModalidade +                                                 // 092 a 094 - Variação da carteira
                  IntToStrZero(0,7) +                                           // 095 a 101 - Zero + Zeros
                  aTipoCobranca +                                               // 102 a 106 - Tipo de cobrança
                  Carteira +                                                    // 107 a 108 - Carteira
                  ATipoOcorrencia +                                             // 109 a 110 - Ocorrência "Comando"
                  PadRight( NumeroDocumento, 10, ' ') +                         // 111 a 120 - Seu Número - Nr. titulo dado pelo cedente
                  FormatDateTime( 'ddmmyy', Vencimento ) +                      // 121 a 126 - Data de vencimento
                  IntToStrZero( Round( ValorDocumento * 100 ), 13) +            // 127 a 139 - Valor do titulo
                  '085' + '0000' + ' ' +                                        // 140 a 147 - Numero do Banco - 085 + Prefixo da agência cobradora + DV-pref. agência cobradora
                  PadLeft(ATipoEspecieDoc, 2, '0') +                            // 148 a 149 - Especie de titulo
                  ATipoAceite +                                                 // 150 a 150 - Aceite
                  FormatDateTime( 'ddmmyy', DataDocumento ) +                   // 151 a 156 - Data de Emissão
                  PadLeft(AInstrucao, 2, '0') +                                 // 157 a 158 - Instrução codificada (cód. Protesto)
                  '00' +                                                        // 159 a 160 - Zeros
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13) +             // 161 a 173 - Juros de mora por dia
                  IntToStrZero(0,6) +                                           // 174 a 179 - Zeros
                  IntToStrZero( round( ValorDesconto * 100), 13) +              // 180 a 192 - Valor do desconto
                  IntToStrZero(0,13) +                                          // 193 a 205 - Zeros
                  IntToStrZero( round( ValorAbatimento * 100 ), 13) +           // 206 a 218 - Valor do abatimento permitido
                  ATipoSacado +                                                 // 219 a 220 - Tipo de inscricao do sacado
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0') +                  // 221 a 234 - CNPJ ou CPF do sacado
                  PadRight( Sacado.NomeSacado, 37) +                            // 235 a 271 - Nome do sacado
                  '   '  +                                                      // 272 a 274 - Brancos
                  PadRight(trim(Sacado.Logradouro) + ', ' +
                    trim(Sacado.Numero), 40) +                                  // 275 a 314 - Endereço do sacado
                  PadRight(trim(Sacado.Bairro), 12) +                           // 315 a 326 - Bairro do Sacado
                  PadLeft( OnlyNumber(Sacado.CEP), 8 ) +                        // 327 a 334 - CEP do endereço do sacado
                  PadRight( trim(Sacado.Cidade), 15) +                          // 335 a 349 - Cidade do sacado
                  PadRight( Sacado.UF, 2 ) +                                    // 350 a 351 - UF da cidade do sacado
                  PadRight( AMensagem, 40) +                                    // 352 a 391 - Observações
                  IfThen( DiasProtesto <> EmptyStr,
                          PadLeft(DiasProtesto ,2,'0'),
                          Space(2)) +                                           // 392 a 393 - Número de dias para protesto (deixar em branco se não houver instrução de protesto)
                  ' ' +                                                         // 394 a 394 - Branco
                  IntToStrZero( aRemessa.Count + 1, 6 );                        // 395 a 400 - Sequencial de Registro 

         if PercentualMulta > 0 then
           wLinha:= wLinha + sLineBreak +
                  '5' +                                                         //Tipo Registro
                  '99' +                                                        //Tipo de Serviço (Cobrança de Multa)
                  IfThen(PercentualMulta > 0, '2','9') +                        //Cod. Multa 2- Percentual 9-Sem Multa
                  IfThen(PercentualMulta > 0,
                         FormatDateTime('ddmmyy', DataMoraJuros),
                                        '000000') +                             //Data Multa
                  IntToStrZero( round( PercentualMulta * 100), 12) +            //Perc. Multa
                  Space(372) +                                                  //Brancos
                  IntToStrZero(aRemessa.Count + 2 ,6);

         aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

function TACBrBancoCecred.GetLocalPagamento: String;
begin
  Result := ACBrStr('Pagar preferencialmente nas cooperativas do Sistema Ailos.');
end;

function TACBrBancoCecred.GerarRegistroTrailler240(ARemessa: TStringList): String;
var
  wQTDTitulos : Integer;
begin
  wQTDTitulos := ARemessa.Count - 1;

  //REGISTRO TRAILER DO LOTE
  Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + // 1 a 3 - Código do banco
           '0001'                                                     + // 4 a 7 - Número do lote
           '5'                                                        + // 8 - Tipo do registro: Registro trailer do lote
           Space(9)                                                   + // 9 - 17 - Uso exclusivo FEBRABAN/CNAB
           IntToStrZero((3 * wQTDTitulos + 2), 6)                     + // 18 a 23 - Quantidade de Registro no Lote
           IntToStrZero((wQTDTitulos), 6)                             + // 24 a 29 - Quantidade títulos em cobrança - Cobr. Simples
           IntToStrZero( round( fValorTotalDocs * 100), 17)           + // 30 a 46 - Valor dos títulos em carteiras - Cobr. Simples
           PadRight('', 6, '0')                                       + // 47 a 52 - Quantidade títulos em cobrança - Cobr. Vinculada
           PadRight('',17, '0')                                       + // 53 a 69 - Valor dos títulos em carteiras - Cobr. Vinculada
           PadRight('', 6, '0')                                       + // 70 a 75 - Quantidade títulos em cobrança - Cobr. Caucionada
           PadRight('',17, '0')                                       + // 76 a 92 - Valor dos títulos em carteiras - Cobr. Caucionada
           PadRight('', 6, '0')                                       + // 93 a 98 - Quantidade títulos em cobrança - Cobr. Descontada
           PadRight('',17, '0')                                       + // 99 a 115 - Valor dos títulos em carteiras - Cobr. Descontada
           Space(8)                                                   + // 116 a 123 - Numero do Aviso de lançamento
           PadRight('',117,' ');                                        // 124 a 240 - Uso exclusivo FEBRABAN/CNAB

  //GERAR REGISTRO TRAILER DO ARQUIVO
  Result:= Result + #13#10 +
           IntToStrZero(ACBrBanco.Numero, 3)                          + // 1 a 3 - Código do banco
           '9999'                                                     + // 4 a 4 - Lote de serviço
           '9'                                                        + // 8 - Tipo do registro: Registro trailer do arquivo
           space(9)                                                   + // 9 a 17 - Uso exclusivo FEBRABAN/CNAB
           '000001'                                                   + // 18 a 23 - Quantidade de lotes do arquivo
           IntToStrZero((wQTDTitulos * 2) + fCountRegR + 4, 6)        + // 24 a 29 - Quantidade de registros do arquivo
           PadRight('', 6, '0')                                       + // 30 a 35 - Qtde de contas concil
           space(205);                                                  // 36 a 240 - Uso exclusivo FEBRABAN/CNAB
end;

procedure TACBrBancoCecred.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero(ARemessa.Count + 1, 6);  // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoCecred.TipoOcorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoRegistroConfirmado                     : Result := '02';
      toRetornoRegistroRecusado                       : Result := '03';
      toRetornoLiquidado                              : Result := '06';
      toRetornoRecebimentoInstrucaoConcederDesconto   : Result := '07';
      toRetornoRecebimentoInstrucaoCancelarDesconto   : Result := '08';
      toRetornoBaixado                                : Result := '09';
      toRetornoAbatimentoConcedido                    : Result := '12';
      toRetornoAbatimentoCancelado                    : Result := '13';
      toRetornoVencimentoAlterado                     : Result := '14';
      toRetornoLiquidadoAposBaixaOuNaoRegistro        : Result := '17';
      toRetornoRecebimentoInstrucaoProtestar          : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto     : Result := '20';
      toRetornoEncaminhadoACartorio                   : Result := '23';
      toRetornoRetiradoDeCartorio                     : Result := '24';
      toRetornoBaixaPorProtesto                       : Result := '25';
      toRetornoInstrucaoRejeitada                     : Result := '26';
      toRetornoAlteracaoUsoCedente                    : Result := '27';
      toRetornoDebitoTarifas                          : Result := '28';
      toRetornoRecebimentoInstrucaoAlterarNomeSacado  : Result := '42';
      toRetornoProtestoOuSustacaoEstornado            : Result := '46';
      //                                              : Result := '76';
      //                                              : Result := '77';
      //                                              : Result := '91';
      toRetornoEntradaNegativacaoRejeitada            : Result := '92';
      toRetornoInclusaoNegativacao                    : Result := '93';
      toRetornoExclusaoNegativacao                    : Result := '94';
    end;
  end
  else
  begin  // 400 -  109/110  Comando Retorno Tabela 04, pag 37
    case TipoOcorrencia of
      toRetornoRegistroConfirmado                   : Result :=  '02';
      toRetornoComandoRecusado     	                : Result :=  '03';
      toRetornoLiquidado     		                    : Result :=  '06';
      toRetornoRecebimentoInstrucaoConcederDesconto : Result :=  '07';
      toRetornoBaixado                              : Result :=  '09';
      toRetornoAbatimentoConcedido                  : Result :=  '12';
      toRetornoAbatimentoCancelado                  : Result :=  '13';
      toRetornoRecebimentoInstrucaoAlterarVencimento: Result :=  '14';
      toRetornoLiquidadoEmCartorio                  : Result :=  '15';
      toRetornoConfirmacaoAlteracaoJurosMora        : Result :=  '16';
      toRetornoLiquidadoAposBaixaOuNaoRegistro      : Result :=  '17';
      toRetornoConfInstrucaoProtesto                : Result :=  '19';
      toRetornoEncaminhadoACartorio                 : Result :=  '23';
      toRetornoConfInstrucaoSustarProtesto          : Result :=  '24';
      toRetornoProtestado                           : Result :=  '25';
      toRetornoInstrucaoRejeitada                   : Result :=  '26';
      toRetornoDebitoTarifas                        : Result :=  '28';

    end;
  end;

  if (Result <> '') then
    Exit;
  {
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                                : Result := '02';
    toRetornoLiquidado                                         : Result := '06';
    toRetornoBaixado                                           : Result := '09';
    toRetornoTituloEmSer                                       : Result := '11';
    toRetornoAbatimentoConcedido                               : Result := '12';
    toRetornoAbatimentoCancelado                               : Result := '13';
    toRetornoVencimentoAlterado                                : Result := '14';
    toRetornoLiquidadoAposBaixaOuNaoRegistro                   : Result := '17';
    toRetornoRecebimentoInstrucaoProtestar                     : Result := '19';
    toRetornoEncaminhadoACartorio                              : Result := '23';
  else
    Result := '02';
  end;
  }
end;

function TACBrBancoCecred.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      02: Result := '02-Entrada Confirmada';
      03: Result := '03-Entrada Rejeitada';
      06: Result := '06-Liquidação';
      07: Result := '07-Confirmação do Recebimento da Instrução de Desconto';
      08: Result := '08-Confirmação do Recebimento do Cancelamento do Desconto';
      09: Result := '09-Baixa';
      12: Result := '12-Confirmação Recebimento Instrução de Abatimento';
      13: Result := '13-Confirmação Recebimento Instrução de Cancelamento Abatimento';
      14: Result := '14-Confirmação Recebimento Instrução Alteração de Vencimento';
      17: Result := '17-Liquidação Após Baixa ou Liquidação Título Não Registrado';
      19: Result := '19-Confirmação Recebimento Instrução de Protesto';
      20: Result := '20-Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
      23: Result := '23-Remessa a Cartório (Aponte em Cartório)';
      24: Result := '24-Retirada de Cartório e Manutenção em Carteira';
      25: Result := '25-Protestado e Baixado (Baixa por Ter Sido Protestado)';
      26: Result := '26-Instrução Rejeitada';
      27: Result := '27-Confirmação do Pedido de Alteração de Outros Dados';
      28: Result := '28-Débito de Tarifas/Custas';
      42: Result := '42-Confirmação da alteração dos dados do Sacado';
      46: Result := '46-Instrução para cancelar protesto confirmada';
    //76: Result := '76-Liquidação de boleto cooperativa emite e expede';
    //77: Result := '77-Liquidação de boleto após baixa ou não registrado cooperativa emite e expede';
    //91: Result := '91-Título em aberto não enviado ao pagador';
      92: Result := '92-Inconsistência Negativação Serasa';
      93: Result := '93-Inclusão Negativação via Serasa';
      94: Result := '94-Exclusão Negativação Serasa';
    end;
  end
  else
  begin  // 400 -  109/110  Comando Retorno Tabela 04, pag 37
    case CodOcorrencia of
      02: Result := '02-Confirmação de entrada de título';
      03: Result := '03-Comando recusado (Motivo indicado na posição 087/088)';
      06: Result := '06-Liquidação Normal';
      07: Result := '07-Confirmação de recebimento da instrução de desconto';
      09: Result := '09-Baixa de Título';
      12: Result := '12-Abatimento Concedido';
      13: Result := '13-Abatimento Cancelado';
      14: Result := '14-Alteração de Vencimento do título';
      15: Result := '15-Liquidação em Cartório';
      16: Result := '16-Confirmação de alteração de juros de mora';
      17: Result := '17-Liquidação após Baixa ou Liquidação de título não registrado';
      19: Result := '19-Confirmação de recebimento de instruções para protesto';
      23: Result := '23-Indicação de encaminhamento a cartório';
      24: Result := '24-Sustar Protesto';
      25: Result := '25-Protestado e baixado';
      26: Result := '26-Instrução rejeitada';
      28: Result := '28–Débito de tarifas/custas';
    end;
  end;

  if (Result <> '') then
  begin
    Result := ACBrSTr(Result);
    Exit;
  end;

  case CodOcorrencia of
    02: Result := '02-Confirmação de Entrada de Título';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixa de Título';
    11: Result := '11-Titulos em Ser';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Alteração de Vencimento do Titulo';
    17: Result := '17–Liquidação após Baixa ou Liquidação de Título não Registrado';
    19: Result := '19-Confirmação de Recebimento de Instruções para Protesto';
    23: Result := '23-Indicação de Encaminhamento a Cartório';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoCecred.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of  // 16,2 Dominio
      02 : Result := toRetornoRegistroConfirmado;
      03 : Result := toRetornoRegistroRecusado;
      06 : Result := toRetornoLiquidado;
      07 : Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08 : Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09 : Result := toRetornoBaixado;
      12 : Result := toRetornoAbatimentoConcedido;
      13 : Result := toRetornoAbatimentoCancelado;
      14 : Result := toRetornoVencimentoAlterado;
      17 : Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19 : Result := toRetornoRecebimentoInstrucaoProtestar;
      20 : Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      23 : Result := toRetornoEncaminhadoACartorio;
      24 : Result := toRetornoRetiradoDeCartorio;
      25 : Result := toRetornoBaixaPorProtesto;
      26 : Result := toRetornoInstrucaoRejeitada;
      27 : Result := toRetornoAlteracaoUsoCedente;
      28 : Result := toRetornoDebitoTarifas;
      42 : Result := toRetornoRecebimentoInstrucaoAlterarNomeSacado;
      46 : Result := toRetornoProtestoOuSustacaoEstornado;
    //76 : Result :=
    //77 : Result :=
    //91 : Result :=
      92 : Result := toRetornoEntradaNegativacaoRejeitada;
      93 : Result := toRetornoInclusaoNegativacao;
      94 : Result := toRetornoExclusaoNegativacao
    else
      Result := toRetornoOutrasOcorrencias;
    end;
  end
  else
  begin // 400 -  109/110  Comando Retorno Tabela 04, pag 37
    case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoComandoRecusado;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      09: Result := toRetornoBaixado;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoRecebimentoInstrucaoAlterarVencimento;
      15: Result := toRetornoLiquidadoEmCartorio;
      16: Result := toRetornoConfirmacaoAlteracaoJurosMora;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19: Result := toRetornoConfInstrucaoProtesto;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoConfInstrucaoSustarProtesto;
      25: Result := toRetornoProtestado;
      26: Result := toRetornoInstrucaoRejeitada;
      28: Result := toRetornoDebitoTarifas;
    else
      Result := toRetornoOutrasOcorrencias;
    end;
  end;


end;

function TACBrBancoCecred.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    11 : Result:= toRemessaDispensarJuros;                  {Instrução para dispensar juros}
    12 : Result:= toRemessaAlterarNomeEnderecoSacado;       {Alteração de nome e endereço do Sacado}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    32 : Result:= toRemessaCancelarDesconto;                {Não conceder desconto}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoCecred.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then // 16,2
     begin
       case TipoOcorrencia of
          toRetornoRegistroConfirmado, //02
          toRetornoRegistroRecusado,    //03
          toRetornoInstrucaoRejeitada: //26
           //** nao existe 30 e 91
          case CodMotivo of // Dominio A - de 1 a 95 para 02,03,26 e 30
            01: Result:= '01-Código do Banco Inválido';
            02: Result:= '02-Código do Registro Detalhe Inválido';
            03: Result:= '03-Código do Segmento Inválido';
            04: Result:= '04-Código de Movimento Não Permitido para Carteira';
            05: Result:= '05-Código de Movimento Inválido';
            06: Result:= '06-Tipo/Número de Inscrição do Cedente Inválidos';
            07: Result:= '07-Agência/Conta/DV Inválido';
            08: Result:= '08-Nosso Número Inválido';
            09: Result:= '09-Nosso Número Duplicado';
            10: Result:= '10-Carteira Inválida';
            11: Result:= '11-Forma de Cadastramento do Título Inválido';
            12: Result:= '12-Tipo de Documento Inválido';
            13: Result:= '13-Identificação da Emissão do Boleto Inválida';
            14: Result:= '14-Identificação da Distribuição do Boleto Inválida';
            15: Result:= '15-Características da Cobrança Incompatíveis';
            16: Result:= '16-Data de Vencimento Inválida';
            17: Result:= '17-Data de Vencimento Anterior a Data de Emissão';
            18: Result:= '18-Vencimento Fora do Prazo de Operação';
            19: Result:= '19-Título a Cargo de Bancos Correspondentes com Vencimento Inferior a XX Dias';
            20: Result:= '20-Valor do Título Inválido';
            21: Result:= '21-Espécie do Título Inválida';
            22: Result:= '22-Espécie do Título Não Permitida para a Carteira';
            23: Result:= '23-Aceite Inválido';
            24: Result:= '24-Data da Emissão Inválida';
            25: Result:= '25-Data da Emissão Posterior a Data de Entrada';
            26: Result:= '26-Código de Juros de Mora Inválido';
            27: Result:= '27-Valor/Taxa de Juros de Mora Inválido';
            28: Result:= '28-Código do Desconto Inválido';
            29: Result:= '29-Valor do Desconto Maior ou Igual ao Valor do Título';
            30: Result:= '30-Desconto a Conceder Não Confere';
            31: Result:= '31-Concessão de Desconto - Já Existe Desconto Anterior';
            32: Result:= '32-Valor do IOF Inválido';
            33: Result:= '33-Valor do Abatimento Inválido';
            34: Result:= '34-Valor do Abatimento Maior ou Igual ao Valor do Título';
            35: Result:= '35-Valor a Conceder Não Confere';
            36: Result:= '36-Concessão de Abatimento - Já Existe Abatimento Anterior';
            37: Result:= '37-Código para Protesto Inválido';
            38: Result:= '38-Prazo para Protesto Inválido';
            39: Result:= '39-Pedido de Protesto Não Permitido para o Título';
            40: Result:= '40-Título com Ordem de Protesto Emitida';
            41: Result:= '41-Pedido de Cancelamento/Sustação para Títulos sem Instrução de Protesto';
            42: Result:= '42-Código para Baixa/Devolução Inválido';
            43: Result:= '43-Prazo para Baixa/Devolução Inválido';
            44: Result:= '44-Código da Moeda Inválido';
            45: Result:= '45-Nome do Sacado Não Informado';
            46: Result:= '46-Tipo/Número de Inscrição do Sacado Inválidos';
            47: Result:= '47-Endereço do Sacado Não Informado';
            48: Result:= '48-CEP Inválido';
            49: Result:= '49-CEP Sem Praça de Cobrança (Não Localizado)';
            50: Result:= '50-CEP Referente a um Banco Correspondente';
            51: Result:= '51-CEP incompatível com a Unidade da Federação';
            52: Result:= '52-Unidade da Federação Inválida';
            53: Result:= '53-Tipo/Número de Inscrição do Sacador/Avalista Inválidos';
            54: Result:= '54-Sacador/Avalista Não Informado';
            55: Result:= '55-Nosso número no Banco Correspondente Não Informado';
            56: Result:= '56-Código do Banco Correspondente Não Informado';
            57: Result:= '57-Código da Multa Inválido';
            58: Result:= '58-Data da Multa Inválida';
            59: Result:= '59-Valor/Percentual da Multa Inválido';
            60: Result:= '60-Movimento para Título Não Cadastrado';
            61: Result:= '61-Alteração da Agência Cobradora/DV Inválida';
            62: Result:= '62-Tipo de Impressão Inválido';
            63: Result:= '63-Entrada para Título já Cadastrado';
            64: Result:= '64-Número da Linha Inválido';
            65: Result:= '65-Código do Banco para Débito Inválido';
            66: Result:= '66-Agência/Conta/DV para Débito Inválido';
            67: Result:= '67-Dados para Débito incompatível com a Identificação da Emissão do Bloqueto';
            68: Result:= '68-Débito Automático Agendado';
            69: Result:= '69-Débito Não Agendado - Erro nos Dados da Remessa';
            70: Result:= '70-Débito Não Agendado - Sacado Não Consta do Cadastro de Autorizante';
            71: Result:= '71-Débito Não Agendado - Cedente Não Autorizado pelo Sacado';
            72: Result:= '72-Débito Não Agendado - Cedente Não Participa da Modalidade Débito Automático';
            73: Result:= '73-Débito Não Agendado - Código de Moeda Diferente de Real (R$)';
            74: Result:= '74-Débito Não Agendado - Data Vencimento Inválida';
            75: Result:= '75-Débito Não Agendado, Conforme seu Pedido, Título Não Registrado';
            76: Result:= '76-Débito Não Agendado, Tipo/Num. Inscrição do Debitado, Inválido';
            77: Result:= '77-Transferência para Desconto Não Permitida para a Carteira do Título';
            78: Result:= '78-Data Inferior ou Igual ao Vencimento para Débito Automático';
            79: Result:= '79-Data Juros de Mora Inválido';
            80: Result:= '80-Data do Desconto Inválida';
            81: Result:= '81-Tentativas de Débito Esgotadas - Baixado';
            82: Result:= '82-Tentativas de Débito Esgotadas - Pendente';
            83: Result:= '83-Limite Excedido';
            84: Result:= '84-Número Autorização Inexistente';
            85: Result:= '85-Título com Pagamento Vinculado';
            86: Result:= '86-Seu Número Inválido';
            87: Result:= '87-e-mail/SMS enviado';
            88: Result:= '88-e-mail Lido';
            89: Result:= '89-e-mail/SMS devolvido - endereço de e-mail ou número do celular incorreto';
            90: Result:= '90-e-mail devolvido - caixa postal cheia';
            91: Result:= '91-e-mail/número do celular do sacado não informado';
            92: Result:= '92-Sacado optante por Bloqueto Eletrônico - e-mail não enviado';
            93: Result:= '93-Código para emissão de bloqueto não permite envio de e-mail';
            94: Result:= '94-Código da Carteira inválido para envio e-mail.';
            95: Result:= '95-Contrato não permite o envio de e-mail';
          else
            Result := 'Ocorrencia(02,03,26)- retorno não mapeado';

          end;
       end;

       case TipoOcorrencia of
            toRetornoDebitoTarifas: // 28
            case CodMotivo of // Dominio B - 01 a 20 tarifas/custas
              01: Result := '01-Tarifa de Extrato de Posição';
              02: Result := '02-Tarifa de Manutenção de Título Vencido';
              03: Result := '03-Tarifa de Sustação';
              04: Result := '04-Tarifa de Protesto';
              05: Result := '05-Tarifa de Outras Instruções';
              06: Result := '06-Tarifa de Outras Ocorrências';
              07: Result := '07-Tarifa de Envio de Duplicata ao Sacado';
              08: Result := '08-Custas de Protesto';
              09: Result := '09-Custas de Sustação de Protesto';
              10: Result := '10-Custas de Cartório Distribuidor';
              11: Result := '11-Custas de Edital';
              12: Result := '12-Tarifa Sobre Devolução de Título Vencido';
              13: Result := '13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
              14: Result := '14-Tarifa Sobre Reapresentação Automática';
              15: Result := '15-Tarifa Sobre Rateio de Crédito';
              16: Result := '16-Tarifa Sobre Informações Via Fax';
              17: Result := '17-Tarifa Sobre Prorrogação de Vencimento';
              18: Result := '18-Tarifa Sobre Alteração de Abatimento/Desconto';
              19: Result := '19-Tarifa Sobre Arquivo mensal (Em Ser)';
              20: Result := '20-Tarifa Sobre Emissão de Bloqueto Pré-Emitido pelo Banco';
            else
              Result := 'Tarifas, retorno não mapeado';
            end;
       end;

       case TipoOcorrencia of
         toRetornoLiquidado,//06
         toRetornoBaixado,  //09
         toRetornoLiquidadoAposBaixaOuNaoRegistro: //17
         case CodMotivo of  // dominio c -Baixa de 01 a 15 , mvto 06,09 e 17
            01: Result := '01-Por Saldo';
            02: Result := '02-Por Conta';
            03: Result := '03-Liquidação no Guichê de Caixa em Dinheiro';
            04: Result := '04-Compensação Eletrônica';
            05: Result := '05-Compensação Convencional';
            06: Result := '06-Por Meio Eletrônico';
            07: Result := '07-Após Feriado Local';
            08: Result := '08-Em Cartório';
            30: Result := '30-Liquidação no Guichê de Caixa em Cheque';
            31: Result := '31-Liquidação em banco correspondente';
            32: Result := '32-Liquidação Terminal de Auto-Atendimento';
            33: Result := '33-Liquidação na Internet (Home banking)';
            34: Result := '34-Liquidado Office Banking';
            35: Result := '35-Liquidado Correspondente em Dinheiro';
            36: Result := '36-Liquidado Correspondente em Cheque';
            37: Result := '37-Liquidado por meio de Central de Atendimento (Telefone)';
            09: Result := '09-Comandada Banco';
            10: Result := '10-Comandada Cliente Arquivo';
            11: Result := '11-Comandada Cliente On-line';
            12: Result := '12-Decurso Prazo - Cliente';
            13: Result := '13-Decurso Prazo - Banco';
            14: Result := '14-Protestado';
            15: Result := '15-Título Excluído';
         else
            Result := 'Liquidado, retorno não mapeado';
         end;
       end;

     end
  else
     begin // c400 Natureza do Recebimento tabela 03 - Posicao 087/088 - Pag 33
        case TipoOcorrencia of

          toRetornoLiquidado,
          toRetornoLiquidadoEmCartorio:   // Comandos 06 e 15 posições 109/110
            case CodMotivo of
              01: Result:='01-Liquidação normal';
              09: Result:='09-Liquidação em cartório';
            else
              Result := 'Liquidado, retorno não mapeado';
            end;
          toRetornoRegistroConfirmado: //02 (Entrada)
            case CodMotivo of
              00: Result:='00-Por meio magnético';
              50: Result:='50-Sacado DDA';
            else
              Result := 'Confirmado, retorno não mapeado';
            end;
          toRetornoBaixado:  // 09 ou 10 (Baixa)
          //** nao existe 10
            case CodMotivo of
              00: Result:='00-Solicitada pelo cliente';
              15: Result:='15-Protestado';
              90: Result:='90-Baixa automática';
            else
              Result:='Baixado, Motivo não mapeado' ;
            end;

          toRetornoComandoRecusado: //03 (Recusado)
            case CodMotivo of
              01: Result:='01-Identificação inválida' ;
              04: Result:='04-Valor do desconto inválido' ;
              05: Result:='05-Espécie de título inválida para carteira' ;
              08: Result:='08-Valor do título/apólice inválido' ;
              09: Result:='09-Data de vencimento inválida' ;
              18: Result:='18-Endereço do sacado não localizado ou incompleto' ;
              24: Result:='24-Valor do abatimento inválido' ;
              27: Result:='27-Nome do sacado/cedente inválido ou não informado' ;
              28: Result:='28-Data do novo vencimento inválida' ;
              30: Result:='30-Registro de título já liquidado' ;
              36: Result:='36-Dias para fichamento de protesto inválido' ;
              37: Result:='37-Data de emissão do título inválida' ;
              38: Result:='38-Data do vencimento anterior a data da emissão do título' ;
              39: Result:='39-Comando de alteração indevido para a carteira' ;
              41: Result:='41-Abatimento não permitido' ;
              42: Result:='42-CEP/UF inválido/não compatíveis (ECT)' ;
              52: Result:='52-Abatimento igual ou maior que o valor do título' ;
              68: Result:='68-Código/Data/Percentual de multa inválido' ;
              69: Result:='69-Valor/Percentual de juros inválido' ;
              80: Result:='80-Nosso número inválido' ;
              81: Result:='81-Data para concessão do desconto inválida' ;
              82: Result:='82-CEP do sacado inválido' ;
              84: Result:='84-Título não localizado na existencia' ;
              99: Result:='99-Outros motivos' ;
            else
              Result:='Recusado, Motivo não mapeado' ;
            end;
        end;

        Result := ACBrSTr(Result);

     end;

end;

procedure TACBrBancoCecred.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF, rConta: String;
  ContLinha : Integer;
  idxMotivo: Integer;
begin
   // informação do Header
   // Verifica se o arquivo pertence ao banco
   if StrToIntDef(copy(ARetorno.Strings[0], 1, 3),-1) <> Numero then
     raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                            'não' + 'é um arquivo de retorno do ' + Nome));

   ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                           Copy(ARetorno[0],146,2)+'/'+
                                                           Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);

   rCedente := trim(copy(ARetorno[0], 73, 30));
   rCNPJCPF := OnlyNumber( copy(ARetorno[0], 19, 14) );
   rConta := OnlyNumber( copy(ARetorno[0], 60, 11) );

   ValidarDadosRetorno('', '', rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
       Cedente.Nome     := rCedente;
       Cedente.CNPJCPF  := rCNPJCPF;
       Cedente.Conta    := rConta;
       Cedente.Agencia  := OnlyNumber(Copy(ARetorno[0], 53, 5));
       Cedente.Convenio := OnlyNumber(Trim(Copy(ARetorno[0], 33, 20)));

       if StrToIntDef(copy(ARetorno[0], 18, 1), 0) = 1 then
         Cedente.TipoInscricao := pFisica
       else
         Cedente.TipoInscricao := pJuridica;
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 20;

   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
     Linha := ARetorno[ContLinha];

     if copy(Linha, 8, 1) <> '3' then // verifica se o registro (linha) é um registro detalhe (segmento J)
       Continue;

     if copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo titulo
       Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     if Assigned(Titulo) then
     with Titulo do
     begin
       if copy(Linha, 14, 1) = 'T' then
       begin
         SeuNumero       := copy(Linha, 106, 25);
         NumeroDocumento := copy(Linha, 59, 15);
         Carteira        := copy(Linha, 58, 1);

         TempData := copy(Linha, 74, 2) + '/'+copy(Linha, 76, 2)+'/'+copy(Linha, 78, 4);
         if TempData<>'00/00/0000' then
           Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

         ValorDocumento       := StrToFloatDef(copy(Linha, 82, 15), 0) / 100;
         NossoNumero          := copy(Linha, 38, 20);
         ValorDespesaCobranca := StrToFloatDef(copy(Linha, 199, 15), 0) / 100;

         OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 16, 2), 0));

         IdxMotivo := 214;

         while (IdxMotivo < 223) do
         begin
           if (trim(Copy(Linha, IdxMotivo, 2)) <> '') then
           begin
             MotivoRejeicaoComando.Add(Copy(Linha, IdxMotivo, 2));
             DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, StrToIntDef(Copy(Linha, IdxMotivo, 2), 0)));
           end;
           Inc(IdxMotivo, 2);
         end;
       end
       else // segmento U
       begin
         ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
         ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
         ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
         ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
         ValorOutrosCreditos := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
         ValorRecebido       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;

         TempData := copy(Linha, 138, 2)+'/'+copy(Linha, 140, 2)+'/'+copy(Linha, 142, 4);
         if TempData <> '00/00/0000' then
           DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

         TempData := copy(Linha, 146, 2)+'/'+copy(Linha, 148, 2)+'/'+copy(Linha, 150, 4);
         if TempData<>'00/00/0000' then
           DataCredito := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
       end;
     end;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := 10;
end;

procedure TACBrBancoCecred.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo, MotivoLinha : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
begin
   fpTamanhoMaximoNossoNum := 17;

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente      := trim(Copy(ARetorno[0],47,30));
   rAgencia      := trim(Copy(ARetorno[0],27,4));
   rDigitoAgencia:= Copy(ARetorno[0],31,1);
   rConta        := trim(Copy(ARetorno[0],32,8));
   rDigitoConta  := Copy(ARetorno[0],40,1);

   rCodigoCedente:= Copy(ARetorno[0],41,6);


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],41,6),0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   ValidarDadosRetorno(rAgencia, rConta);
   with ACBrBanco.ACBrBoleto do
   begin
      Cedente.Nome         := rCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;
      Cedente.CodigoCedente:= rCodigoCedente;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if (Copy(Linha,1,1) <> '7') then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         SeuNumero                   := copy(Linha,39,25);
         NumeroDocumento             := copy(Linha,117,10);

         // Tabela 04 Ocorrencias Retorno
         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,109,2),0));

         // Tabela 04 Ocorrencias Retorno
         CodOcorrencia := StrToIntDef(IfThen(copy(Linha,109,2) = '00','00',copy(Linha,109,2)),0);

         if (CodOcorrencia = 5) or   // nao existe algumas ocorrencias
            (CodOcorrencia = 6) or
            (CodOcorrencia = 7) or
            (CodOcorrencia = 8) or
            (CodOcorrencia = 15) or
            (CodOcorrencia = 46) then
         begin
           // Tabela 04 Ocorrencias Retorno
           CodigoLiquidacao := copy(Linha,109,2);
           CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
         end;


         if(CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
         begin
           MotivoLinha:= 87; // 87,2 -> tabela 03
           CodMotivo:= StrToInt(IfThen(copy(Linha,MotivoLinha,2) = '00','00',copy(Linha,MotivoLinha,2)));
           MotivoRejeicaoComando.Add(copy(Linha,MotivoLinha,2));
           DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
         end;

         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,2),0, 'DD/MM/YY' );

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,306,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         NossoNumero          := Copy(Linha,64,17);
         Carteira             := Copy(Linha,107,2);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

         if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                               Copy(Linha,178,2)+'/'+
                                               Copy(Linha,180,2),0, 'DD/MM/YY' );
      end;
   end;

   fpTamanhoMaximoNossoNum := 17;
end;


end.
