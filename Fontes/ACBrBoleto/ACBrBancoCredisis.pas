{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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

unit ACBrBancoCredisis;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao;

const
  CACBrBancoCredisis_Versao = '0.0.1';

type
  { TACBrBancoCredisis}

  TACBrBancoCredisis = class(TACBrBancoClass)
   protected
   private
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
    procedure LerRetorno400Pos6(ARetorno: TStringList);
    procedure LerRetorno400Pos7(ARetorno: TStringList);
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList);  override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    Procedure LerRetorno240(ARetorno:TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;

    {function CalcularTamMaximoNossoNumero(const Carteira : String; NossoNumero : String = ''; Convenio: String = ''): Integer; override;}
   end;

implementation

uses {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  StrUtils, Variants, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrBancoCredisis.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 3;
   fpNome                  := 'CrediSIS';
   fpNumero                := 097;
   fpTamanhoMaximoNossoNum := 6;
   fpTamanhoConta          := 12;
   fpTamanhoAgencia        := 4;
   fpTamanhoCarteira       := 2;
   fpCodigosMoraAceitos    := '123';
end;

function TACBrBancoCredisis.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string;
begin
  Modulo.CalculoPadrao;
  Modulo.Documento := ACBrTitulo.NossoNumero;
  Modulo.Calcular;
  if Modulo.ModuloFinal = 0 then
    Result := '1'
  else
    Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoCredisis.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero, AConvenio: String;
  wTamNossoNum: Integer;
begin
  with ACBrTitulo do
  begin
    AConvenio    := ACBrBoleto.Cedente.Convenio;
    ANossoNumero := NossoNumero;
    wTamNossoNum := CalcularTamMaximoNossoNumero(Carteira,ANossoNumero);
      
    if ((ACBrTitulo.Carteira = '16') or (ACBrTitulo.Carteira = '18')) and
        (Length(AConvenio) = 6) and (wTamNossoNum = 17) then
      ANossoNumero := PadLeft(ANossoNumero, 17, '0')
    else if Length(AConvenio) <= 4 then
      ANossoNumero := PadLeft(AConvenio, 4, '0') + PadLeft(ANossoNumero, 7, '0')
    else if (Length(AConvenio) > 4) and (Length(AConvenio) <= 6) then
      ANossoNumero := PadLeft(AConvenio, 6, '0') + PadLeft(ANossoNumero, 5, '0')
    else if (Length(AConvenio) = 7) then
      ANossoNumero := PadLeft(AConvenio, 7, '0') + RightStr(ANossoNumero, 10);
  end;
  Result := ANossoNumero;
end;

function TACBrBancoCredisis.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  ANossoNumero : String;
  {wTamNossNum: Integer;}
begin
   ANossoNumero := MontarCampoNossoNumero(ACBrTitulo);

   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
      CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                       '9' +
                       FatorVencimento +
                       IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                       '00000' +
                       ANossoNumero;

      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44) ;
end;

function TACBrBancoCredisis.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta,0)) +'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoCredisis.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo
  ): String;
begin
  Result := IfThen(ACBrTitulo.ACBrBoleto.Cedente.Modalidade = '',
                   ACBrTitulo.Carteira,
                   ACBrTitulo.Carteira + '/' + ACBrTitulo.ACBrBoleto.Cedente.Modalidade );
end;

function TACBrBancoCredisis.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var
  aSeqBoleto, aAgencia, aConvenio :string;
begin
  aAgencia     := PadLeft(OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.Agencia), 4, '0');
  aConvenio    := PadLeft(OnlyNumber(ACBrBanco.ACBrBoleto.Cedente.Convenio), 6, '0');

  aSeqBoleto   := PadLeft( IntToStr( StrToInt( OnlyNumber(ACBrTitulo.NossoNumero) ) ), 6, '0');
                  {retira 0 zeros esquerda (Layout do Banco precisa de 6 digitos) }

  Result       := IntToStrZero(ACBrBanco.Numero, 3) + CalcularDigitoVerificador(ACBrTitulo) + aAgencia + aConvenio + aSeqBoleto;
                  {numero banco na requsicao "097"                   1 digito                  4 digitos   6 digitos  6 digitos (sequencial do boleto)}

end;

function TACBrBancoCredisis.GerarRegistroHeader240(NumeroRemessa : Integer): String;
var
  ATipoInscricao,aConta:String;
  aAgencia,aModalidade :String;
begin

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
      aModalidade := PadLeft(trim(Modalidade), 3, '0');

      { GERAR REGISTRO-HEADER DO ARQUIVO }

      Result:= IntToStrZero(ACBrBanco.Numero, 3)               + // 1 a 3 - Código do banco
               '0000'                                          + // 4 a 7 - Lote de serviço
               '0'                                             + // 8 - Tipo de registro - Registro header de arquivo
               StringOfChar(' ', 9)                            + // 9 a 17 Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                  + // 18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 14, '0')           + // 19 a 32 -Número de inscrição do cedente
               PadLeft(Convenio, 20, '0')                      + // 33 a 52 - Código Convênio do Beneficiário
               PadLeft(Agencia, 5, '0')                        + // 53 a 57 - Agência Beneficiário
               StringOfChar(' ', 1)                            + // 58 - Brancos
               aConta                                          + // 59 a 70 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')                   + // 71 - Dígito da conta do cedente
               ' '                                             + // 72 - Dígito verificador da agência / conta
               TiraAcentos(UpperCase(PadRight(Nome, 30, ' '))) + // 73 a 102 - Nome do cedente
               PadRight('097CENTRALCREDI', 30, ' ')            + // 103 a 132 - Nome do banco
               StringOfChar(' ', 10)                           + // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1'                                             + // 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)                 + // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)                   + // 152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0')        + // 158 a 163 - Número seqüencial do arquivo
               '101'                                           + // 164 a 166 - Número da versão do layout do arquivo
               StringOfChar(' ', 5)                            + // 167 a 171 - Brancos
               StringOfChar(' ', 20)                           + // 172 a 191 - Uso reservado do banco
               StringOfChar(' ', 20)                           + // 192 a 211 - Uso reservado da empresa
               StringOfChar(' ', 11)                           + // 212 a 222 - 11 brancos
               'CSP'                                           + // 223 a 225 - 'CSP'
               StringOfChar(' ', 3)                            + // 226 a 228 - Uso exclusivo de Vans
               StringOfChar(' ', 2)                            + // 229 a 230 - Tipo de servico
               StringOfChar(' ', 10);                            // 231 a 240 - titulo em carteira de cobranca

          { GERAR REGISTRO HEADER DO LOTE }

      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)               + // 1 a 3 - Código do banco
               '0001'                                          + // 4 a 7 - Lote de serviço
               '1'                                             + // 8 - Tipo de registro - Registro header de arquivo
               'R'                                             + // 9 - Tipo de operação: R (Remessa) ou T (Retorno)
               '01'                                            + // 10 a 11 - Tipo de serviço: 01 (Cobrança)
               StringOfChar(' ', 2)                            + // 12 a 13 - Forma de lançamento: preencher com ZEROS no caso de cobrança
               '060'                                           + // 14 a 16 - Número da versão do layout do lote
               ' '                                             + // 17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                  + // 18 - Tipo de inscrição do cedente
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')           + // 19 a 33 -Número de inscrição do cedente
               PadLeft(Convenio, 20, '0')                      + // 34 a 53 - Código Convênio do Beneficiário
               PadLeft(Agencia, 5, '0')                        + // 53 a 58 - Agência Beneficiário
               StringOfChar(' ', 1)                            + // 59 - Brancos
               aConta                                          + // 60 a 71 - Número da conta do cedente
               PadRight(ContaDigito, 1, '0')                   + // 72 - Dígito da conta do cedente
               ' '                                             + // 73 - Dígito verificador da agência / conta
               PadRight(Nome, 30, ' ')                         + // 74 a 103 - Nome do cedente
               StringOfChar(' ', 40)                           + // 104 a 143 - Mensagem 1 para todos os boletos do lote
               StringOfChar(' ', 40)                           + // 144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0')        + // 184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now)                 + // 192 a 199 - Data de geração do arquivo
               StringOfChar(' ', 8)                            + // 200 a 207 - Data do crédito - Só para arquivo retorno
               StringOfChar(' ', 33);                            // 208 a 240 - Uso exclusivo FEBRABAN/CNAB
   end;
end;

function TACBrBancoCredisis.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
   ATipoOcorrencia, ATipoBoleto : String;
   ADataMoraJuros, ADataDesconto: String;
   ANossoNumero, ATipoAceite    : String;
   aAgencia, aConta{, aDV}        : String;
   ProtestoBaixa                : String;
//   wTamConvenio, wTamNossoNum   : Integer;
//   wCarteira                    : Integer;
   {ACaracTitulo,} wTipoCarteira  : Char;
begin
   with ACBrTitulo do
   begin
     ANossoNumero := MontarCampoNossoNumero(ACBrTitulo);
     aAgencia := PadLeft(ACBrBoleto.Cedente.Agencia, 5, '0');
     aConta   := PadLeft(ACBrBoleto.Cedente.Conta, 12, '0');

     {SEGMENTO P}

     {Pegando o Tipo de Ocorrencia}
     case OcorrenciaOriginal.Tipo of
       toRemessaRegistrar                     : ATipoOcorrencia := '01';
       toRemessaAlterarDadosEmissaoBloqueto   : ATipoOcorrencia := '31';
       toRemessaBaixar                        : ATipoOcorrencia := '50';
       toRemessaCancelarInstrucao             : ATipoOcorrencia := '51';
     else
       ATipoOcorrencia := '01';   //default registrar
     end;

     { Pegando o tipo de EspecieDoc }
     if EspecieDoc = 'DMI' then
       EspecieDoc   := '03'
     else if EspecieDoc = 'DSI' then
       EspecieDoc   := '05'
     else if EspecieDoc = 'NP' then
       EspecieDoc   := '12'
     else if EspecieDoc = 'RC' then
       EspecieDoc   := '17'
     else if EspecieDoc = 'NE' then
       EspecieDoc   := '21'
     else if EspecieDoc = 'NF' then
       EspecieDoc   := '23';
//     else
//       EspecieDoc := EspecieDoc;

     { Pegando o Aceite do Titulo }
     case Aceite of
       atSim :  ATipoAceite := 'A';
       atNao :  ATipoAceite := 'N';
     else
       ATipoAceite := 'N';
     end;

     {Pegando Tipo de Boleto}
     case ACBrBoleto.Cedente.ResponEmissao of
       tbCliEmite        : ATipoBoleto := '2' + '2';
       tbBancoEmite      : ATipoBoleto := '1' + '1';
       tbBancoReemite    : ATipoBoleto := '4' + '1';
       tbBancoNaoReemite : ATipoBoleto := '5' + '2';
     end;
//     ACaracTitulo := ' ';
//     case CaracTitulo of
//       tcSimples     : ACaracTitulo  := '1';
//       tcVinculada   : ACaracTitulo  := '2';
//       tcCaucionada  : ACaracTitulo  := '3';
//       tcDescontada  : ACaracTitulo  := '4';
//       tcVendor      : ACaracTitulo  := '5';
//     end;

//     wCarteira:= StrToIntDef(Carteira,0);
//     if (wCarteira = 18) and (ACaracTitulo = '1') then
//       wTipoCarteira := '1'
//     else
       wTipoCarteira := '1';

     {Mora Juros}
     if (ValorMoraJuros > 0) and (DataMoraJuros > 0) then
       ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
     else
       ADataMoraJuros := PadRight('', 8, '0');

     {Descontos}
     if (ValorDesconto > 0) and (DataDesconto > 0) then
       ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto)
     else
       ADataDesconto := PadRight('', 8, '0');

     case CodigoNegativacao of
       cnProtestarCorrido: ProtestoBaixa := '1';
       cnProtestarUteis: ProtestoBaixa := '2';
       cnNaoProtestar: ProtestoBaixa := '3';
       cnNegativar: begin
         case TipoDiasNegativacao of
           diCorridos: ProtestoBaixa := '4';
           diUteis: ProtestoBaixa := '5';
         end;
       end;
       cnNaoNegativar: ProtestoBaixa := '6';
     else
       if (DataNegativacao > 0) then
       begin
         case TipoDiasNegativacao of
           diCorridos: ProtestoBaixa := '4';
           diUteis: ProtestoBaixa := '5';
         end;
       end
       else if (DataProtesto > 0) then
       begin
         case TipoDiasProtesto of
           diCorridos : ProtestoBaixa := '1';
           diUteis    : ProtestoBaixa := '2';
         end;
       end
       else
         ProtestoBaixa:= '3';
     end;

     {SEGMENTO P}
     Result:= IntToStrZero(ACBrBanco.Numero, 3)                                         + // 1 a 3 - Código do banco
              '0001'                                                                    + // 4 a 7 - Lote de serviço
              '3'                                                                       + // 8 - Tipo do registro: Registro detalhe
              IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 1 , 5) + // 9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'P'                                                                       + // 14 - Código do segmento do registro detalhe
              ' '                                                                       + // 15 - Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                           + // 16 a 17 - Código de movimento
              aAgencia                                                                  + // 18 a 22 - Agência mantenedora da conta
              ' '                                                                       + // 23 - Dígito verificador da agência
              aConta                                                                    + // 24 a 35 - Número da conta corrente
              PadRight(ACBrBoleto.Cedente.ContaDigito, 1, '0')                          + // 36 - Dígito verificador da conta
              ' '                                                                       + // 37 - Dígito verificador da agência / conta
              PadRight(ANossoNumero, 20, ' ')                                           + // 38 a 57 - Nosso número - identificação do título no banco
              wTipoCarteira                                                             + // 58 - Cobrança Simples
              '1'                                                                       + // 59 - Forma de cadastramento do título no banco: com cadastramento
              ' '                                                                       + // 60 - Tipo de documento: Tradicional
              ATipoBoleto                                                               + // 61 a 62 - Quem emite e quem distribui o boleto?
              PadRight(NumeroDocumento, 15, ' ')                                        + // 63 a 77 - Número que identifica o título na empresa [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              FormatDateTime('ddmmyyyy', Vencimento)                                    + // 78 a 85 - Data de vencimento do título
              IntToStrZero( round( ValorDocumento * 100), 15)                           + // 86 a 100 - Valor nominal do título
              aAgencia + ' '                                                            + // 101 a 106 - Agência cobradora + Digito. Se ficar em branco, a caixa determina automaticamente pelo CEP do sacado
              PadRight(EspecieDoc,2)                                                    + // 107 a 108 - Espécie do documento
              ATipoAceite                                                               + // 109 - Identificação de título Aceito / Não aceito
              FormatDateTime('ddmmyyyy', DataDocumento)                                 + // 110 a 117 - Data da emissão do documento
              IfThen(ValorMoraJuros > 0, '1', '3')                                      + // 118 - Código de juros de mora: Valor por dia
              ADataMoraJuros                                                            + // 119 a 126 - Data a partir da qual serão cobrados juros
              IfThen(ValorMoraJuros > 0,
                     IntToStrZero(round(ValorMoraJuros * 100), 13) + '00',
                     PadRight('', 15, '0'))                                             + // 127 a 141 - Valor de juros de mora por dia
              IfThen(ValorDesconto > 0, IfThen(DataDesconto > 0, '1','3'), '0')         + // 142 - Código de desconto: 1 - Valor fixo até a data informada 4-Desconto por dia de antecipacao 0 - Sem desconto
              IfThen(ValorDesconto > 0,
                     IfThen(DataDesconto > 0, ADataDesconto,'00000000'), '00000000')    + // 143 a 150 - Data do desconto
              IfThen(ValorDesconto > 0, IntToStrZero( round(ValorDesconto * 100), 13) + '00',
                     PadRight('', 15, '0'))                                             + // 151 a 165 - Valor do desconto por dia
              StringOfChar(' ', 15)                                                     + // 166 a 180 - Valor do IOF a ser recolhido
              StringOfChar(' ', 15)                                                     + // 181 a 195 - Valor do abatimento
              PadRight(SeuNumero, 25, ' ')                                              + // 196 a 220 - Identificação do título na empresa
              {IfThen((DataProtesto <> null) and (DataProtesto > Vencimento),
                     IfThen((DaySpan(Vencimento, DataProtesto) > 5), '1', '2'), '3')}
              ProtestoBaixa                                                             + // 221 - Código de protesto: Protestar em XX dias corridos
              IfThen((DataProtesto <> null) and (DataProtesto > Vencimento),
                     PadLeft(IntToStr(DaysBetween(DataProtesto, Vencimento)), 2, '0'),
                     '00')                                                              + // 222 a 223 - Prazo para protesto (em dias corridos)
              ' '                                                                       + // 224 - Campo não tratado pelo BB [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              StringOfChar(' ', 3)                                                      + // 225 a 227 - Campo não tratado pelo BB [ Alterado conforme instruções da CSO Brasília ] {27-07-09}
              '09'                                                                      + // 228 a 229 - Código da moeda: Real
              StringOfChar(' ', 10)                                                     + // 230 a 239 - Uso exclusivo FEBRABAN/CNAB
              ' ';                                                                        // 240 - Uso exclusivo FEBRABAN/CNAB

     {SEGMENTO Q}
     Result:= Result + #13#10 +
              IntToStrZero(ACBrBanco.Numero, 3)                                        + // 1 a 3 Código do banco
              '0001'                                                                   + // 4 a 7 Número do lote
              '3'                                                                      + // 8 a 8 Tipo do registro: Registro detalhe
              IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo)) + 2 ,5) + // 9 a 13 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'Q'                                                                      + // 14 a 14 Código do segmento do registro detalhe
              ' '                                                                      + // 15 a 15 Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                          + // 16 a 17 Tipo Ocorrencia
              IfThen(Sacado.Pessoa = pJuridica,'2','1')                                + // 18 a 18 Tipo inscricao
              PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                             + // 19 a 33 CPF/CNPJ do Pagador
              PadRight(Sacado.NomeSacado, 40, ' ')                                     + // 34 a 73 Nome ou Razão Social do Pagador
              PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' '+
                       Sacado.Complemento , 40, ' ')                                   + // 74 a 113 Endereço com N°. do Pagador.
              PadRight(Sacado.Bairro, 15, ' ')                                         + // 114 a 128 Bairro do Pagador
              PadLeft(OnlyNumber(Sacado.CEP), 5, '0')                                  + // 129 a 133 CEP do Pagador
              PadLeft(Copy(OnlyNumber(Sacado.CEP), 5, 8), 3, '0')                      + // 134 a 136 Sufixo do CEP do Pagador
              PadRight(Sacado.Cidade, 15, ' ')                                         + // 137 a 151 Cidade do Pagador
              PadRight(Sacado.UF, 2, ' ')                                              + // 152 a 153 Unidade Federação do Pagador
              ' '                                                                      + // 154 a 154 Tipo de inscrição: Não informado
              StringOfChar(' ', 15)                                                    + // 155 a 169 Número de inscrição
              StringOfChar(' ', 40)                                                    + // 170 a 209 Nome do sacador/avalista
              PadRight('097', 3, ' ')                                                  + // 210 a 212 Uso exclusivo FEBRABAN/CNAB
              PadRight(ANossoNumero,20, ' ')                                           + // 213 a 232 Identificação do Título - Nosso Numero
              StringOfChar(' ', 8);                                                      // 233 a 240 Uso exclusivo FEBRABAN/CNAB

     {SEGMENTO R}
     Result:= Result + #13#10 +
              IntToStrZero(ACBrBanco.Numero, 3)                                        + // 1 - 3 Código do banco
              '0001'                                                                   + // 4 - 7 Número do lote {TODO: Criar propriedade para receber o número do lote}
              '3'                                                                      + // 8 - 8 Tipo do registro: Registro detalhe
              IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 3 ,5)  + // 9 - 13 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
              'R'                                                                      + // 14 - 14 Código do segmento do registro detalhe
              ' '                                                                      + // 15 - 15 Uso exclusivo FEBRABAN/CNAB: Branco
              ATipoOcorrencia                                                          + // 16 - 17 Código de Movimento Remessa (01/50/51/31) *C004 (Manual)
              TipoDescontoToString(TipoDesconto2)                                      + // 18 - 18 Código do Desconto 2   (0/1/2)  *C021 (Manual)  (isento,valor,percentual)
              IfThen(TipoDescontoToString(TipoDesconto2)='0',
                    PadRight('', 8, '0'),FormatDateTime('ddmmyyyy', DataDesconto2))    + // 19 - 26 Data do Desconto 2 (se anterior for 0, não enviar data)
              IfThen((ValorDesconto2 <> null) and (ValorDesconto2 > 0),
                    IntToStrZero(round(ValorDesconto2 * 100 ),15),PadRight('',15,' ')) + // 27 - 41 Valor/Percentual Desconto 2,  *C023 (Manual)
              '0'                    {código desconto 3, ainda não implementado}       + // 42 - 42 Código de Desconto 3 (0/1/2)  *C021 (Manual)
              PadRight('',8,' ')     {data desconto 3, ainda não implementado}         + // 43 - 50 Data Desconto 3               *C022 (Manual)
              PadRight('',15,' ')    {valor/percentual 3, ainda não implementado}      + // 51 - 65 Valor/Percentual Desconto 3,  *C023 (Manual)
              IfThen((PercentualMulta <> null) and (PercentualMulta > 0), '2', '1')    + // 66 - 66 Código da Multa 1-Valor Fixo / 2-Percentual
              IfThen((PercentualMulta <> null) and (PercentualMulta > 0),
                    FormatDateTime('ddmmyyyy', DataMulta), PadRight('',8,' '))         + // 67 - 74 Se cobrar informe a data para iniciar a cobrança ou informe EM BRANCO para CONSIDERAR a DATA DO VENCIMENTO
              IfThen(PercentualMulta > 0,
                     IntToStrZero(round(PercentualMulta * 100), 13) + '00',
                     PadRight('', 15, '0'))                                            + // 75 - 89 Percentual de multa. Informar zeros se não cobrar
              PadRight('',10,' ')                                                      + // 90 - 99 Brancos
              PadRight( Instrucao1, 40, ' ')                                           + // 100 - 139 - Instrucao 1
              PadRight( Instrucao2, 40, ' ')                                           + // 140 - 179 - Instrucao 2
              PadRight('', 20, ' ')                                                    + // 180 - 199 - Uso exclusivo FEBRABAN/CNAB: Brancos
              PadRight('', 8 , ' ')                                                    + // 200 - 207 - Brancos
              PadRight('', 3 , ' ')                                                    + // 208 - 210 - Brancos
              PadRight('', 5 , ' ')                                                    + // 211 - 215 - Brancos
              PadRight('', 1 , ' ')                                                    + // 216 - 216 - Brancos
              PadRight('', 12, ' ')                                                    + // 217 - 228 - Brancos
              PadRight('', 1 , ' ')                                                    + // 229 - 229 - Brancos
              PadRight('', 1 , ' ')                                                    + // 230 - 230 - Brancos
              PadRight('', 1 , ' ')                                                    + // 231 - 231 - Brancos
              StringOfChar(' ', 9);                                                      // 232 - 240 - Uso exclusivo FEBRABAN/CNAB: Brancos
   end;
end;

function TACBrBancoCredisis.GerarRegistroTrailler240( ARemessa : TStringList ): String;
begin
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
            '0001'                                                     + //Número do lote
            '5'                                                        + //Tipo do registro: Registro trailer do lote
            Space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB
            //IntToStrZero(ARemessa.Count-1, 6)                        + //Quantidade de Registro da Remessa
            IntToStrZero((3 * ARemessa.Count-1), 6)                    + //Quantidade de Registro da Remessa
            PadRight('', 6, '0')                                           + //Quantidade títulos em cobrança
            PadRight('',17, '0')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, ' ')                                           + //Quantidade títulos em cobrança
            PadRight('',17, ' ')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, ' ')                                           + //Quantidade títulos em cobrança
            PadRight('',17, ' ')                                           + //Valor dos títulos em carteiras}
            PadRight('', 6, ' ')                                           + //Quantidade títulos em cobrança
            PadRight('',17, ' ')                                           + //Valor dos títulos em carteiras}
            Space(8)                                                   + //Uso exclusivo FEBRABAN/CNAB}
            PadRight('',117,' ')                                           ;

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
            '9999'                                                     + //Lote de serviço
            '9'                                                        + //Tipo do registro: Registro trailer do arquivo
            space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB}
            '000001'                                                   + //Quantidade de lotes do arquivo}
            IntToStrZero(((ARemessa.Count-1)* 3)+4, 6)                 + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
            space(6)                                                   + //Uso exclusivo FEBRABAN/CNAB}
            space(205);                                                  //Uso exclusivo FEBRABAN/CNAB}
end;


procedure TACBrBancoCredisis.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa:TStringList);
var
  TamConvenioMaior6 :Boolean;
  aAgencia, aConta  :String;
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    TamConvenioMaior6:= Length(trim(Convenio)) > 6;
    aAgencia:= RightStr(Agencia, 4);
    aConta  := RightStr(Conta, 8);

    wLinha:= '0'                              + // ID do Registro
             '1'                              + // ID do Arquivo( 1 - Remessa)
             'REMESSA'                        + // Literal de Remessa
             '01'                             + // Código do Tipo de Serviço
             PadRight( 'COBRANCA', 15 )       + // Descrição do tipo de serviço
             aAgencia                         + // Prefixo da agência/ onde esta cadastrado o convenente lider do cedente
             PadRight( AgenciaDigito, 1, ' ') + // DV-prefixo da agencia
             aConta                           + // Codigo do cedente/nr. da conta corrente que está cadastro o convenio lider do cedente
             PadRight( ContaDigito, 1, ' ');    // DV-código do cedente


    if TamConvenioMaior6 then
      wLinha:= wLinha + '000000'                       // Complemento
    else
      wLinha:= wLinha + PadLeft(trim(Convenio),6,'0'); // Convenio;

    wLinha:= wLinha + PadRight( Nome, 30)     + // Nome da Empresa
             IntToStrZero( Numero, 3)         + // Código do Banco
             PadRight('097CENTRALCRED', 15)   + // Nome do Banco(BANCO CREDISIS)
             FormatDateTime('ddmmyy',Now)     + // Data de geração do arquivo
             IntToStrZero(NumeroRemessa,7);     // Numero Remessa

    if TamConvenioMaior6 then
      wLinha:= wLinha + Space(22)                                        + // Nr. Sequencial de Remessa + brancos
               PadLeft(trim(ACBrBanco.ACBrBoleto.Cedente.Convenio),7,'0')+ // Nr. Convenio
               space(258)                                                  // Brancos
    else
      wLinha:= wLinha + Space(287);

      wLinha:= wLinha + IntToStrZero(1,6); // Nr. Sequencial do registro-informar 000001

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
  end;
end;

procedure TACBrBancoCredisis.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ANossoNumero, ADigitoNossoNumero :String;
  ATipoOcorrencia, AInstrucao      :String;
  ATipoSacado, ATipoCendente       :String;
  ATipoAceite, ATipoEspecieDoc     :String;
  AMensagem, DiasProtesto          :String;
  aDataDesconto, aAgencia, aConta  :String;
  aModalidade,wLinha, aTipoCobranca:String;
  TamConvenioMaior6                :Boolean;
  wCarteira: Integer;
begin

   with ACBrTitulo do
   begin
     wCarteira:= strtoint(Carteira);
     if ((wCarteira = 11) or (wCarteira= 31) or (wCarteira = 51)) or
        (((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17)) and
         (ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite)) then
      begin
       ANossoNumero       := '00000000000000000000';
       ADigitoNossoNumero := ' ';
      end
     else
      begin
       ANossoNumero       := FormataNossoNumero(ACBrTitulo);
       ADigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
      end;
      
     TamConvenioMaior6:= Length(trim(ACBrBoleto.Cedente.Convenio)) > 6;
     aAgencia         := PadLeft(ACBrBoleto.Cedente.Agencia, 4, '0');
     aConta           := RightStr(ACBrBoleto.Cedente.Conta, 8);
     aModalidade      := PadLeft(trim(ACBrBoleto.Cedente.Modalidade), 3, '0');

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
       toRemessaOutrasOcorrencias              : ATipoOcorrencia := '31'; {Alteração de Outros Dados}
       toRemessaCancelarDesconto               : ATipoOcorrencia := '32'; {Não conceder desconto}
       toRemessaAlterarModalidade              : ATipoOcorrencia := '40'; {Alterar modalidade (Vide Observações)}
     else
       ATipoOcorrencia := '01'; {Remessa}
     end;

     { Pegando o Aceite do Titulo }
     case Aceite of
       atSim :  ATipoAceite := 'A';
       atNao :  ATipoAceite := 'N';
     else
       ATipoAceite := 'N';
     end;

     { Pegando o tipo de EspecieDoc }
     if EspecieDoc = 'DMI' then
       ATipoEspecieDoc   := '03'
     else if EspecieDoc = 'DSI' then
       ATipoEspecieDoc   := '05'
     else if EspecieDoc = 'NP' then
       ATipoEspecieDoc   := '12'
     else if EspecieDoc = 'RC' then
       ATipoEspecieDoc   := '17'
     else if EspecieDoc = 'ME' then
       ATipoEspecieDoc   := '21'
     else if EspecieDoc = 'NF' then
       ATipoEspecieDoc   := '23'
     else
       ATipoEspecieDoc := EspecieDoc;

     { Pegando Tipo de Cobrança}
     case StrToInt(ACBrTitulo.Carteira) of
       11,17 :
          case ACBrBoleto.Cedente.CaracTitulo of
            tcSimples   : aTipoCobranca := '     ';
            tcDescontada: aTipoCobranca := '04DSC';
            tcVendor    : aTipoCobranca := '08VDR';
            tcVinculada : aTipoCobranca := '02VIN';
          else
            aTipoCobranca := '     ';
          end;
     else
       aTipoCobranca:='     ';
     end;

     if (DataProtesto > 0) and (DataProtesto > Vencimento) then
      begin
       DiasProtesto := '  ';
       case (DaysBetween(DataProtesto,Vencimento)) of
          3: // Protestar no 3º dia util após vencimento
          begin
            if (trim(Instrucao1) = '') or (trim(Instrucao1) = '03') then
              AInstrucao := '03'+ PadLeft(trim(Instrucao2),2,'0');
          end;
          4: // Protestar no 4º dia util após vencimento
          begin
            if (trim(Instrucao1) = '') or (trim(Instrucao1) = '04') then
              AInstrucao := '04'+ PadLeft(trim(Instrucao2),2,'0');
          end;
          5: // Protestar no 5º dia util após vencimento
          begin
            if (trim(Instrucao1) = '') or (trim(Instrucao1) = '05') then
              AInstrucao := '05'+ PadLeft(trim(Instrucao2),2,'0');
          end;
       else
         if (trim(Instrucao1) = '') or (trim(Instrucao1) = '06') then
           AInstrucao := '06'+ PadLeft(trim(Instrucao2),2,'0');
          DiasProtesto:=IntToStr(DaysBetween(DataProtesto,Vencimento));
       end;
      end
     else
      begin
       Instrucao1  := '07'; //Não Protestar
       AInstrucao  := PadLeft(Trim(Instrucao1),2,'0') + PadLeft(Trim(Instrucao2),2,'0');
       DiasProtesto:= '  ';
      end;

     aDataDesconto:= '000000';

     if ValorDesconto > 0 then
     begin
       if DataDesconto > EncodeDate(2000,01,01) then
         aDataDesconto := FormatDateTime('ddmmyy',DataDesconto)
       else
         aDataDesconto := '777777';
     end;

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
     else
       ATipoCendente := '02';
     end;

     AMensagem   := '';
     if Mensagem.Text <> '' then
       AMensagem   := Mensagem.Strings[0];

     with ACBrBoleto do
     begin
       if TamConvenioMaior6 then
         wLinha := '7'
       else
         wLinha := '1';

       wLinha:= wLinha                                     + // ID Registro
                ATipoCendente                              + // Tipo de inscrição da empresa
                PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0')+ // Inscrição da empresa
                aAgencia                                   + // Prefixo da agencia
                PadRight( Cedente.AgenciaDigito, 1)        + // DV-prefixo da agencia
                aConta                                     + // Código do cendete/nr. conta corrente da empresa
                PadRight( Cedente.ContaDigito, 1);           // DV-código do cedente

       if TamConvenioMaior6 then
         wLinha:= wLinha + PadLeft( trim(Cedente.Convenio), 7)  // Número do convenio
       else
         wLinha:= wLinha + PadLeft( trim(Cedente.Convenio), 6); // Número do convenio

       wLinha:= wLinha + PadRight( SeuNumero, 25 );             // Numero de Controle do Participante

       if TamConvenioMaior6 then
         wLinha:= wLinha + PadLeft( ANossoNumero, 17, '0')      // Nosso numero
       else
         wLinha:= wLinha + PadLeft( ANossoNumero,11)+ ADigitoNossoNumero;


       wLinha:= wLinha +
                '0000' + Space(7) + aModalidade;                // Zeros + Brancos + Prefixo do titulo + Variação da carteira

       if TamConvenioMaior6  then
         wLinha:= wLinha + IntToStrZero(0,7)                    // Zero + Zeros + Zero + Zeros
       else
         wLinha:= wLinha + IntToStrZero(0,13);

       wLinha:= wLinha                                                  +
                aTipoCobranca                                           + // Tipo de cobrança - 11, 17 (04DSC, 08VDR, 02VIN, BRANCOS) 12,31,51 (BRANCOS)
                Carteira                                                + // Carteira
                ATipoOcorrencia                                         + // Ocorrência "Comando"
                PadRight( NumeroDocumento, 10, ' ')                     + // Seu Numero - Nr. titulo dado pelo cedente
                FormatDateTime( 'ddmmyy', Vencimento )                  + // Data de vencimento
                IntToStrZero( Round( ValorDocumento * 100 ), 13)        + // Valor do titulo
                '001' + '0000' + ' '                                    + // Numero do Banco - 001 + Prefixo da agencia cobradora + DV-pref. agencia cobradora
                PadLeft(ATipoEspecieDoc, 2, '0') + ATipoAceite          + // Especie de titulo + Aceite
                FormatDateTime( 'ddmmyy', DataDocumento )               + // Data de Emissão
                AInstrucao                                              + // 1ª e 2ª instrução codificada
                IntToStrZero( round(ValorMoraJuros * 100 ), 13)         + // Juros de mora por dia
                aDataDesconto                                           + // Data limite para concessao de desconto
                IntToStrZero( round( ValorDesconto * 100), 13)          + // Valor do desconto
                IntToStrZero( round( ValorIOF * 100 ), 13)              + // Valor do IOF
                IntToStrZero( round( ValorAbatimento * 100 ), 13)       + // Valor do abatimento permitido
                ATipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')+ // Tipo de inscricao do sacado + CNPJ ou CPF do sacado
                PadRight( Sacado.NomeSacado, 37) + '   '                + // Nome do sacado + Brancos
                PadRight(trim(Sacado.Logradouro) + ', ' +
                         trim(Sacado.Numero) + ', '+
                         trim(Sacado.Complemento), 40)                  + // Endereço do sacado
                PadRight( Trim(Sacado.Bairro), 12)                      +
                PadLeft( OnlyNumber(Sacado.CEP), 8 )                    + // CEP do endereço do sacado
                PadRight( trim(Sacado.Cidade), 15)                      + // Cidade do sacado
                PadRight( Sacado.UF, 2 )                                + // UF da cidade do sacado
                PadRight( AMensagem, 40)                                + // Observações
                PadLeft(DiasProtesto,2,'0')+ ' '                        + // Número de dias para protesto + Branco
                IntToStrZero( aRemessa.Count + 1, 6 );


       wLinha:= wLinha + sLineBreak                              +
                '5'                                              + //Tipo Registro
                '99'                                             + //Tipo de Serviço (Cobrança de Multa)
                IfThen(PercentualMulta > 0, '2','9')             + //Cod. Multa 2- Percentual 9-Sem Multa
                IfThen(PercentualMulta > 0,
                       FormatDateTime('ddmmyy', DataMoraJuros),
                                      '000000')                  + //Data Multa
                IntToStrZero( round( PercentualMulta * 100), 12) + //Perc. Multa
                Space(372)                                       + //Brancos
                IntToStrZero(aRemessa.Count + 2 ,6);

       aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
     end;
   end;
end;

procedure TACBrBancoCredisis.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero(ARemessa.Count + 1, 6);   // Contador de Registros

   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoCredisis.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF: String;
  ContLinha : Integer;
  idxMotivo: Integer;
  rConvenioCedente: String;
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

   rCedente        := trim(copy(ARetorno[0], 73, 30));
   rCNPJCPF        := OnlyNumber( copy(ARetorno[0], 19, 14) );
   rConvenioCedente:= Trim(RemoveZerosEsquerda(Copy(ARetorno[0], 33, 9)));

   with ACBrBanco.ACBrBoleto do
   begin
      if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
         raise Exception.create(ACBrStr('CNPJ\CPF do arquivo inválido'));

      if LeCedenteRetorno then
      begin
        Cedente.Nome          := rCedente;
        Cedente.CNPJCPF       := rCNPJCPF;
        Cedente.Convenio      := rConvenioCedente;
        Cedente.Agencia       := trim(copy(ARetorno[0], 53, 5));
        Cedente.AgenciaDigito := trim(copy(ARetorno[0], 58, 1));
        Cedente.Conta         := trim(copy(ARetorno[0], 59, 12));
        Cedente.ContaDigito   := trim(copy(ARetorno[0], 71, 1));
      end;

      case StrToIntDef(copy(ARetorno[0], 18, 1), 0) of
        01:
          Cedente.TipoInscricao := pFisica;
        else
          Cedente.TipoInscricao := pJuridica;
      end;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   {ACBrBanco.TamanhoMaximoNossoNum := 20;}
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
            SeuNumero := copy(Linha, 106, 25);
            NumeroDocumento := copy(Linha, 59, 15);
            Carteira := copy(Linha, 58, 1);

            TempData := copy(Linha, 74, 2) + '/'+copy(Linha, 76, 2)+'/'+copy(Linha, 78, 4);
            if TempData<>'00/00/0000' then
               Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

            ValorDocumento := StrToFloatDef(copy(Linha, 82, 15), 0) / 100;
           
            {if Length(ACBrBoleto.Cedente.Convenio) = 6 then
              NossoNumero := copy(Linha, 44, 10);
            else
              NossoNumero := copy(Linha, 45, 10);}
            NossoNumero := copy(Linha, 52, 6);
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

            // quando o numero documento vier em branco
            if Trim(NumeroDocumento) = '' then
              NumeroDocumento := NossoNumero;
          end
         else if copy(Linha, 14, 1) = 'U' then // segmento U
          begin
            ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
            ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
            ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
            ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
            ValorOutrosCreditos := StrToFloatDef(copy(Linha, 123, 15), 0) / 100;
            ValorOutrasDespesas := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
            ValorRecebido       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;
            TempData := copy(Linha, 138, 2)+'/'+copy(Linha, 140, 2)+'/'+copy(Linha, 142, 4);
            if TempData<>'00/00/0000' then
              DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
            TempData := copy(Linha, 146, 2)+'/'+copy(Linha, 148, 2)+'/'+copy(Linha, 150, 4);
            if TempData<>'00/00/0000' then
              DataCredito := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
          end;
      end;
   end;

   {ACBrBanco.TamanhoMaximoNossoNum := 10;}
end;

function TACBrBancoCredisis.TipoOcorrenciaToCod (
   const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   Result := '';

   if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
   begin
     case TipoOcorrencia of
       toRetornoTransferenciaCarteiraEntrada        : Result := '04';
       toRetornoTransferenciaCarteiraBaixa          : Result := '05';
       toRetornoBaixaAutomatica                     : Result := '09';
       toRetornoBaixadoFrancoPagamento              : Result := '15';
       toRetornoLiquidadoSemRegistro                : Result := '17';
       toRetornoRecebimentoInstrucaoSustarProtesto  : Result := '20';
       toRetornoRetiradoDeCartorio                  : Result := '24';
       toRetornoBaixaPorProtesto                    : Result := '25';
       toRetornoInstrucaoRejeitada                  : Result := '26';
       toRetornoAlteracaoUsoCedente                 : Result := '27';
       toRetornoDebitoTarifas                       : Result := '28';
       toRetornoOcorrenciasDoSacado                 : Result := '29';
       toRetornoAlteracaoDadosRejeitados            : Result := '30';
       toRetornoChequePendenteCompensacao           : Result := '50';
     end;
   end
    else
    begin
      case TipoOcorrencia of
        toRetornoLiquidadoSemRegistro               : Result := '05';
        toRetornoLiquidadoPorConta                  : Result := '08';
        toRetornoLiquidadoSaldoRestante             : Result := '08';
        toRetornoBaixaSolicitada                    : Result := '10';
        toRetornoLiquidadoEmCartorio                : Result := '15';
        toRetornoConfirmacaoAlteracaoJurosMora      : Result := '16';
        toRetornoDebitoEmConta                      : Result := '20';
        toRetornoNomeSacadoAlterado                 : Result := '21';
        toRetornoEnderecoSacadoAlterado             : Result := '22';
        toRetornoProtestoSustado                    : Result := '24';
        toRetornoJurosDispensados                   : Result := '25';
        toRetornoManutencaoTituloVencido            : Result := '28';
        toRetornoDescontoConcedido                  : Result := '31';
        toRetornoDescontoCancelado                  : Result := '32';
        toRetornoDescontoRetificado                 : Result := '33';
        toRetornoAlterarDataDesconto                : Result := '34';
        toRetornoRecebimentoInstrucaoAlterarJuros   : Result := '35';
        toRetornoRecebimentoInstrucaoDispensarJuros : Result := '36';
        toRetornoDispensarIndexador                 : Result := '37';
        toRetornoDispensarPrazoLimiteRecebimento    : Result := '38';
        toRetornoAlterarPrazoLimiteRecebimento      : Result := '39';
        toRetornoChequePendenteCompensacao          : Result := '46';
        toRetornoTipoCobrancaAlterado               : Result := '72';
        toRetornoDespesasProtesto                   : Result := '96';
        toRetornoDespesasSustacaoProtesto           : Result := '97';
        toRetornoDebitoCustasAntecipadas            : Result := '98';
      end;
    end;

    if (Result <> '') then
    Exit;

    case TipoOcorrencia of
      toRetornoRegistroConfirmado                   : Result := '02';
      toRetornoRegistroRecusado                     : Result := '03';
      toRetornoLiquidado                            : Result := '06';
      toRetornoTituloEmSer                          : Result := '11';
      toRetornoAbatimentoConcedido                  : Result := '12';
      toRetornoAbatimentoCancelado                  : Result := '13';
      toRetornoVencimentoAlterado                   : Result := '14';
      toRetornoRecebimentoInstrucaoProtestar        : Result := '19';
      toRetornoEntradaEmCartorio                    : Result := '23';
      toRetornoChequeDevolvido                      : Result := '44';
    else
      Result := '02';
    end;
end;

function TACBrBancoCredisis.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin

  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      02: Result:= '02 – Entrada confirmada';
      03: Result:= '03 – Entrada Rejeitada';
      04: Result:= '04 – Transferência de Carteira/Entrada';
      05: Result:= '05 – Transferência de Carteira/Baixa';
      06: Result:= '06 – Liquidação';
      09: Result:= '09 – Baixa';
      11: Result:= '11 – Títulos em Carteira (em ser)';
      12: Result:= '12 – Confirmação Recebimento Instrução de Abatimento';
      13: Result:= '13 – Confirmação Recebimento Instrução de Cancelamento Abatimento';
      14: Result:= '14 – Confirmação Recebimento Instrução Alteração de Vencimento';
      15: Result:= '15 – Franco de Pagamento';
      17: Result:= '17 – Liquidação Após Baixa ou Liquidação Título Não Registrado';
      19: Result:= '19 – Confirmação Recebimento Instrução de Protesto';
      20: Result:= '20 – Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
      23: Result:= '23 – Remessa a Cartório';
      24: Result:= '24 – Retirada de Cartório e Manutenção em Carteira';
      25: Result:= '25 – Protestado e Baixado';
      26: Result:= '26 – Instrução Rejeitada';
      27: Result:= '27 – Confirmação do Pedido de Alteração de Outros Dados';
      28: Result:= '28 – Débito de Tarifas/Custas';
      29: Result:= '29 – Ocorrências do Sacado';
      30: Result:= '30 – Alteração de Dados Rejeitada';
      44: Result:= '44 – Título pago com cheque devolvido';
      50: Result:= '50 – Título pago com cheque pendente de compensação'
    end;
  end
  else
  begin
    case CodOcorrencia of
      02: Result:= '02-Confirmação de Entrada de Título';
      03: Result:= '03-Comando recusado';
      05: Result:= '05-Liquidado sem registro';
      06: Result:= '06-Liquidação Normal';
      07: Result:= '07-Liquidação por Conta';
      08: Result:= '08-Liquidação por Saldo';
      09: Result:= '09-Baixa de Título';
      10: Result:= '10-Baixa Solicitada';
      11: Result:= '11-Titulos em Ser';
      12: Result:= '12-Abatimento Concedido';
      13: Result:= '13-Abatimento Cancelado';
      14: Result:= '14-Alteração de Vencimento do Titulo';
      15: Result:= '15-Liquidação em Cartório';
      16: Result:= '16-Confirmação de alteração de juros de mora';
      19: Result:= '19-Confirmação de recebimento de instruções para protesto';
      20: Result:= '20-Débito em Conta';
      21: Result:= '21-Alteração do Nome do Sacado';
      22: Result:= '22-Alteração do Endereço do Sacado';
      23: Result:= '23-Indicação de encaminhamento a cartório';
      24: Result:= '24-Sustar Protesto';
      25: Result:= '25-Dispensar Juros';
      28: Result:= '28-Manutenção de titulo vencido';
      31: Result:= '31-Conceder desconto';
      32: Result:= '32-Não conceder desconto';
      33: Result:= '33-Retificar desconto';
      34: Result:= '34-Alterar data para desconto';
      35: Result:= '35-Cobrar multa';
      36: Result:= '36-Dispensar multa';
      37: Result:= '37-Dispensar indexador';
      38: Result:= '38-Dispensar prazo limite para recebimento';
      39: Result:= '39-Alterar prazo limite para recebimento';
      44: Result:= '44-Título pago com cheque devolvido';
      46: Result:= '46-Título pago com cheque, aguardando compensação';
      72: Result:= '72-Alteração de tipo de cobrança';
      96: Result:= '96-Despesas de Protesto';
      97: Result:= '97-Despesas de Sustação de Protesto';
      98: Result:= '98-Débito de Custas Antecipadas';
    end;
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoCredisis.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
   Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      15: Result := toRetornoBaixadoFrancoPagamento;
      17: Result := toRetornoLiquidadoSemRegistro;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      28: Result := toRetornoDebitoTarifas;
      29: Result := toRetornoOcorrenciasDoSacado;
      30: Result := toRetornoAlteracaoDadosRejeitados;
      50: Result := toRetornoChequePendenteCompensacao;
    end;
  end
  else
  begin
    case CodOcorrencia of
      03: Result := toRetornoComandoRecusado;
      05: Result := toRetornoLiquidadoSemRegistro;
      07: Result := toRetornoLiquidadoPorConta;
      08: Result := toRetornoLiquidadoSaldoRestante;
      10: Result := toRetornoBaixaSolicitada;
      15: Result := toRetornoLiquidadoEmCartorio;
      16: Result := toRetornoConfirmacaoAlteracaoJurosMora;
      20: Result := toRetornoDebitoEmConta;
      21: Result := toRetornoNomeSacadoAlterado;
      22: Result := toRetornoEnderecoSacadoAlterado;
      24: Result := toRetornoProtestoSustado;
      25: Result := toRetornoJurosDispensados;
      28: Result := toRetornoManutencaoTituloVencido;
      31: Result := toRetornoDescontoConcedido;
      32: Result := toRetornoDescontoCancelado;
      33: Result := toRetornoDescontoRetificado;
      34: Result := toRetornoAlterarDataDesconto;
      35: Result := toRetornoRecebimentoInstrucaoAlterarJuros;
      36: Result := toRetornoRecebimentoInstrucaoDispensarJuros;
      37: Result := toRetornoDispensarIndexador;
      38: Result := toRetornoDispensarPrazoLimiteRecebimento;
      39: Result := toRetornoAlterarPrazoLimiteRecebimento;
      46: Result := toRetornoChequePendenteCompensacao;
      72: Result := toRetornoTipoCobrancaAlterado;
      96: Result := toRetornoDespesasProtesto;
      97: Result := toRetornoProtestoSustado;
      98: Result := toRetornoDebitoCustasAntecipadas;
    end;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixaAutomatica;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    23: Result := toRetornoEntradaEmCartorio;
    44: Result := toRetornoChequeDevolvido;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoCredisis.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin

    if (ACBrBanco.ACBrBoleto.LayoutRemessa = c400) then
    begin
  case TipoOcorrencia of
    toRetornoComandoRecusado: //03 (Recusado)
      case CodMotivo of
        01: Result:='01-Identificação inválida' ;
        02: Result:='02-Variação da carteira inválida' ;
        03: Result:='03-Valor dos juros por um dia inválido' ;
        04: Result:='04-Valor do desconto inválido' ;
        05: Result:='05-Espécie de título inválida para carteira' ;
        06: Result:='06-Espécie de valor variável inválido' ;
        07: Result:='07-Prefixo da agência usuária inválido' ;
        08: Result:='08-Valor do título/apólice inválido' ;
        09: Result:='09-Data de vencimento inválida' ;
        10: Result:='10-Fora do prazo' ;
        11: Result:='11-Inexistência de margem para desconto' ;
        12: Result:='12-O Banco não tem agência na praça do sacado' ;
        13: Result:='13-Razões cadastrais' ;
        14: Result:='14-Sacado interligado com o sacador' ;
        15: Result:='15-Título sacado contra orgão do Poder Público' ;
        16: Result:='16-Título preenchido de forma irregular' ;
        17: Result:='17-Título rasurado' ;
        18: Result:='18-Endereço do sacado não localizado ou incompleto' ;
        19: Result:='19-Código do cedente inválido' ;
        20: Result:='20-Nome/endereco do cliente não informado /ECT/' ;
        21: Result:='21-Carteira inválida' ;
        22: Result:='22-Quantidade de valor variável inválida' ;
        23: Result:='23-Faixa nosso número excedida' ;
        24: Result:='24-Valor do abatimento inválido' ;
        25: Result:='25-Novo número do título dado pelo cedente inválido' ;
        26: Result:='26-Valor do IOF de seguro inválido' ;
        27: Result:='27-Nome do sacado/cedente inválido ou não informado' ;
        28: Result:='28-Data do novo vencimento inválida' ;
        29: Result:='29-Endereco não informado' ;
        30: Result:='30-Registro de título já liquidado' ;
        31: Result:='31-Numero do bordero inválido' ;
        32: Result:='32-Nome da pessoa autorizada inválido' ;
        33: Result:='33-Nosso número já existente' ;
        34: Result:='34-Numero da prestação do contrato inválido' ;
        35: Result:='35-Percentual de desconto inválido' ;
        36: Result:='36-Dias para fichamento de protesto inválido' ;
        37: Result:='37-Data de emissão do título inválida' ;
        38: Result:='38-Data do vencimento anterior a data da emissão do título' ;
        39: Result:='39-Comando de alteração indevido para a carteira' ;
        40: Result:='40-Tipo de moeda inválido' ;
        41: Result:='41-Abatimento não permitido' ;
        42: Result:='42-CEP do sacado inválido /ECT/' ;
        43: Result:='43-Codigo de unidade variavel incompativel com a data emissão do título' ;
        44: Result:='44-Dados para debito ao sacado inválidos' ;
        45: Result:='45-Carteira' ;
        46: Result:='46-Convenio encerrado' ;
        47: Result:='47-Título tem valor diverso do informado' ;
        48: Result:='48-Motivo de baixa inválido para a carteira' ;
        49: Result:='49-Abatimento a cancelar não consta do título' ;
        50: Result:='50-Comando incompativel com a carteira' ;
        51: Result:='51-Codigo do convenente inválido' ;
        52: Result:='52-Abatimento igual ou maior que o valor do título' ;
        53: Result:='53-Título já se encontra situação pretendida' ;
        54: Result:='54-Título fora do prazo admitido para a conta 1' ;
        55: Result:='55-Novo vencimento fora dos limites da carteira' ;
        56: Result:='56-Título não pertence ao convenente' ;
        57: Result:='57-Variação incompativel com a carteira' ;
        58: Result:='58-Impossivel a transferencia para a carteira indicada' ;
        59: Result:='59-Título vencido em transferencia para a carteira 51' ;
        60: Result:='60-Título com prazo superior a 179 dias em transferencia para carteira 51' ;
        61: Result:='61-Título já foi fichado para protesto' ;
        62: Result:='62-Alteração da situação de debito inválida para o codigo de responsabilidade' ;
        63: Result:='63-DV do nosso número inválido' ;
        64: Result:='64-Título não passivel de debito/baixa - situação anormal' ;
        65: Result:='65-Título com ordem de não protestar-não pode ser encaminhado a cartorio' ;
        66: Result:= '66-Número do documento do sacado (CNPJ/CPF) inválido';
        67: Result:='66-Título/carne rejeitado' ;
        68: Result:= '68-Código/Data/Percentual de multa inválido';
        69: Result:= '69-Valor/Percentual de Juros Inválido';
        70: Result:= '70-Título já se encontra isento de juros';
        71: Result:= '71-Código de Juros Inválido';
        72: Result:= '72-Prefixo da Ag. cobradora inválido';
        73: Result:= '73–Numero do controle do participante inválido';
        74: Result:= '74–Cliente não cadastrado no CIOPE (Desconto/Vendor)';
        75: Result:= '75–Qtde. de dias do prazo limite p/ recebimento de título vencido inválido';
        76: Result:= '76–Titulo excluído automaticamente por decurso deprazo CIOPE (Desconto/Vendor)';
        77: Result:= '77–Titulo vencido transferido para a conta 1 – Carteira vinculada'; 
        80: Result:='80-Nosso número inválido' ;
        81: Result:='81-Data para concessão do desconto inválida' ;
        82: Result:='82-CEP do sacado inválido' ;
        83: Result:='83-Carteira/variação não localizada no cedente' ;
        84: Result:='84-Título não localizado na existencia' ;
        99: Result:='99-Outros motivos' ;
      end;
    toRetornoLiquidadoSemRegistro, toRetornoLiquidado, toRetornoLiquidadoPorConta,
       toRetornoLiquidadoSaldoRestante, toRetornoLiquidadoEmCartorio: // 05, 06, 07, 08 e 15 (Liquidado)
      case CodMotivo of
        01: Result:='01-Liquidação normal';
        02: Result:='02-Liquidação parcial';
        03: Result:='03-Liquidação por saldo';
        04: Result:='04-Liquidação com cheque a compensar';
        05: Result:='05-Liquidação de título sem registro (carteira 7 tipo 4)';
        07: Result:='07-Liquidação na apresentação';
        09: Result:='09-Liquidação em cartório';
      end;
    toRetornoTituloPagoEmCheque,    // 46–Título pago com cheque, aguardando compensação
    toRetornoTipoCobrancaAlterado:  // 72 (Tipo de Cobrança)
      case CodMotivo of
        00: Result := '00-Transferência de título de cobrança simples para descontada ou vice-versa';
        52: Result := '52-Reembolso de título vendor ou descontado';
      end;
      end;    
    end
    else
      begin
        case TipoOcorrencia of
        toRetornoRegistroRecusado: // 03 (Recusado)
      case CodMotivo of
          01: Result:='Codigo do banco invalido';
          02: Result:='Codigo do registro detalhe invalido';
          03: Result:='Codigo do segmento invalido';
          04: Result:='Codigo do movimento nao permitido para carteira';
          05: Result:='Codigo de movimento invalido';
          06: Result:='Tipo/numero de inscricao do cedente Invalidos';
          07: Result:='Agencia/Conta/DV invalido';
          08: Result:='Nosso numero invalido';
          09: Result:='Nosso numero duplicado';
          10: Result:='Carteira invalida';
          11: Result:='Forma de cadastramento do titulo invalido';
          12: Result:='Tipo de documento invalido';
          13: Result:='Identificacao da emissao do bloqueto invalida';
          14: Result:='Identificacao da distribuicao do bloqueto invalida';
          15: Result:='Caracteristicas da cobranca incompativeis';
          16: Result:='Data de vencimento invalida';
          17: Result:='Data de vencimento anterior a data de emissao';
          18: Result:='Vencimento fora do prazo de operacao';
          19: Result:='Titulo a cargo de Bancos Correspondentes com vencimento inferior XX dias';
          20: Result:='Valor do titulo invalido';
          21: Result:='Especie do titulo invalida';
          22: Result:='Especie nao permitida para a carteira';
          23: Result:='Aceite invalido';
          24: Result:='Data da emissao invalida';
          25: Result:='Data da emissao posterior a data';
          26: Result:='Codigo de juros de mora invalido';
          27: Result:='Valor/Taxa de juros de mora invalido';
          28: Result:='Codigo do desconto invalido';
          29: Result:='Valor do desconto maior ou igual ao valor do titulo ';
          30: Result:='Desconto a conceder nao confere';
          31: Result:='Concessao de desconto - ja existe desconto anterior';
          32: Result:='Valor do IOF invalido';
          33: Result:='Valor do abatimento invalido';
          34: Result:='Valor do abatimento maior ou igual ao valor do titulo';
          35: Result:='Abatimento a conceder nao confere';
          36: Result:='Concessao de abatimento - ja existe abatimento anterior';
          37: Result:='Codigo para protesto invalido';
          38: Result:='Prazo para protesto invalido';
          39: Result:='Pedido de protesto nao permitido para o titulo';
          40: Result:='Titulo com ordem de protesto emitida';
          41: Result:='Pedido de cancelamento/sustacao para titulos sem instrucao de protesto';
          42: Result:='Codigo para baixa/devolucao invalido';
          43: Result:='Prazo para baixa/devolucao invalido';
          44: Result:='Codigo da moeda invalido';
          45: Result:='Nome do sacado nao informado';
          46: Result:='Tipo/numero de inscricao do sacado invalidos';
          47: Result:='Endereco do sacado nao informado';
          48: Result:='CEP invalido';
          49: Result:='CEP sem praca de cobranca /nao localizado';
          50: Result:='CEP referente a um Banco Correspondente';
          51: Result:='CEP incompativel com a unidade da federacao';
          52: Result:='Unidade da federacao invalida';
          53: Result:='Tipo/numero de inscricao do sacador/avalista invalidos';
          54: Result:='Sacador/Avalista nao informado';
          55: Result:='Nosso numero no Banco Correspondente nao informado';
          56: Result:='Codigo do Banco Correspondente nao informado';
          57: Result:='Codigo da multa invalido';
          58: Result:='Data da multa invalida';
          59: Result:='Valor/Percentual da multa invalido';
          60: Result:='Movimento para titulo nao cadastrado';
          61: Result:='Alteracao da agencia cobradora/dv invalida';
          62: Result:='Tipo de impressao invalido';
          63: Result:='Entrada para titulo ja cadastrado';
          64: Result:='Numero da linha invalido';
          65: Result:='Codigo do banco para debito invalido';
          66: Result:='Agencia/conta/DV para debito invalido';
          67: Result:='Dados para debito incompativel com a identificacao da emissao do bloqueto';
          88: Result:='Arquivo em duplicidade';
          99: Result:='Contrato inexistente';
          end;
        toRetornoLiquidado, toRetornoBaixaAutomatica, toRetornoLiquidadoSemRegistro: // 06, 09 e 17 (Liquidado)
        case CodMotivo of
          01: Result:='Por saldo';
          02: Result:='Parcial';
          03: Result:='No proprio banco';
          04: Result:='Compensacao eletronica';
          05: Result:='Compensacao convencional';
          06: Result:='Por meio eletronico';
          07: Result:='Apos feriado local';
          08: Result:='Em cartorio';
          30: Result:='Liquidação no Guichê de Caixa em cheque';
          09: Result:='Comandada banco';
          10: Result:='Comandada cliente arquivo';
          11: Result:='Comandada cliente on-line';
          12: Result:='Decurso prazo - cliente';
          13: Result:='Decurso prazo - banco';
      end;
    toRetornoDebitoTarifas: // 28 - Débito de Tarifas/Custas (Febraban 240 posições, v08.9 de 15/04/2014)
      case CodMotivo of
        01: Result:='01-Tarifa de Extrato de Posição';
        02: Result:='02-Tarifa de Manutenção de Título Vencido';
        03: Result:='03-Tarifa de Sustação';
        04: Result:='04-Tarifa de Protesto';
        05: Result:='05-Tarifa de Outras Instruções';
        06: Result:='06-Tarifa de Outras Ocorrências';
        07: Result:='07-Tarifa de Envio de Duplicata ao Sacado';
        08: Result:='08-Custas de Protesto';
        09: Result:='09-Custas de Sustação de Protesto';
        10: Result:='10-Custas de Cartório Distribuidor';
        11: Result:='11-Custas de Edital';
        12: Result:='12-Tarifa Sobre Devolução de Título Vencido';
        13: Result:='13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
        14: Result:='14-Tarifa Sobre Reapresentação Automática';
        15: Result:='15-Tarifa Sobre Rateio de Crédito';
        16: Result:='16-Tarifa Sobre Informações Via Fax';
        17: Result:='17-Tarifa Sobre Prorrogação de Vencimento';
        18: Result:='18-Tarifa Sobre Alteração de Abatimento/Desconto';
        19: Result:='19-Tarifa Sobre Arquivo mensal (Em Ser)';
        20: Result:='20-Tarifa Sobre Emissão de Bloqueto Pré-Emitido pelo Banco';
      end;         
      end;
    end;
    case TipoOcorrencia of
    toRetornoRegistroConfirmado:       //02 (Entrada)
    case CodMotivo of
        00: Result:='00-Por meio magnético';
        11: Result:='11-Por via convencional';
        16: Result:='16-Por alteração do código do cedente';
        17: Result:='17-Por alteração da variação';
        18: Result:='18-Por alteração de carteira';
      end;    
    toRetornoBaixaAutomatica, toRetornoBaixaSolicitada, toRetornoDebitoEmConta: // 09, 10 ou 20 (Baixa)
      case CodMotivo of
        00: Result:='00-Solicitada pelo cliente';
        14: Result:='14-Protestado';
        15: 
          case ACBrBanco.ACBrBoleto.LayoutRemessa of
            c240: Result := '15-Título Excluído';
            c400: Result := '15-Protestado';
          end;
        18: Result:='18-Por alteração de carteira';
        19: Result:='19-Débito automático';
        31: Result:='31-Liquidado anteriormente';
        32: Result:='32-Habilitado em processo';
        33: Result:='33-Incobrável por nosso intermédio';
        34: Result:='34-Transferido para créditos em liquidação';
        46: Result:='46-Por alteração da variação';
        47: Result:='47-Por alteração da variação';
        51: Result:='51-Acerto';
        90: Result:='90-Baixa automática';
    end;
  end;

  Result := ACBrSTr(Result);
end;

procedure TACBrBancoCredisis.LerRetorno400(ARetorno: TStringList);
var
 TamConvenioMaior6: Boolean;
begin
 TamConvenioMaior6:= Length(trim(ACBrBanco.ACBrBoleto.Cedente.Convenio)) > 6;
 if TamConvenioMaior6 then
    LerRetorno400Pos7(ARetorno)
 else
    LerRetorno400Pos6(ARetorno);
end;

procedure TACBrBancoCredisis.LerRetorno400Pos6(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo, MotivoLinha : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
  rConvenioCedente: String;
begin
   fpTamanhoMaximoNossoNum := 11;

   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
     raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                    'não é um arquivo de retorno do '+ Nome));

   rCedente      := trim(Copy(ARetorno[0],47,30));
   rAgencia      := trim(Copy(ARetorno[0],27,4));
   rDigitoAgencia:= Copy(ARetorno[0],31,1);
   rConta        := trim(Copy(ARetorno[0],32,8));
   rDigitoConta  := Copy(ARetorno[0],40,1);

   rCodigoCedente  := Copy(ARetorno[0],41,6);
   rConvenioCedente:= Copy(ARetorno[0],41,6);


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],101,7),0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   ValidarDadosRetorno(rAgencia, rConta);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
       Cedente.Nome         := rCedente;
       Cedente.Agencia      := rAgencia;
       Cedente.AgenciaDigito:= rDigitoAgencia;
       Cedente.Conta        := rConta;
       Cedente.ContaDigito  := rDigitoConta;
       Cedente.CodigoCedente:= rCodigoCedente;
       Cedente.Convenio     := rConvenioCedente;
     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   ACBrBanco.TamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
     Linha := ARetorno[ContLinha] ;

     if (Copy(Linha,1,1) <> '1') then
       Continue;

     Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     with Titulo do
     begin
       SeuNumero               := copy(Linha,38,25);
       NumeroDocumento         := copy(Linha,117,10);

       CodOcorrencia := StrToIntDef(copy(Linha,109,2),0);
       OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

       if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or
          (CodOcorrencia = 15) or (CodOcorrencia = 46) then
       begin
         CodigoLiquidacao := IntToStr(CodOcorrencia);
         CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
       end;

       if(CodOcorrencia >= 2) and (CodOcorrencia <= 10) then
       begin
         MotivoLinha:= 81;
         CodMotivo:= StrToInt(copy(Linha,MotivoLinha,2));
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
       ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
       ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
       ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
       Carteira             := Copy(Linha,107,2);
       NossoNumero          := Copy(Linha,63,11);
       ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
       ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

       if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
         DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                            Copy(Linha,178,2)+'/'+
                                            Copy(Linha,180,2),0, 'DD/MM/YY' );
     end;
   end;

   fpTamanhoMaximoNossoNum := 10;
end;

procedure TACBrBancoCredisis.LerRetorno400Pos7(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
  rConvenioCedente: String;
begin
  fpTamanhoMaximoNossoNum := 20;

  if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                   'não é um arquivo de retorno do '+ Nome));

  rCedente      := trim(Copy(ARetorno[0],47,30));
  rAgencia      := trim(Copy(ARetorno[0],27,4));
  rDigitoAgencia:= Copy(ARetorno[0],31,1);
  rConta        := trim(Copy(ARetorno[0],32,8));
  rDigitoConta  := Copy(ARetorno[0],40,1);

  rCodigoCedente  := Copy(ARetorno[0],150,7);
  rConvenioCedente:= Copy(ARetorno[0],150,7);


  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],101,7),0);

  ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                            Copy(ARetorno[0],97,2)+'/'+
                                                            Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

  ValidarDadosRetorno(rAgencia, rConta);
  with ACBrBanco.ACBrBoleto do
  begin
    if LeCedenteRetorno then
    begin
      Cedente.Nome         := rCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;
      Cedente.CodigoCedente:= rCodigoCedente;
      Cedente.Convenio     := rConvenioCedente;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  ACBrBanco.TamanhoMaximoNossoNum := 20;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha] ;

    if (Copy(Linha,1,1) <> '7') and (Copy(Linha,1,1) <> '1') then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      SeuNumero       := copy(Linha,39,25);
      NumeroDocumento := copy(Linha,117,10);
      CodOcorrencia   := StrToIntDef(copy(Linha,109,2),0);
      OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

      if ((CodOcorrencia >= 5) and (CodOcorrencia <= 8)) or
          (CodOcorrencia = 15) or (CodOcorrencia = 46) then
      begin
        CodigoLiquidacao := copy(Linha,109,2);
        CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(OcorrenciaOriginal.Tipo);
      end;

      if(CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
      begin

        CodMotivo:= StrToIntDef(Copy(Linha,87,2),0);
        MotivoRejeicaoComando.Add(copy(Linha,87,2));
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
      ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
      ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
      ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
      Carteira             := Copy(Linha,107,2);
      NossoNumero          := Copy(Linha,71,10);
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100;
      ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

      if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
        DataCredito:= StringToDateTimeDef( Copy(Linha,176,2)+'/'+
                                           Copy(Linha,178,2)+'/'+
                                           Copy(Linha,180,2),0, 'DD/MM/YY' );
    end;
  end;
  fpTamanhoMaximoNossoNum := 10;
end;



end.
