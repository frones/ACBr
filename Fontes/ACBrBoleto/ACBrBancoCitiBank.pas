{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Luiz Carlos Rodrigues                         }
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

unit ACBrBancoCitiBank;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto;

type

  { TACBrBancoCitiBank }
  TACBrBancoCitiBank = class(TACBrBancoClass)
  private
    fValorTotalDocs: Double;
  protected
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero( const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa: TStringList): String; override;

  end;

implementation

uses StrUtils, Variants,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  ACBrUtil;

constructor TACBrBancoCitiBank.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpDigito := 5;
  fpNome   := 'CITIBANK';
  fpNumero := 745;
  fpTamanhoMaximoNossoNum := 11;
  fpTamanhoAgencia := 5;
  fpTamanhoConta   := 12;
  fpTamanhoCarteira:= 3;
  fValorTotalDocs  := 0;
end;

function TACBrBancoCitiBank.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
begin
  Modulo.CalculoPadrao;
  Modulo.MultiplicadorFinal := 2;
  Modulo.MultiplicadorFinal := 9;
  Modulo.Documento := ACBrTitulo.NossoNumero;
  Modulo.Calcular;

  Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoCitiBank.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var CodigoBarras, DigitoCodBarras, FatorVencimento, DVNossoNumero, CampoLivre: String;
begin
  FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);
  DVNossoNumero   := CalcularDigitoVerificador(ACBrTitulo);

  {Montando Campo Livre}
  CampoLivre := OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta+ACBrTitulo.ACBrBoleto.Cedente.ContaDigito);
  CampoLivre := '3'+                                                            // Cobrança com registro / sem registro.
                PadLeft(ACBrTitulo.Carteira, 3,'0')+                             // Carteira/Portfólio
                Copy(CampoLivre,2,9)+                                           // Base+Sequência+Dígito da Conta Cosmos
                Copy(ACBrTitulo.NossoNumero+DVNossoNumero,1,12);


  {Codigo de Barras}
  with ACBrTitulo.ACBrBoleto do
  begin
    CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                    '9' +
                    FatorVencimento +
                    IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                    CampoLivre;
  end;

  DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  Result := Copy(CodigoBarras, 1, 4) + DigitoCodBarras + Copy(CodigoBarras, 5, 44);
end;

function TACBrBancoCitiBank.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  Result := RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia,5)+
            IfThen(Trim(ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito) <> '','-','')+
            ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
            ACBrTitulo.ACBrBoleto.Cedente.Conta+
            IfThen(Trim(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito) <> '','-','')+
            ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoCitiBank.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.NossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoCitiBank.GerarRegistroHeader240(NumeroRemessa: Integer): String;
var
  ATipoInscricao: string;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case TipoInscricao of
      pFisica  : ATipoInscricao := '1';
      pJuridica: ATipoInscricao := '2';
    end;

    { GERAR REGISTRO-HEADER DO ARQUIVO }

    Result:= IntToStrZero(ACBrBanco.Numero, 3)       + //1 a 3 - Código do banco
             '0000'                                  + //4 a 7 - Lote de serviço
             '0'                                     + //8 - Tipo de registro - Registro header de arquivo
             Space(9)                                + //9 a 17 Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                          + //18 - Tipo de inscrição do cedente
             PadLeft(OnlyNumber(CNPJCPF), 14, '0')   + //19 a 32 -Número de inscrição do cedente
             PadRight(CodigoCedente, 20, ' ')        + //33 a 52 - Código do convênio no banco
             PadLeft('', 5, '0')                     + //53 a 57 - Código da agência mantenedora da conta (Zeros)
             ' '                                     + //58 - Dígito da agência (Branco)
             PadLeft(OnlyNumber(Conta), 12, '0')     + //59 a 70 - Número da conta cosmos
             ' '                                     + //071 a 071 - DV Conta
             ' '                                     + //72 a 72 - Dígito verificador da ag
             PadRight(Nome, 30)                      + //73  a 102 - Nome da empresa
             PadRight('CITIBANK', 30, ' ')           + //103 a 132 - Nome do banco
             Space(10)                               + //133 a 142 - Uso exclusivo FEBRABAN/CNAB
             '1'                                     + //143 - Código de Remessa (1) / Retorno (2)
             FormatDateTime('ddmmyyyy', Now)         + //144 a 151 - Data do de geração do arquivo
             FormatDateTime('hhmmss', Now)           + //152 a 157 - Hora de geração do arquivo
             PadLeft(IntToStr(NumeroRemessa), 6, '0')+ //158 a 163 - Número seqüencial do arquivo
             '040'                                   + //164 a 166 - Número da versão do layout do arquivo
             '01600'                                 + //167 a 171 - Densidade de gravação do arquivo = "01600"
             Space(20)                               + //172 a 191 - Para uso reservado do banco
             Space(20)                               + //192 a 211 - Para uso reservado da empresa
             Space(29);                                // 212 a 240 - Uso exclusivo FEBRABAN/CNAB

    { GERAR REGISTRO HEADER DO LOTE }

    Result:= Result + #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)       + //1 a 3 - Código do banco
             '0001'                                  + //4 a 7 - Lote de serviço
             '1'                                     + //8 - Tipo de registro = "1" HEADER LOTE
             'R'                                     + //9 - Tipo de operação: R (Remessa) ou T (Retorno)
             '01'                                    + //10 a 11 - Tipo de serviço: 01 (Cobrança)
             Space(2)                                + //012 a 013 - Uso exclusivo FEBRABAN/CNAB
             '030'                                   + //14 a 16 - Número da versão do layout do lote
             ' '                                     + //17 - Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                          + //18 - Tipo de inscrição do cedente
             PadLeft(OnlyNumber(CNPJCPF), 15, '0')   + //19 a 33 -Número de inscrição do cedente
             PadRight(CodigoCedente, 20, ' ')        + //34 a 53 - Código do convênio no banco
             PadLeft('', 5, '0')                     + //54 a 58 - Código da agência mantenedora da conta (Zeros)
             ' '                                     + //59 - Dígito da agência (Branco)
             PadLeft(OnlyNumber(Conta), 12, '0')     + //60 a 71 - Número da conta cosmos
             ' '                                     + //72 a 72 - DV Conta
             ' '                                     + //73 a 73 - Dígito verificador da ag
             PadRight(Nome, 30, ' ')                 + //74  a 103 - Nome do cedente
             Space(40)                               + //104 a 143 - Mensagem 1
             Space(40)                               + //144 a 183 - Mensagem 2
             '00000000'                              + //184 a 191 - Número remessa/retorno
             FormatDateTime('ddmmyyyy', Now)         + //192 a 199 - Data de gravação rem./ret.
             PadRight('', 8, '0')                    + //200 a 207 - Data do crédito - Só para arquivo retorno
             PadRight('', 33, ' ');                    //208 a 240 - Uso exclusivo FEBRABAN/CNAB
  end;
end;

function TACBrBancoCitiBank.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
  ATipoOcorrencia, ATipoBoleto, CodProtesto, DiasProtesto: String;
  DigitoNossoNumero, ATipoAceite, AEspecieDoc, TipoSacado, EndSacado: String;
  TipoAvalista: Char;
begin
  with ACBrTitulo do
  begin
    {Nosso Número}
    DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

    {Aceite}
    case Aceite of
      atSim: ATipoAceite := 'A';
      atNao: ATipoAceite := 'N';
    end;

    {Espécie}
    if AnsiSameText(EspecieDoc, 'DM') then
      AEspecieDoc := '03'
    else if AnsiSameText(EspecieDoc, 'DMI') then
      AEspecieDoc := '03'
    else
      AEspecieDoc := '99';

    {Pegando o Tipo de Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar             : ATipoOcorrencia := '02';
      toRemessaConcederAbatimento : ATipoOcorrencia := '04';
      toRemessaAlterarVencimento  : ATipoOcorrencia := '06';
      toRemessaConcederDesconto   : ATipoOcorrencia := '07';
      toRemessaProtestar          : ATipoOcorrencia := '09';
      toRemessaSustarProtesto     : ATipoOcorrencia := '11';
    else
      ATipoOcorrencia := '01';
    end;

    {Protesto}
    CodProtesto := '3';
    DiasProtesto := '00';
    if (DataProtesto > 0) and (DataProtesto > Vencimento) then
    begin
      CodProtesto := '1';
      DiasProtesto := PadLeft(IntToStr(DaysBetween(DataProtesto, Vencimento)), 2, '0');
    end;

    {Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
    case ACBrBoleto.Cedente.ResponEmissao of
       tbBancoEmite : ATipoBoleto := '1';
       tbCliEmite   : ATipoBoleto := '2';
    end;

    {Sacado}
    case Sacado.Pessoa of
      pFisica:   TipoSacado := '1';
      pJuridica: TipoSacado := '2';
    else
      TipoSacado := '9';
    end;

    EndSacado := Sacado.Logradouro;
    if (Sacado.Numero <> '') then
      EndSacado := EndSacado + ', ' + Sacado.Numero;
    EndSacado := PadRight(trim(EndSacado), 40);

    {Avalista}
    if PadRight(Sacado.SacadoAvalista.CNPJCPF, 15, '0') = PadRight('0', 15, '0') then
      TipoAvalista := '0'
    else
      case Sacado.SacadoAvalista.Pessoa of
        pFisica:   TipoAvalista := '1';
        pJuridica: TipoAvalista := '2';
      else
        TipoAvalista := '9';
      end;

    {SEGMENTO P}
    fValorTotalDocs:= fValorTotalDocs  + ValorDocumento;
    Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
             '0001'                                                     + //4 a 7 - Lote de serviço
             '3'                                                        + //8 - Tipo do registro: Registro detalhe
             IntToStrZero((2 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+1,5) + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
             'P'                                                        + //14 - Código do segmento do registro detalhe
             ' '                                                        + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
             ATipoOcorrencia                                            + //16 a 17 - Código de movimento
             PadLeft('', 5, '0')                                        + //18 a 22 - Agência mantenedora da conta (Zeros)
             ' '                                                        + //23 -Dígito verificador da agência
             PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta), 12, '0')     + //24 a 35 - Número da conta cosmos
             ' '                                                        + //36 a 36 - Dígito verificador da conta
             ' '                                                        + //37 a 37 - Dígito verificador da ag/conta
             PadRight(NossoNumero + DigitoNossoNumero, 12, ' ')         + //38 a 49 - Identificação do título no banco
             Space(8)                                                   + //50 a 57 - Brancos
             Copy(ACBrBoleto.Cedente.Modalidade+' ',1,1)                + //58 - Código da Carteira (característica dos títulos dentro das modalidades de cobrança: '1' = Cobrança Simples '3' = Cobrança Caucionada)
             '1'                                                        + //59 - Forma de cadastramento do título no banco (1=Cobrança Registrada, 2=Cobrança sem Registro)
             ' '                                                        + //60 - Tipo de documento: Brancos
             ATipoBoleto                                                + //61 - Quem emite
             ' '                                                        + //62 - Quem distribui
             PadRight(NumeroDocumento, 10)                              + //63 a 72 - Nº do documento de cobrança
             Space(5)                                                   + //73 a 77 - Brancos
             FormatDateTime('ddmmyyyy', Vencimento)                     + //78 a 85 - Data de vencimento do título
             IntToStrZero( Round( ValorDocumento * 100), 15)            + //86 a 100 - Valor nominal do título
             '00000'                                                    + //101 a 105 - Agência cobradora. Se ficar em branco, o CITIBANK determina automaticamente pelo CEP do sacado
             ' '                                                        + //106 - Dígito da agência cobradora
             PadRight(AEspecieDoc, 2)                                   + //107 a 108 - Espécie do documento
             ATipoAceite                                                + //109 - Identificação de título Aceito / Não aceito
             FormatDateTime('ddmmyyyy', DataDocumento)                  + //110 a 117 - Data da emissão do documento
             '1'                                                        + //118 - Código do juro de mora
             '00000000'                                                 + //119 a 126 - Data do juro de mora
             IntToStrZero(Round(ValorMoraJuros * 100), 15)              + //127 a 141 - Juros de mora por dia/taxa
             '1'                                                        + //142 a 142 - Código do desconto 1
             IfThen(ValorDesconto = 0, '00000000', FormatDateTime('ddmmyyyy', Vencimento)) + // 143 a 150 - Data do desconto 1
             IntToStrZero(Round(ValorDesconto * 100), 15)               + //151 a 165 - Valor percentual a ser concedido
             IntToStrZero(Round(ValorIOF * 100), 15)                    + //166 a 180 - Valor do IOF a ser recolhido
             IntToStrZero(Round(ValorAbatimento * 100), 15)             + //181 a 195 - Valor do abatimento
             PadRight(NumeroDocumento, 25)                              + //196 a 220 - Identificação do título na empresa
             CodProtesto                                                + //221 a 221 - Código para protesto
             DiasProtesto                                               + //222 a 223 - Número de dias para protesto
             IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento), '1', '2') + //224 - Código para baixa/devolução: Não baixar/não devolver
             '   '                                                      + //225 a 227 - Brancos
             '09'                                                       + //228 a 229 - Código da moeda: Real
             PadRight('', 10 , '0')                                     + //230 a 239 - Uso Exclusivo CITIBANK
             ' ';                                                         //240 - Uso exclusivo FEBRABAN/CNAB

    {SEGMENTO Q}
    Result:= Result + #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)                          + //1 a 3 - Código do banco
             '0001'                                                     + //4 a 7 - Número do lote
             '3'                                                        + //8 - Tipo do registro: Registro detalhe
             IntToStrZero((2 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 2 ,5) + //9 a 13 - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
             'Q'                                                        + //14 - Código do segmento do registro detalhe
             ' '                                                        + //15 - Uso exclusivo FEBRABAN/CNAB: Branco
             ATipoOcorrencia                                            + //16 a 17 - Código de movimento
             TipoSacado                                                 + //018 - Tipo de inscrição
             PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')               + //19 a 33 - Número de Inscrição
             PadRight(Sacado.NomeSacado, 40, ' ')                       + //34 a 73 - Nome sacado
             EndSacado                                                  + //74 a 113 - Endereço
             PadRight(Sacado.Bairro, 15, ' ')                           + //114 a 128 - bairro sacado
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),1,5)            + //129 a 133 - CEP
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),6,3)            + //134 a 136 - Sufixo do CEP
             PadRight(Sacado.Cidade, 15, ' ')                           + //137 a 151 - cidade sacado
             PadRight(Sacado.UF, 2, ' ')                                + //152 a 153 - UF sacado
             TipoAvalista                                               + //154 a 154 - Tipo de inscrição sacador/avalista
             PadRight(Sacado.SacadoAvalista.CNPJCPF, 15, '0')           + //155 a 169 - Número de inscrição
             PadRight(Sacado.SacadoAvalista.NomeAvalista,40,' ')        + //170 a 209 - Nome do sacador/avalista
             PadRight('', 3, '0')                                       + //210 a 212 - Portfolio de Cobrança Simples
             PadRight('', 3, '0')                                       + //213 a 215 - Número do carnê
             PadRight('', 3, '0')                                       + //216 a 218 - Número da parcela
             Space(14)                                                  + //219 a 232 - Brancos
             Space(8);                                                    //233 a 240 - Uso exclusivo FEBRABAN/CNAB
  end;
end;

function TACBrBancoCitiBank.GerarRegistroTrailler240( ARemessa : TStringList ): String;
var
  wQTDTitulos: Integer;
begin
  wQTDTitulos := ARemessa.Count - 1;
  {REGISTRO TRAILER DO LOTE}
  Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
           '0001'                                                     + //Lote de Serviço
           '5'                                                        + //Tipo do registro: Registro trailer do lote
           Space(9)                                                   + //Uso exclusivo FEBRABAN/CNAB
           IntToStrZero((2 * wQTDTitulos + 2 ), 6)                    + //Quantidade de Registro no Lote (Registros P,Q header e trailer do lote)
           IntToStrZero((wQTDTitulos), 6)                             + //Quantidade títulos em cobrança
           IntToStrZero(Round(fValorTotalDocs * 100), 17)             + //Valor dos títulos em carteiras}
           PadRight('', 6, '0')                                       + //Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + //Valor dos títulos em carteiras}
           PadRight('',6,  '0')                                       + //Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + //Quantidade de Títulos em Carteiras
           PadRight('',6,  '0')                                       + //Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + //Quantidade de Títulos em Carteiras
           Space(8)                                                   + //Número do aviso de lançamento
           Space(117);                                                  //Uso exclusivo FEBRABAN/CNAB

  {GERAR REGISTRO TRAILER DO ARQUIVO}
  Result:= Result + #13#10 +
           IntToStrZero(ACBrBanco.Numero, 3)                          + //Código do banco
           '9999'                                                     + //Lote de serviço
           '9'                                                        + //Tipo do registro: Registro trailer do arquivo
           PadRight('',9,' ')                                         + //Uso exclusivo FEBRABAN/CNAB}
           '000001'                                                   + //Quantidade de lotes do arquivo (Registros P,Q header e trailer do lote e do arquivo)
           IntToStrZero((2 * wQTDTitulos)+4, 6)                       + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
           PadRight('',6,'0')                                         + //Uso exclusivo FEBRABAN/CNAB}
           PadRight('',205,' ');                                        //Uso exclusivo FEBRABAN/CNAB}
end;


end.
