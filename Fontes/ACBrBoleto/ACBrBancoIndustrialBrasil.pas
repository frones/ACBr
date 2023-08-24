{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaaa                     }
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
//Incluido em: 30/06/2023

{$I ACBr.inc}
unit ACBrBancoIndustrialBrasil;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  ACBrBoleto,
  ACBrBoletoConversao;

type
  { TACBrBancoIndustrialBrasil }
  TACBrBancoIndustrialBrasil = class(TACBrBancoClass)
  protected
    function GetLocalPagamento: String; override;
  private
    procedure MontarRegistroMensagem400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
    procedure MontarRegistroBeneficiarioFinal400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
    procedure GerarRegistrosNFe(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
    function DefineCarteira(const ACBrTitulo: TACBrTitulo): String;
  protected
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String; override;
  end;

implementation

uses
  ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  StrUtils,
  Variants,
  DateUtils,
  ACBrUtil.Strings;

constructor TACBrBancoIndustrialBrasil.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpNome                  := 'BCO INDUSTRIAL';
  fpNumero                := 604;
  fpDigito                := 1;
  fpTamanhoMaximoNossoNum := 10;
  fpTamanhoAgencia        := 4;
  fpTamanhoConta          := 7;
  fpTamanhoCarteira       := 3;
end;

function TACBrBancoIndustrialBrasil.CalcularDigitoVerificador(
  const ACBrTitulo: TACBrTitulo): string;
begin
  Modulo.FormulaDigito        := frModulo10;
  Modulo.MultiplicadorFinal   := 1;
  Modulo.MultiplicadorInicial := 2;
  Modulo.Documento :=
    ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.Carteira
    + PadLeft(ACBrTitulo.NossoNumero, fpTamanhoMaximoNossoNum, '0');
  Modulo.Calcular;
  Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoIndustrialBrasil.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
var
  NossoNr, DV: string;
begin
  if not((ACBrTitulo.Carteira = '121') or (ACBrTitulo.Carteira = '110')) then
    raise Exception.Create( ACBrStr('Carteira Inválida.'+sLineBreak+'Utilize "121 ou 110"') ) ;

  NossoNr := PadLeft(ACBrTitulo.NossoNumero, 10, '0');
  DV := CalcularDigitoVerificador(ACBrTitulo);
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
    + '/' + ACBrTitulo.Carteira
    + '/' + NossoNr
    + '-' + DV;
end;

function TACBrBancoIndustrialBrasil.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
    + '/'
    + ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoIndustrialBrasil.GerarRegistroHeader400(NumeroRemessa: Integer;
  aRemessa: TStringList);
var
  LLinha: string;
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;

  LLinha := '0'                                             + // 001-001 Identificação do registro
  '1'                                                       + // 002-002 Identificação do arquivo remessa
  'REMESSA'                                                 + // 003-009 Literal remessa
  '01'                                                      + // 010-011 Código de serviço
  PadRight('COBRANCA', 15, ' ')                             + // 012-026 Literal serviço
  PadRight(Beneficiario.CodigoCedente, 20, ' ')             + // 027-046 Código da empresa
  PadRight(Beneficiario.Nome, 30, ' ')                      + // 047-076 Nome da empresa Mãe
  IntToStrZero(ACBrBanco.Numero, 3)                         + // 077-079 Número do BIB na câmara de compensação CIP
  PadRight(ACBrBanco.Nome, 15, ' ')                         + // 080-094 Nome do banco por extenso
  FormatDateTime('ddmmyy', Now)                             + // 095-100 Data da gravação do arquivo
  Space(294)                                                + // 101-394 Brancos
  '000001';                                                   // 395-400 Nº sequencial do registro

  ARemessa.Add(UpperCase(LLinha));
end;

procedure TACBrBancoIndustrialBrasil.GerarRegistroTransacao400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  Boleto             : TACBrBoleto;
  Beneficiario       : TACBrCedente;
  Pagador            : TACBrSacado;
  BeneficiarioFinal  : TACBrSacadoAvalista;
  LTipoPessoa        : String;
  LCPFCNPJ           : String;
  LLinha             : String;
  LCodigoMulta       : String;
  LValorMulta        : Integer;
  LCarteira          : String;
  LOcorrencia        : String;
  LEspecieDoc        : String;
  LDiasProtesto      : Integer;
begin
  Boleto            := ACBrTitulo.ACBrBoleto;
  Beneficiario      := Boleto.Cedente;
  Pagador           := ACBrTitulo.Sacado;
  BeneficiarioFinal := ACBrTitulo.Sacado.SacadoAvalista;

  {Código de Inscrição 002 - 003}
  {Número de Inscrição da Empresa 004-017}
  if (BeneficiarioFinal.CNPJCPF <> '') then
  begin
    LTipoPessoa :=  ifThen(BeneficiarioFinal.Pessoa = pFisica, '03', '04');
    LCPFCNPJ    :=  OnlyNumber(BeneficiarioFinal.CNPJCPF);
  end else
  begin
    LTipoPessoa :=  IfThen(Beneficiario.TipoInscricao = pFisica, '01', '02');
    LCPFCNPJ    :=  OnlyNumber(Beneficiario.CNPJCPF);
  end;
  {Código da Multa 090-090}
  LCodigoMulta := '0'; //Sem Multa
  if (ACBrTitulo.MultaValorFixo) then
    LCodigoMulta := '1' //Valor Fixo
  else
  if (ACBrTitulo.PercentualMulta > 0) then
    LCodigoMulta := '2'; //Percentual

  {Valor ou Taxa de Multa 091-103}
  case StrToInt(LCodigoMulta) of
    1 : LValorMulta := Round(ACBrTitulo.ValorDocumento * ACBrTitulo.PercentualMulta);
    2 : LValorMulta := Round(ACBrTitulo.PercentualMulta * 10000);
  end;

  case StrToIntDef(ACBrTitulo.Carteira,0) of
    4,110 : LCarteira := '4';
    6,121 : LCarteira := '6';
    else
       LCarteira := '0';
  end;

  {Código de Ocorrência 109-110}
  case ACBrTitulo.OcorrenciaOriginal.Tipo of
    toRemessaRegistrar          : LOcorrencia := '01';
    toRemessaBaixar             : LOcorrencia := '02';
    toRemessaConcederAbatimento : LOcorrencia := '04';
    toRemessaCancelarAbatimento : LOcorrencia := '05';
    toRemessaAlterarVencimento  : LOcorrencia := '06';
    toRemessaProtestar          : LOcorrencia := '09';
    toRemessaNaoProtestar       : LOcorrencia := '10';
    toRemessaSustarProtesto     : LOcorrencia := '18';
    toRemessaAlterarValorTitulo : LOcorrencia := '47';
    else
      LOcorrencia := '01';
  end;

  {Espécie do título 148-149}
  case AnsiIndexStr(ACBrTitulo.EspecieDoc, ['DM', 'NP', 'CHQ', 'LC', 'RC', 'AS', 'DS', 'CC', 'OUT']) of
    0 : LEspecieDoc := '01'; //DM duplicata mercantil por indicação
    1 : LEspecieDoc := '02'; //NP nota promissória
    2 : LEspecieDoc := '03'; //CHQ Cheque
    3 : LEspecieDoc := '04'; //LC letra de câmbio
    4 : LEspecieDoc := '05'; //RC Recibo
    5 : LEspecieDoc := '08'; //AS Apólice de Seguro
    6 : LEspecieDoc := '12'; //DS Duplicata de Serviço
    7 : LEspecieDoc := '31'; //CC Cartão de Crédito
    8 : LEspecieDoc := '99'; //OUT Outros
  else
    LEspecieDoc := ACBrTitulo.EspecieDoc; //Outros
  end;

  {Instrução01 157-158}
  {Prazo 392-393}
  LDiasProtesto := ACBrTitulo.DiasDeProtesto;
  if LDiasProtesto = 0 then
    if ACBrTitulo.DataProtesto > 0 then
      LDiasProtesto := DaysBetween(ACBrTitulo.DataProtesto, ACBrTitulo.Vencimento);

  try
    LLinha := '1'                                                    + // 001-001 Identificação do registro de transação
              LTipoPessoa                                            + // 002-003 Identificação do Tipo de Inscrição da empresa
     PadLeft(LCPFCNPJ, 14, '0')                                      + // 004-017 Número de Inscrição da Empresa (CNPJ/CPF)
     PadRight(Beneficiario.CodigoCedente, 20, ' ')                   + // 018-037 Identificação da empresa no BIB
     PadRight(ACBrTitulo.SeuNumero, 25, ' ')                         + // 038-062 Identificação do Título na empresa
     PadLeft(ACBrTitulo.NossoNumero, 10, '0')                        + // 063-072 Identificação do Título no Banco
     PadLeft(CalcularDigitoVerificador(ACBrTitulo), 1, '0')          + // 073-073 DV Nosso Número
     Space(13)                                                       + // 074-086 Cobrança Direta Título Correspondente
     Space(03)                                                       + // 087-089 Modalidade de Cobrança com bancos correspondentes
     LCodigoMulta                                                    + // 090-090 Código da Multa 0 - Sem multa 1 - Valor fixo 2 - percentual
     Poem_Zeros(LValorMulta, 13)                                     + // 091-103 Valor ou Taxa de Multa
     IfThen(LValorMulta > 0,'01','00')                               + // 104-105 Número de Dias Após o Vencimento para aplicar a Multa
     Space(02)                                                       + // 106-107 Brancos
     LCarteira                                                       + // 108-108 Código da Carteira :: Carteira 6 alterada segundo email 112 para 6
     LOcorrencia                                                     + // 109-110 Identificação da ocorrência
     PadRight(ACBrTitulo.NumeroDocumento, 10, ' ')                   + // 111-120 N. documento de Cobrança (Duplicata, Promissória etc.)
     FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)                 + // 121-126 Data de vencimento do título
     Poem_Zeros(Round(ACBrTitulo.ValorDocumento * 100), 13)          + // 127-139 Valor do título
     Poem_Zeros(ACBrBanco.Numero, 3)                                 + // 140-142 Código do Banco
     Poem_Zeros(0, 4)                                                + // 143-146  Agência encarregada da cobrança ZEROS
     Poem_Zeros(0, 1)                                                + // 147-147  DV Agência encarregada da cobrança ZEROS
     LEspecieDoc                                                     + // 148-149 Espécie do título
     IfThen(ACBrTitulo.Aceite = atSim, 'A', 'N')                     + // 150-150 Aceite (A ou N)
     FormatDateTime('ddmmyy', ACBrTitulo.DataDocumento)              + // 151-156 Data da emissão do título
     IfThen(LDiasProtesto > 0, '00', '10')                           + // 157-158 Primeira Instrução
     '00'                                                            + // 159-160 Segunda Instrução
     Poem_Zeros(Round(ACBrTitulo.ValorMoraJuros * 100), 13)          + // 161-173 Valor de mora por dia de atraso
     IfThen(ACBrTitulo.DataDesconto > 0,
      FormatDateTime('ddmmyy', ACBrTitulo.DataDesconto), '000000')   + // 174-179 Data Limite para concessão de desconto
     Poem_Zeros(Round(ACBrTitulo.ValorDesconto * 100), 13)           + // 180-192 Valor do desconto a ser concedido
     Poem_Zeros(Round(ACBrTitulo.ValorIOF * 100), 13)                + // 193-205 Valor do I.O.F. a ser recolhido pelo Banco no caso de seguro
     Poem_Zeros(Round(ACBrTitulo.ValorAbatimento * 100), 13)         + // 206-218 Valor do abatimento a ser concedido
     IfThen(Pagador.Pessoa = pFisica, '01', '02')                    + // 219-220 Identificação do tipo de inscrição do sacado
     PadLeft(OnlyNumber(Pagador.CNPJCPF), 14, '0')                   + // 221-234 Número de Inscrição do Sacado
     PadRight(TiraAcentos(Pagador.NomeSacado), 30, ' ')              + // 234-264 Nome do Sacado
     Space(10)                                                       + // 265-274 Complementação do Registro - Brancos
     PadRight(TiraAcentos(Pagador.Logradouro) + ' ' +
                          Pagador.Numero + ' ' +
                          Pagador.Complemento, 40, ' ')              + // 275-314 Rua, Número e Complemento do Sacado
     PadRight(TiraAcentos(Pagador.Bairro), 12, ' ')                  + // 315-326 Bairro do Sacado
     PadLeft(OnlyNumber(Pagador.CEP), 8, '0')                        + // 327-334 CEP do Sacado
     PadRight(TiraAcentos(Pagador.Cidade), 15, ' ')                  + // 335-349 Cidade do Sacado
     PadRight(TiraAcentos(Pagador.UF), 2, ' ')                       + // 350-351 Bairro do Sacado
     PadLeft(BeneficiarioFinal.NomeAvalista, 30, ' ')                + // 352-381 Nome do Sacador ou Avalista
     Space(4)                                                        + // 382-391 Complementação do Registro - Brancos
     Space(6)                                                        + // 382-391 Complementação do Registro - Brancos
     IfThen(LDiasProtesto > 0, Poem_Zeros(LDiasProtesto, 2), '00')   + // 392-393 Quantidade de dias para início da Ação de Protesto
     '0'                                                             + // 394-394 Moeda 0 ou 1 Moeda Corrente Nacional
     Poem_Zeros(ARemessa.Count + 1, 6);                                // 395-400 Número Sequencial do Registro no Arquivo

  finally
    ARemessa.Add(UpperCase(LLinha));
    MontarRegistroBeneficiarioFinal400(ACBrTitulo, aRemessa);
    //MontarRegistroMensagem400(ACBrTitulo, aRemessa); // existe no manual mas a validação bancária
                                                       // pediu para remover pois há erro,
                                                       // que eles não usam esse bloco no sistema deles
    if ACBrTitulo.ListaDadosNFe.Count > 0 then
      GerarRegistrosNFe(ACBrTitulo, aRemessa);
  end;
end;

procedure TACBrBancoIndustrialBrasil.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  LLinha: string;
begin
  LLinha := '9'                                   + // 001-001 Identificação registro
  Space(393)                                      + // 002-394 Branco
  IntToStrZero(ARemessa.Count + 1, 6);              // 395-400 Número sequencial de registro

  ARemessa.Add(UpperCase(LLinha));
end;

procedure TACBrBancoIndustrialBrasil.MontarRegistroBeneficiarioFinal400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  BeneficiarioFinal  : TACBrSacadoAvalista;
  LLinha             : String;
begin
  BeneficiarioFinal := ACBrTitulo.Sacado.SacadoAvalista;

  if NaoEstaVazio(BeneficiarioFinal.CNPJCPF) then
  begin
    LLinha := '5'                                                           + // 001-001 Identificação registro
     Space(120)                                                             + // 002-121 Complementação do Registro - Brancos
     PadLeft(ifThen(BeneficiarioFinal.Pessoa = pFisica, '03', '04'),2,'0')  + // 122-123 Identificação de Inscrição do Sacador/Avalista
     PadLeft(OnlyNumber(BeneficiarioFinal.CNPJCPF),14,'0')                  + // 124-137 Número de Inscrição do Sacador/Avalista
     PadRight(TiraAcentos(BeneficiarioFinal.Logradouro) + ' ' +
                          BeneficiarioFinal.Numero + ' ' +
                          BeneficiarioFinal.Complemento, 40, ' ')           + // 138-177 Rua, Número de complemento do Sacador/Avalista
     PadRight(TiraAcentos(BeneficiarioFinal.Bairro), 12, ' ')               + // 315-326 Bairro do Sacador/Avalista
     PadLeft(OnlyNumber(BeneficiarioFinal.CEP), 8, '0')                     + // 327-334 CEP do Sacador/Avalista
     PadRight(TiraAcentos(BeneficiarioFinal.Cidade), 15, ' ')               + // 335-349 Cidade do Sacador/Avalista
     PadRight(TiraAcentos(BeneficiarioFinal.UF), 2, ' ')                    + // 350-351 Bairro do Sacador/Avalista
     Space(180)                                                             + // 348-394 Complementação do Registro - Brancos
     Poem_Zeros(ARemessa.Count + 1, 6);                                       // 395-400 Número sequencial de registro

    ARemessa.Add(UpperCase(LLinha));
  end;
end;

procedure TACBrBancoIndustrialBrasil.MontarRegistroMensagem400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  LMensagem: array[0..4] of string;
  LTextos : TStrings;
  Index: integer;
  LLinha: string;
begin
  LLinha := EmptyStr;

  if NaoEstaVazio(ACBrTitulo.Informativo.Text) then
    LTextos := ACBrTitulo.Informativo
  else
    LTextos := ACBrTitulo.Mensagem;

  for Index := 0 to Pred(LTextos.Count) do
  begin
    if Index > 4 then Break;
    LMensagem[Index] := LTextos[Index];
  end;

  if NaoEstaVazio(LTextos.Text) then
  begin
    LLinha := '2'                                   + // 001-001 Identificação registro
     '0'                                            + // 002-002 Zero
     PadRight(TiraAcentos(LMensagem[0]), 69)        + // 003-071 Mensagem Livre 1 69 posições
     PadRight(TiraAcentos(LMensagem[1]), 69)        + // 072-140 Mensagem Livre 2 69 posições
     PadRight(TiraAcentos(LMensagem[2]), 69)        + // 141-209 Mensagem Livre 3 69 posições
     PadRight(TiraAcentos(LMensagem[3]), 69)        + // 210-278 Mensagem Livre 4 69 posições
     PadRight(TiraAcentos(LMensagem[4]), 69)        + // 279-347 Mensagem Livre 5 69 posições
     Space(47)                                      + // 348-394 Brancos
     Poem_Zeros(ARemessa.Count + 1, 6);               // 395-400 Número sequencial de registro

    ARemessa.Add(UpperCase(LLinha));
  end;
end;

procedure TACBrBancoIndustrialBrasil.LerRetorno400(ARetorno: TStringList);
var
  Index: Integer;
  LLinha: string;
  LTitulo: TACBrTitulo;
  LBoleto: TACBrBoleto;
  LColunaMotivoRejeicao : integer;
  LQtdeMotivosRejeicao : integer;
begin
  LBoleto := ACBrBanco.ACBrBoleto;

  if (StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero) then
    raise Exception.create(ACBrStr(LBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

  LBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2)
    + '/'
    + Copy(ARetorno[0], 97, 2)
    + '/'
    + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

  if LBoleto.LeCedenteRetorno then
  begin
    LBoleto.Cedente.Nome          := Trim(Copy(ARetorno[0], 47, 30));
    LBoleto.Cedente.CNPJCPF       := Trim(Copy(ARetorno[1], 4, 14));
    LBoleto.Cedente.CodigoCedente := Trim(Copy(ARetorno[1], 18, 20));
  end;

  if Trim(LBoleto.Cedente.CodigoCedente) <> Trim(Copy(ARetorno[1], 18, 20)) then
    raise Exception.create(ACBrStr(format('O Código de cedente do arquivo %s não é o mesmo do componente %s.',[Copy(ARetorno[1], 18, 20),LBoleto.Cedente.CodigoCedente])));

  case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
    01: LBoleto.Cedente.TipoInscricao := pFisica;
  else
    LBoleto.Cedente.TipoInscricao := pJuridica;
  end;

  LBoleto.ListadeBoletos.Clear;

  for Index := 1 to ARetorno.Count - 2 do
  begin
    LLinha := ARetorno[Index];

    if (Copy(LLinha, 1, 1) <> '1') then
      Continue;

    LTitulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;
    LTitulo.SeuNumero       := copy(LLinha, 38, 25);
    LTitulo.NossoNumero     := Copy(LLinha, 63, TamanhoMaximoNossoNum);
    LTitulo.Carteira        := copy(LLinha, 83, 3);
    LTitulo.NumeroDocumento := copy(LLinha, 117, 10);

    LTitulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(LLinha, 109, 2), 0));
    if LTitulo.OcorrenciaOriginal.Tipo in [toRetornoRegistroRecusado,
      toRetornoBaixaRejeitada, toRetornoInstrucaoRejeitada] then
    begin
      LColunaMotivoRejeicao := 378; // posição da primeira rejeicao
      for LQtdeMotivosRejeicao := 1 to 3 do
      begin
        if Copy(LLinha, LColunaMotivoRejeicao, 2)<>'00' then
        begin
          LTitulo.MotivoRejeicaoComando.Add(Copy(LLinha, LColunaMotivoRejeicao, 2));
          LTitulo.DescricaoMotivoRejeicaoComando.add(CodMotivoRejeicaoToDescricao(LTitulo.OcorrenciaOriginal.Tipo,copy(LLinha, LColunaMotivoRejeicao, 2)));
        end;
        LColunaMotivoRejeicao := LColunaMotivoRejeicao + 2; // incrementa 2 posicoes para próxima rejeicao
      end;
    end;

    LTitulo.DataOcorrencia := StringToDateTimeDef(Copy(LLinha, 111, 2)
       + '/'
       + Copy(LLinha, 113, 2)
       + '/'
       + Copy(LLinha, 115, 2), 0, 'DD/MM/YY');

    case StrToIntDef(Copy(LLinha, 174, 2), 0) of
      01: LTitulo.EspecieDoc := 'DM';
      02: LTitulo.EspecieDoc := 'NP';
      03: LTitulo.EspecieDoc := 'CH';
      04: LTitulo.EspecieDoc := 'LC';
      05: LTitulo.EspecieDoc := 'RC';
      08: LTitulo.EspecieDoc := 'AS';
      12: LTitulo.EspecieDoc := 'DS';
      31: LTitulo.EspecieDoc := 'CC';
      99: LTitulo.EspecieDoc := 'OUT';
    end;

    case StrToInt(copy(LLinha, 108, 1)) of
      1: LTitulo.CaracTitulo := tcSimples;
      2: LTitulo.CaracTitulo := tcVinculada;
      3: LTitulo.CaracTitulo := tcCaucionada;
      4: LTitulo.CaracTitulo := tcDescontada;
    end;

    if (StrToIntDef(Copy(LLinha, 147, 6), 0) <> 0) then
      LTitulo.Vencimento := StringToDateTimeDef(Copy(LLinha, 147, 2)
        + '/'
        + Copy(LLinha, 149, 2)
        + '/'
        + Copy(LLinha, 151, 2), 0, 'DD/MM/YY');

    LTitulo.ValorDocumento       := StrToFloatDef(Copy(LLinha, 153, 13), 0) / 100;
    LTitulo.ValorDespesaCobranca := StrToFloatDef(Copy(LLinha, 176, 13), 0) / 100;
    LTitulo.ValorIOF             := StrToFloatDef(Copy(LLinha, 215, 13), 0) / 100;
    LTitulo.ValorAbatimento      := StrToFloatDef(Copy(LLinha, 228, 13), 0) / 100;
    LTitulo.ValorDesconto        := StrToFloatDef(Copy(LLinha, 241, 13), 0) / 100;
    LTitulo.ValorRecebido        := StrToFloatDef(Copy(LLinha, 254, 13), 0) / 100;
    LTitulo.ValorMoraJuros       := StrToFloatDef(Copy(LLinha, 267, 13), 0) / 100;

    if (StrToIntDef(Copy(LLinha, 386, 6), 0) <> 0) then
      LTitulo.DataCredito := StringToDateTimeDef(Copy(LLinha, 386, 2)
        + '/'
        + Copy(LLinha, 388, 2)
        + '/'
        + Copy(LLinha, 390, 2), 0, 'DD/MM/YY');
  end;
end;


function TACBrBancoIndustrialBrasil.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
begin

  case TipoOcorrencia of
      toRetornoRegistroRecusado: // Ocorrencia 03
        begin
          case StrtoIntDef(CodMotivo, -1) of
            -1:
              begin
                  if CodMotivo = 'AA' Then result := 'AA - Serviço de cobrança inválido'
                  else if CodMotivo = 'AB' Then result := 'AB - Serviço de "0" ou "5" e banco cobrador <> zeros'
                  else if CodMotivo = 'AE' Then result := 'AE - Título não possui abatimento'
                  else if CodMotivo = 'AG' Then result := 'AG - Movto não permitido para título À Vista/Contra Apresentação'
                  else if CodMotivo = 'AH' Then result := 'AH - Cancelamento de Valores Inválidos'
                  else if CodMotivo = 'AI' Then result := 'AI - Nossa carteira inválida'
                  else if CodMotivo = 'AJ' Then result := 'AJ - Modalidade com bancos correspondentes inválida'
                  else if CodMotivo = 'AK' Then result := 'AK - Título pertence a outro cliente'
                  else if CodMotivo = 'AL' Then result := 'AL - Sacado impedido de entrar nesta cobrança'
                  else if CodMotivo = 'AT' Then result := 'AT - Valor Pago Inválido'
                  else if CodMotivo = 'AU' Then result := 'AU - Data da ocorrência inválida'
                  else if CodMotivo = 'AV' Then result := 'AV - Valor da tarifa de cobrança inválida'
                  else if CodMotivo = 'AX' Then result := 'AX - Título em pagamento parcial'
                  else if CodMotivo = 'AY' Then result := 'AY - Título em Aberto e Vencido para acatar protestol'
                  else if CodMotivo = 'BA' Then result := 'BA - Banco Correspondente Recebedor não é o Cobrador Atual'
                  else if CodMotivo = 'BB' Then result := 'BB - Título deve estar em cartório para baixar'
                  else if CodMotivo = 'BC' Then result := 'BC - Análise gerencial-sacado inválido p/operação crédito'
                  else if CodMotivo = 'BD' Then result := 'BD - Análise gerencial-sacado inadimplente'
                  else if CodMotivo = 'BE' Then result := 'BE - Análise gerencial-sacado difere do exigido'
                  else if CodMotivo = 'BF' Then result := 'BF - Análise gerencial-vencto excede vencto da operação de crédito'
                  else if CodMotivo = 'BG' Then result := 'BG - Análise gerencial-sacado com baixa liquidez'
                  else if CodMotivo = 'BH' Then result := 'BH - Análise gerencial-sacado excede concentração'
                  else if CodMotivo = 'CC' Then result := 'CC - Valor de iof incompatível com a espécie documento'
                  else if CodMotivo = 'CD' Then result := 'CD - Efetivação de protesto sem agenda válida'
                  else if CodMotivo = 'CE' Then result := 'CE - Título não aceito - pessoa física'
                  else if CodMotivo = 'CF' Then result := 'CF - Excede prazo máximo da entrada ao vencimento'
                  else if CodMotivo = 'CG' Then result := 'CG - Título não aceito – por análise gerencial'
                  else if CodMotivo = 'CH' Then result := 'CH - Título em espera – em análise pelo banco'
                  else if CodMotivo = 'CJ' Then result := 'CJ - Análise gerencial-vencto do titulo abaixo przcurto'
                  else if CodMotivo = 'CK' Then result := 'CK - Análise gerencial-vencto do titulo abaixo przlongo'
                  else if CodMotivo = 'CS' Then result := 'CS - Título rejeitado pela checagem de duplicatas'
                  else if CodMotivo = 'DA' Then result := 'DA - Análise gerencial – Entrada de Título Descontado com limite cancelado'
                  else if CodMotivo = 'DB' Then result := 'DB - Análise gerencial – Entrada de Título Descontado com limite vencido'
                  else if CodMotivo = 'DC' Then result := 'DC - Análise gerencial - cedente com limite cancelado'
                  else if CodMotivo = 'DD' Then result := 'DD - Análise gerencial – cedente é sacado e teve seu limite cancelado'
                  else if CodMotivo = 'DE' Then result := 'DE - Análise gerencial - apontamento no Serasa'
                  else if CodMotivo = 'DG' Then result := 'DG - Endereço sacador/avalista não informado'
                  else if CodMotivo = 'DH' Then result := 'DH - Cep do sacador/avalista não informado'
                  else if CodMotivo = 'DI' Then result := 'DI - Cidade do sacador/avalista não informado'
                  else if CodMotivo = 'DJ' Then result := 'DJ - Estado do sacador/avalista inválido ou n informado'
                  else if CodMotivo = 'DM' Then result := 'DM - Cliente sem Código de Flash cadastrado no cobrador'
                  else if CodMotivo = 'DN' Then result := 'DN - Título Descontado com Prazo ZERO – Recusado'
                  else if CodMotivo = 'DP' Then result := 'DP - Data de Referência menor que a Data de Emissão do Título'
                  else if CodMotivo = 'DT' Then result := 'DT - Nosso Número do Correspondente não deve ser informado'
                  else if CodMotivo = 'EB' Then result := 'EB - HSBC não aceita endereço de sacado com mais de 38 caracteres'
                  else if CodMotivo = 'G1' Then result := 'G1 - Endereço do sacador incompleto ( lei 12.039)'
                  else if CodMotivo = 'G2' Then result := 'G2 - Sacador impedido de movimentar'
                  else if CodMotivo = 'G3' Then result := 'G3 - Concentração de cep não permitida'
                  else if CodMotivo = 'G4' Then result := 'G4 - Valor do título não permitido'
                  else if CodMotivo = 'HA' Then result := 'HA - Serviço e Modalidade Incompatíveis'
                  else if CodMotivo = 'HB' Then result := 'HB - Inconsistências entre Registros Título e Sacador'
                  else if CodMotivo = 'HC' Then result := 'HC - Ocorrência não disponível'
                  else if CodMotivo = 'HD' Then result := 'HD - Título com Aceite'
                  else if CodMotivo = 'HF' Then result := 'HF - Baixa Liquidez do Sacado'
                  else if CodMotivo = 'HG' Then result := 'HG - Sacado Informou que não paga Boletos'
                  else if CodMotivo = 'HH' Then result := 'HH - Sacado não confirmou a Nota Fiscal'
                  else if CodMotivo = 'HI' Then result := 'HI - Checagem Prévia não Efetuada'
                  else if CodMotivo = 'HJ' Then result := 'HJ - Sacado desconhece compra e Nota Fiscal'
                  else if CodMotivo = 'HK' Then result := 'HK - Compra e Nota Fiscal canceladas pelo sacado'
                  else if CodMotivo = 'HL' Then result := 'HL - Concentração além do permitido pela área de Crédito'
                  else if CodMotivo = 'HM' Then result := 'HM - Vencimento acima do permitido pelo área de Crédito'
                  else if CodMotivo = 'HN' Then result := 'HN - Excede o prazo limite da operação'
                  else if CodMotivo = 'IX' Then result := 'IX - Título de Cartão de Crédito não aceita instruções'
                  else if CodMotivo = 'JB' Then result := 'JB - Título de Cartão de Crédito inválido para o Produto'
                  else if CodMotivo = 'JC' Then result := 'JC - Produto somente para Cartão de Crédito'
                  else if CodMotivo = 'JH' Then result := 'JH - CB Direta com operação de Desconto Automático'
                  else if CodMotivo = 'JI' Then result := 'JI - Espécie de Documento incompatível para produto de Cartão de Crédito'
                  else Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
                end;
            03: result := '03 - CEP inválido – Não temos cobrador – Cobrador não Localizado';
            04: result := '04 - Sigla do Estado inválida';
            05: result := '05 - Data de Vencimento inválida ou fora do prazo mínimo';
            06: result := '06 - Código do Banco inválido';
            08: result := '08 - Nome do sacado não informado';
            10: result := '10 - Logradouro não informado';
            14: result := '14 - Registro em duplicidade';
            19: result := '19 - Data de desconto inválida ou maior que a data de vencimento';
            20: result := '20 - Valor de IOF não numérico';
            21: result := '21 - Movimento para título não cadastrado no sistema';
            22: result := '22 - Valor de desconto + abatimento maior que o valor do título';
            23: result := '25 - CNPJ ou CPF do sacado inválido (aceito com restrições)';
            26: result := '26 - Espécie de documento inválida';
            27: result := '27 - Data de emissão do título inválida';
            28: result := '28 - Seu número não informado';
            29: result := '29 - CEP é igual a espaço ou zeros; ou não numérico';
            30: result := '30 - Valor do título não numérico ou inválido';
            36: result := '36 - Valor de permanência (mora) não numérico ';
            37: result := '37 - Valor de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título';
            38: result := '38 - Valor de desconto/abatimento não numérico ou inválido';
            39: result := '39 - Valor de abatimento não numérico';
            42: result := '42 - Título já existente em nossos registros. Nosso número não aceito ';
            43: result := '43 - Título enviado em duplicidade nesse movimento';
            44: result := '44 - Título zerado ou em branco; ou não numérico na remessa';
            46: result := '46 - Título enviado fora da faixa de Nosso Número, es1ipulada para o cliente.';
            51: result := '51 - Tipo/Número de Inscrição Sacador/Avalista Inválido';
            52: result := '52 - Sacador/Avalista não informado';
            53: result := '53 - Prazo de vencimento do título excede ao da contratação';
            54: result := '54 - Banco informado não é nosso correspondente 140-142';
            55: result := '55 - Banco correspondente informado não cobra este CEP ou não possui faixas de CEP cadastradas';
            56: result := '56 - Nosso número no correspondente não foi informado';
            57: result := '57 - Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido.';
            58: result := '58 - Entradas Rejeitadas – Reprovado no Represamento para Análise';
            60: result := '60 - CNPJ/CPF do sacado inválido – título recusado';
            87: result := '87 - Excede Prazo máximo entre emissão e vencimento';
          else
              Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
          end;
        end;
      toRetornoBaixaRejeitada: // ocorrencia 15
        begin
           case StrtoIntDef(CodMotivo, 0) of
             05: result := '05 - Solicitação de baixa para título já baixado ou liquidado';
             06: result := '06 - Solicitação de baixa para título não registrado no sistema';
             08: result := '08 - Solicitação de baixa para título em float';
             else
             Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';
           end;
        end;
      toRetornoInstrucaoRejeitada: // ocorrência16
        begin
          case StrtoIntDef(CodMotivo, -1) of
             -1: begin
               if CodMotivo = 'AA' Then result := 'AA - Serviço de cobrança inválido'
               else if CodMotivo = 'AE' Then result := 'AE - Título não possui abatimento'
               else if CodMotivo = 'AG' Then result := 'AG - Movimento não permitido – Título à vista ou contra apresentação'
               else if CodMotivo = 'AH' Then result := 'AH - Cancelamento de valores inválidos'
               else if CodMotivo = 'AI' Then result := 'AI - Nossa carteira inválida'
               else if CodMotivo = 'AK' Then result := 'AK - Título pertence a outro cliente'
               else if CodMotivo = 'AU' Then result := 'AU - Data da ocorrência inválida'
               else if CodMotivo = 'AY' Then result := 'AY - Título deve estar em aberto e vencido para acatar protesto'
               else if CodMotivo = 'CB' Then result := 'CB - Título possui protesto efetivado/a efetivar hoje'
               else if CodMotivo = 'CT' Then result := 'CT - Título já baixado'
               else if CodMotivo = 'CW' Then result := 'CW - Título já transferido'
               else if CodMotivo = 'DO' Then result := 'DO - Título em Prejuízo'
               else if CodMotivo = 'JK' Then result := 'JK - Produto não permite alteração de valor de título'
               else if CodMotivo = 'JQ' Then result := 'JQ - Título em Correspondente – Não alterar Valor'
               else if CodMotivo = 'JS' Then result := 'JS - Título possui Descontos/Abto/Mora/Multa'
               else if CodMotivo = 'JT' Then result := 'JT - Título possui Agenda de Protesto/Devolução'
               else Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';

             end;
             04: result := '04 - Data de vencimento não numérica ou inválida';
             05: result := '05 - Data de Vencimento inválida ou fora do prazo mínimo';
             14: result := '14 - Registro em duplicidade';
             19: result := '19 - Data de desconto inválida ou maior que a data de vencimento';
             20: result := '20 - Campo livre não informado';
             21: result := '21 - Título não registrado no sistema';
             22: result := '22 - Título baixado ou liquidado';
             26: result := '26 - Espécie de documento inválida';
             27: result := '27 - Instrução não aceita, por não ter sido emitida ordem de protesto ao cartório';
             28: result := '28 - Título tem instrução de cartório ativa';
             29: result := '29 - Título não tem instrução de carteira ativa';
             30: result := '30 - Existe instrução de não protestar, ativa para o título';
             36: result := '36 - Valor de permanência (mora) não numérico';
             37: result := '37 - Título Descontado – Instrução não permitida para a carteira';
             38: result := '38 - Valor do abatimento não numérico ou maior que a soma do valor do título permanência + multa';
             39: result := '39 - Título em cartório';
             40: result := '40 - Instrução recusada – Reprovado no Represamento para Análise';
             44: result := '44 - Título zerado ou em branco; ou não numérico na remessa';
             51: result := '51 - Tipo/Número de Inscrição Sacador/Avalista Inválido';
             53: result := '53 - Prazo de vencimento do título excede ao da contratação';
             57: result := '57 - Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido';
             else
             Result := PadLeft(CodMotivo,2,'0') + ' - Outros motivos';

          end;
        end;
  end;
end;


function TACBrBancoIndustrialBrasil.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    01: Result := toRetornoEntradaConfirmadaNaCip;
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    05: Result := toRetornoDadosAlterados;
    06: Result := toRetornoLiquidado;
    08: Result := toRetornoLiquidadoEmCartorio;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixaPorTerSidoLiquidado;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoBaixaRejeitada;
    16: Result := toRetornoInstrucaoRejeitada;
    19: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    20: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    22: Result := toRetornoAlteracaoSeuNumero;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
    40: Result := toRetornoDebitoTarifas;
    43: Result := toRetornoBaixaPorProtesto;
    96: Result := toRetornoDebitoMensalTarifasOutrasInstrucoes;
    97: Result := toRetornoDebitoMensalTarifasOutrasInstrucoes;
    98: Result := toRetornoDebitoMensalTarifasOutrasInstrucoes;
    99: Result := toRetornoDebitoMensalTarifasSustacaoProtestos;
  else
    Result := toTipoOcorrenciaNenhum;
  end;
end;

function TACBrBancoIndustrialBrasil.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): string;
begin
  case TipoOcorrencia of
    toRetornoEntradaConfirmadaNaCip                      : Result := '01-Confirma Entrada Título na CIP';
    toRetornoRegistroConfirmado                          : Result := '02–Entrada Confirmada';
    toRetornoRegistroRecusado                            : Result := '03–Entrada Rejeitada';
    toRetornoDadosAlterados                              : Result := '05-Campo Livre Alterado';
    toRetornoLiquidado                                   : Result := '06-Liquidação Normal';
    toRetornoLiquidadoEmCartorio                         : Result := '08-Liquidação em Cartório';
    toRetornoBaixaAutomatica                             : Result := '09-Baixa Automática';
    toRetornoBaixaPorTerSidoLiquidado                    : Result := '10-Baixa por ter sido liquidado';
    toRetornoAbatimentoConcedido                         : Result := '12-Confirma Abatimento';
    toRetornoAbatimentoCancelado                         : Result := '13-Abatimento Cancelado';
    toRetornoVencimentoAlterado                          : Result := '14-Vencimento Alterado';
    toRetornoBaixaRejeitada                              : Result := '15-Baixa Rejeitada';
    toRetornoInstrucaoRejeitada                          : Result := '16-Instrução Rejeitadas';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente : Result := '19-Confirma Recebimento de Ordem de Protesto';
    toRetornoAlteracaoSeuNumero                          : Result := '22-Seu número alterado';
    toRetornoEncaminhadoACartorio                        : Result := '23-Título enviado para cartório';
    toRetornoRecebimentoInstrucaoNaoProtestar            : Result := '24-Confirma recebimento de ordem de não protestar';
    toRetornoDebitoTarifas                               : Result := '40-Tarifa de Entrada (debitada na Liquidação)';
    toRetornoBaixaPorProtesto                            : Result := '43-Baixado por ter sido protestado';
    toRetornoDebitoMensalTarifasOutrasInstrucoes         : Result := '96, 97 e 98-Tarifas - Mês Anterior';
    toRetornoDebitoMensalTarifasSustacaoProtestos        : Result := '99-Tarifa Sobre Instruções de Protesto/Sustação – Mês Anterior';
  else
    Result := 'Outras ocorrências';
  end;
end;

function TACBrBancoIndustrialBrasil.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoRegistro);
end;

procedure TACBrBancoIndustrialBrasil.GerarRegistrosNFe(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  LQtdRegNFes, X, I, LQtdNFeNaLinha: Integer;
  LLinha, LNFeSemDados: String;
  LContinua: Boolean;
begin
   LNFeSemDados:= StringOfChar(' ',15) + StringOfChar('0', 65);
   LQtdRegNFes:= trunc(ACBrTitulo.ListaDadosNFe.Count / 3);

   if (ACBrTitulo.ListaDadosNFe.Count mod 3) <> 0 then
      Inc(LQtdRegNFes);

   X:= 0;
   I:= 0;
   repeat
   begin
      LContinua:=  true;

      LLinha:= '4';
      LQtdNFeNaLinha:= 0;
      while (LContinua) and (X < ACBrTitulo.ListaDadosNFe.Count) do
      begin
         LLinha:= LLinha +
                  PadRight(ACBrTitulo.ListaDadosNFe[X].NumNFe,15) +
                  IntToStrZero(round(ACBrTitulo.ListaDadosNFe[X].ValorNFe  * 100 ), 13) +
                  FormatDateTime('ddmmyyyy',ACBrTitulo.ListaDadosNFe[X].EmissaoNFe) +
                  PadLeft(ACBrTitulo.ListaDadosNFe[X].ChaveNFe, 44, '0');

         Inc(X);
         Inc(LQtdNFeNaLinha);
         LContinua := (X mod 3) <> 0 ;
      end;

      if LQtdNFeNaLinha < 3 then
      begin
         LLinha:= LLinha + LNFeSemDados;
         if LQtdNFeNaLinha < 2 then
            LLinha:= LLinha + LNFeSemDados;
      end;

      LLinha:= PadRight(LLinha,241) + StringOfChar(' ', 153) +
               IntToStrZero(aRemessa.Count + 1, 6);

      aRemessa.Add(LLinha);
      Inc(I);
   end;
   until (I = LQtdRegNFes);
end;

function TACBrBancoIndustrialBrasil.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
begin

  Result :=
   PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0')
   + PadLeft(DefineCarteira(ACBrTitulo), 3, '0')
   + PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Operacao, 7, '0')
   + PadLeft(ACBrTitulo.NossoNumero, 10, '0')
   + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoIndustrialBrasil.DefineCarteira(const ACBrTitulo: TACBrTitulo): String;
var LCarteira : String;
begin
  LCarteira := ACBrTitulo.Carteira;
  if (LCarteira = 'D') or (LCarteira = '4') then
    Result := '110'
  else if LCarteira = '6' then
    Result := '121'
  else
    Result := ACBrTitulo.Carteira;
end;

end.

