  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

//Incluido em 06/05/2025
{$I ACBr.inc}
unit ACBrBancoCredisan;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  ACBrBoleto,
  ACBrBoletoConversao;

type
  { TACBrBancoCredisan}

  TACBrBancoCredisan = class(TACBrBancoClass)
  protected
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
  private
    fNumeroRemessa  : Integer;
  public
    constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function CalcularNomeArquivoRemessa: string; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    Procedure LerRetorno240(ARetorno: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  ACBrUtil.Base,
  StrUtils,
  ACBrUtil.DateTime,
  Math,
  DateUtils,
  ACBrUtil.Strings;

constructor TACBrBancoCredisan.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                := 2;
  fpNome                  := 'CREDISANCED';
  fpNumero                := 089;
  fpTamanhoMaximoNossoNum := 7;
  fpTamanhoConta          := 8;
  fpTamanhoAgencia        := 4;
  fpTamanhoCarteira       := 1;
  fpCodigosMoraAceitos    := '2'
end;

function TACBrBancoCredisan.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var LParcela : String;
begin
  LParcela := PadLeft(IntToStr(ACBrTitulo.Parcela),3,'0');

  Result := ACBrTitulo.Carteira +
            PadLeft(Trim(ACBrTitulo.ACBrBoleto.Cedente.Agencia),4,'0') +
            PadLeft(Trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade),2,'0') +
            PadLeft(Trim(ACBrTitulo.ACBrBoleto.Cedente.Conta),7,'0') +
            PadLeft(ACBrTitulo.NossoNumero,8,'0') +
            LParcela;
end;

function TACBrBancoCredisan.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := '0';

  Modulo.CalculoPadrao;
  Modulo.MultiplicadorFinal   := 2;
  Modulo.MultiplicadorInicial := 9;
  Modulo.Documento            := ACBrTitulo.NossoNumero;
  Modulo.Calcular;

  if Modulo.ModuloFinal >= 10 then
    Result := 'X'
  else
    Result := IntToStr(Modulo.ModuloFinal);
end;

function TACBrBancoCredisan.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := PadLeft( ACBrTitulo.Carteira, 2, '0');
end;

function TACBrBancoCredisan.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  if (ACBrTitulo.ACBrBoleto.Banco.TipoCobranca = cobBancoDoBrasilAPI) then
  begin
    Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '/' + IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta, 0));
  end
  else
  begin
    Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '-' + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito + '/' + IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta, 0)) + '-' + ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
  end;
end;

function TACBrBancoCredisan.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := MontarCampoCarteira(ACBrTitulo) +
            ' / '+
            ACBrTitulo.NossoNumero +
            '-'+
            CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoCredisan.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.create(ACBrStr('Não permitido para o layout deste banco.'));
end;

procedure TACBrBancoCredisan.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  LBeneficiario   : TACBrCedente;
  LAgencia, LConta: String;
  LLinha          : String;
begin
  fNumeroRemessa := NumeroRemessa;

  LBeneficiario := ACBrBanco.ACBrBoleto.Cedente;
  LAgencia      := RightStr(LBeneficiario.Agencia, 4);
  LConta        := RightStr(LBeneficiario.Conta, 8);

  LLinha := '0'                                             + // 001 - 001 ID do Registro
            '1'                                             + // 002 - 002 ID do Arquivo( 1 - Remessa)
            'REMESSA'                                       + // 003 - 009 Literal de Remessa
            '01'                                            + // 010 - 011 Código do Tipo de Serviço
            'COBRANÇA'                                      + // 012 - 019 Nome do tipo de serviço
            Space(7)                                        + // 020 - 026 Brancos
            LAgencia                                        + // 027 - 030 Prefixo da Cooperativa
            PadRight(LBeneficiario.AgenciaDigito, 1, '0')   + // 031 - 031 DV da agencia
            PadLeft(LBeneficiario.Conta, 8, '0')            + // 032 - 039 Codigo do cedente
            PadRight(LBeneficiario.ContaDigito, 1, '0')     + // 040 - 040 DV código do cedente
            PadLeft(Trim(LBeneficiario.Convenio), 6, '0')   + // 041 - 046 Convenio Lider
            PadRight(Nome, 30)                              + // 047 - 076 Nome da Empresa
            IntToStrZero(Numero, 3)                         + // 077 - 079 Código do Banco
            PadRight(UpperCase(fpNome), 15)                 + // 080 - 094 Nome do Banco CREDISANCRED
            FormatDateTime('ddmmyy', Now)                   + // 095 - 100 Data de gravação do arquivo
            IntToStrZero(NumeroRemessa, 7)                  + // 101 - 107 Numero Sequencial de Remessa
            Space(287)                                      + // 108 - 394 Filler
            IntToStrZero(1, 6);                               // Nr. Sequencial do registro-informar 000001

  ARemessa.Add(UpperCase(LLinha));
end;

procedure TACBrBancoCredisan.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList);
var
  LNossoNumero, LDigitoNossoNumero, LMensagem,
  LConvenio, LParcela, LIndicadorSacador,
  LPrefixoTitulo, LLinha, LAgencia, LConta,
  LTipoEspecieDoc, LTipoAceite, LValorTaxaMoraMes,
  LValorTaxaMulta, LDataDesconto, LTipoSacado,
  LTipoCendente,LTipoOcorrencia : String;
  LBeneficiario : TACBrCedente;
begin

  {Pegando dados de Cedente}
  LBeneficiario := ACBrTitulo.ACBrBoleto.Cedente;
  LAgencia    := PadLeft(LBeneficiario.Agencia, 4, '0');
  LConta      := PadLeft(LBeneficiario.Conta, 8, '0');

  case LBeneficiario.TipoInscricao of
    pFisica:
      LTipoCendente := '01';
    pJuridica:
      LTipoCendente := '02';
    else
      LTipoCendente := '02';
  end;

  if (ACBrTitulo.ACBrBoleto.Cedente.ResponEmissao <> tbCliEmite) then
  begin
    LNossoNumero       := '00000000000';
    LDigitoNossoNumero := ' ';
    LPrefixoTitulo     := '112'; // Emissão pelo PAC
  end else
  begin
    LNossoNumero       := ACBrTitulo.NossoNumero;
    LDigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
    LPrefixoTitulo     := '109'; // Emissão pelo Cliente
  end;
  LParcela := PadLeft(IntToStr(ACBrTitulo.Parcela),2,'0');

  if (ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF <> '') then //Indica se Mensagem ou Avalista
    LIndicadorSacador :=  'A'//Avalista
  else
    LIndicadorSacador := ' '; //Mensagem

  { Pegando o Aceite do Titulo }
  case ACBrTitulo.Aceite of
    atSim:
      LTipoAceite := 'A';
    atNao:
      LTipoAceite := 'N';
    else
      LTipoAceite := 'N';
  end;
  {Pegando Código da Ocorrencia Lista 254}
  case ACBrTitulo.OcorrenciaOriginal.Tipo of
    toRemessaBaixar                 : LTipoOcorrencia := '02';
    toRemessaDebitarEmConta         : LTipoOcorrencia := '03';
    toRemessaConcederAbatimento     : LTipoOcorrencia := '04';
    toRemessaCancelarAbatimento     : LTipoOcorrencia := '05';
    toRemessaAlterarVencimento      : LTipoOcorrencia := '06';
    toRemessaAlterarControleParticipante : LTipoOcorrencia := '07';
    toRemessaAlterarNumeroControle  : LTipoOcorrencia := '08';
    toRemessaProtestar              : LTipoOcorrencia := '09';
    toRemessaCancelarInstrucaoProtestoBaixa : LTipoOcorrencia := '10';
    toRemessaDispensarJuros         : LTipoOcorrencia := '11';
    toRemessaAlterarNomeEnderecoSacado :LTipoOcorrencia := '12';
    toRemessaRecusaAlegacaoSacado   : LTipoOcorrencia := '30';
    toRemessaOutrasOcorrencias      : LTipoOcorrencia := '31';
    toRemessaBaixaporPagtoDiretoCedente : LTipoOcorrencia := '34';
    else
      LTipoOcorrencia := '01';
  end;

  {Pegando Tipo de Sacado}
  case ACBrTitulo.Sacado.Pessoa of
    pFisica:
      LTipoSacado := '01';
    pJuridica:
      LTipoSacado := '02';
    else
      LTipoSacado := '00';
  end;

  { Pegando o tipo de EspecieDoc Tabela 61}
    if ACBrTitulo.EspecieDoc =      'DM' then
      LTipoEspecieDoc := '01'
    else if ACBrTitulo.EspecieDoc = 'NP' then
      LTipoEspecieDoc := '02'
    else if ACBrTitulo.EspecieDoc = 'NS' then
      LTipoEspecieDoc := '03'
    else if ACBrTitulo.EspecieDoc = 'RC' then
      LTipoEspecieDoc := '05'
    else if ACBrTitulo.EspecieDoc = 'DR' then
      LTipoEspecieDoc := '06'
    else if ACBrTitulo.EspecieDoc = 'LC' then
      LTipoEspecieDoc := '08'
    else if ACBrTitulo.EspecieDoc = 'WR' then
      LTipoEspecieDoc := '09'
    else if ACBrTitulo.EspecieDoc = 'CQ' then
      LTipoEspecieDoc := '10'
    else if ACBrTitulo.EspecieDoc = 'DS' then
      LTipoEspecieDoc := '12'
    else if ACBrTitulo.EspecieDoc = 'ND' then
      LTipoEspecieDoc := '13'
    else if ACBrTitulo.EspecieDoc = 'TM' then
      LTipoEspecieDoc := '14'
    else if ACBrTitulo.EspecieDoc = 'TS' then
      LTipoEspecieDoc := '15'
    else if ACBrTitulo.EspecieDoc = 'FT' then
      LTipoEspecieDoc := '18'
    else if ACBrTitulo.EspecieDoc = 'AS' then
      LTipoEspecieDoc := '20'
    else if ACBrTitulo.EspecieDoc = 'ME' then
      LTipoEspecieDoc := '21'
    else if ACBrTitulo.EspecieDoc = 'PC' then
      LTipoEspecieDoc := '22'
    else if ACBrTitulo.EspecieDoc = 'OT' then
      LTipoEspecieDoc := '99';

    if ACBrTitulo.CodigoMoraJuros <> cjTaxaMensal then
      raise Exception.Create('Permitido somente Taxa Juros Mensal');

    if ACBrTitulo.CodigoMulta <> cmPercentual then
      raise Exception.Create('Permitido somente Taxa Multa Mensal');

    LValorTaxaMoraMes := FloatToStr(ACBrTitulo.ValorMoraJuros * 100);
    if ACBrTitulo.MultaValorFixo then
      raise Exception.Create('Permitido somente Taxa Multa Mensal')
    else
      LValorTaxaMulta   := FloatToStr(ACBrTitulo.PercentualMulta * 100);



    if (ACBrTitulo.ValorDesconto > 0) and (ACBrTitulo.DataDesconto > EncodeDate(2000, 01, 01)) then
      LDataDesconto := FormatDateTime('ddmmyy', ACBrTitulo.DataDesconto)
    else
      LDataDesconto := '000000';

    LMensagem := '';
    if (ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF <> '') then
    begin
      if ACBrTitulo.Sacado.SacadoAvalista.Pessoa = pJuridica then
        LMensagem := copy(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 1, 21) +
                     ' CNPJ' +
                     ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF
      else
        LMensagem := copy(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 1, 25) +
                     ' CPF' + ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF;
    end;

    if ACBrTitulo.Mensagem.Text <> '' then
      LMensagem := ACBrTitulo.Mensagem.Strings[ 0 ];

    LLinha := '1'                                                      + // 001 - 001 ID Registro
              LTipoCendente                                            + // 002 - 003 Tipo de inscrição da empresa
              PadLeft(OnlyNumber(LBeneficiario.CNPJCPF), 14, '0')      + // 004 - 017 Inscrição da empresa
              LAgencia                                                 + // 018 - 021 Cooperativa
              PadRight(LBeneficiario.AgenciaDigito, 1)                 + // 022 - 022 DV Agencia
              LConta                                                   + // 023 - 030 conta corrente da empresa
              PadRight(LBeneficiario.ContaDigito, 1)                   + // 031 - 031 DV código do cedente
              PadLeft(Trim(LBeneficiario.Convenio), 6)                 + // 032 - 037 Número do convenio
              PadRight(ACBrTitulo.SeuNumero, 25)                       + // 038 - 062 Numero de Controle do Participante
              PadLeft(LNossoNumero, 11, '0')                           + // 063 - 073 Nosso Numero
              LDigitoNossoNumero                                       + // 074 - 074 DV Nosso Numero
              LParcela                                                 + // 075 - 076 Parcela
              '00'                                                     + // 077 - 078 Indicativo de valor de grupo
              Space(3)                                                 + // 079 - 081 Filler Brancos
              LIndicadorSacador                                        + // 082 - 082 Indicador Sacador
              LPrefixoTitulo                                           + // 083 - 085 109 Emissão CLiente 112 - Emissão PAC
              IntToStrZero(0, 3)                                       + // 086 - 088 Variacao
              IntToStrZero(0, 1)                                       + // 089 - 089 Contrato
              IntToStrZero(0, 5)                                       + // 090 - 094 Contrato
              IntToStrZero(0, 1)                                       + // 095 - 095 Contrato
              IntToStrZero(0, 6)                                       + // 096 - 101 Borderô
              Space(5)                                                 + // 102 - 106 Brancos
              PadLeft(ACBrTitulo.Carteira,2,'0')                       + // 107 - 108 Carteira
              LTipoOcorrencia                                          + // 109 - 110 Ocorrência "Comando" Lista 254
              PadRight(ACBrTitulo.NumeroDocumento, 10, ' ')            + // 111 - 120 Nr. titulo dado pelo cedente
              FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)          + // 121 - 126 Data de vencimento
              IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 13) + // 127 - 139 Valor do titulo
              IntToStrZero(Numero, 3)                                  + // 140 - 142 Banco 089
              LAgencia                                                 + // 143 - 146 Numero da Agencia da Cooperativa
              '0'                                                      + // 147 - 147 DV Agencia Cobradora
              PadLeft(LTipoEspecieDoc,2,'0')                           + // 148 - 149 Especie Documento Lista 61
              LTipoAceite                                              + // 150 - 150 Aceite
              FormatDateTime('ddmmyy', ACBrTitulo.DataDocumento)       + // 151 - 156 Data de Emissão
              PadLeft(ACBrTitulo.Instrucao1,2,'0')                     + // 157 - 158 1ª instrução codificada
              PadLeft(ACBrTitulo.Instrucao2,2,'0')                     + // 159 - 160 2ª instrução codificada
              PadRight(LValorTaxaMoraMes,6,'0')                        + // 161 - 166 Taxa Juros de mora por mês
              PadRight(LValorTaxaMulta,6,'0')                          + // 167 - 172 Taxa de multa por mês
              Space(1)                                                 + // 173 - 173 Filler Branco
              LDataDesconto                                            + // 174 - 179 Data Primeiro Desconto
              IntToStrZero(Round(ACBrTitulo.ValorDesconto * 100), 13)  + // 180 - 192 Valor do desconto
              IntToStrZero(Round(ACBrTitulo.ValorIOF * 100), 13)       + // 193 - 205 Valor do IOF
              IntToStrZero(Round(ACBrTitulo.ValorAbatimento * 100), 13)+ // 206 - 218 Valor do abatimento
              LTipoSacado                                              + // 219 - 220 Tipo Inscricao
              PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF), 14, '0')  + // 221 - 234 CNPJ ou CPF do sacado
              PadRight(ACBrTitulo.Sacado.NomeSacado, 40)               + // 235 - 274 Nome do sacado
              PadRight(Trim(ACBrTitulo.Sacado.Logradouro) +
                ', ' +
                Trim(ACBrTitulo.Sacado.Numero) +
                ', ' +
              Trim(ACBrTitulo.Sacado.Complemento), 37)                 + // 275 - 311 Endereço do sacado
              PadRight(Trim(ACBrTitulo.Sacado.Bairro), 15)             + // 312 - 326 Bairro do sacado
              PadLeft(OnlyNumber(ACBrTitulo.Sacado.CEP), 8)            + // 327 - 334 CEP do endereço do sacado
              PadRight(Trim(ACBrTitulo.Sacado.Cidade), 15)             + // 335 - 349 Cidade do sacado
              PadRight(ACBrTitulo.Sacado.UF, 2)                        + // 350 - 351 UF da cidade do sacado
              PadRight(LMensagem, 40)                                  + // 352 - 391 Observações
              IfThen(ACBrTitulo.DiasDeNegativacao > 0,
                     PadLeft(IntToStr(ACBrTitulo.DiasDeNegativacao), 2, '0'),
                     PadLeft(IntToStr(ACBrTitulo.DiasDeProtesto), 2, '0')) + // 392 - 393 Dias de Proteste
              Space(1)                                                 + // 394 - 394 Filler Branco
              IntToStrZero(ARemessa.Count + 1, 6);                       // 395 - 400 N Sequencial Regigistro Arquivo
    ARemessa.add(LLinha);
end;

procedure TACBrBancoCredisan.GerarRegistroTrailler400(ARemessa: TStringList);
var
  LLinha: String;
begin
  LLinha := '9' +
            Space(393) +
            IntToStrZero(ARemessa.Count + 1, 6); // Contador de Registros

  ARemessa.Add(UpperCase(LLinha));
end;

procedure TACBrBancoCredisan.LerRetorno240(ARetorno: TStringList);
begin
  raise Exception.create(ACBrStr('Não permitido para o layout deste banco.'));
end;

function TACBrBancoCredisan.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  {Lista 257}


  case TipoOcorrencia of
    toRetornoRegistroConfirmado           : Result := '02';
    toRetornoComandoRecusado              : Result := '03';
    toRetornoTransferenciaCarteiraEntrada : Result := '04';
    toRetornoLiquidadoSemRegistro         : Result := '05';
    toRetornoLiquidado                    : Result := '06';
    toRetornoBaixaAutomatica              : Result := '09';
    toRetornoBaixaSolicitada              : Result := '10';
    toRetornoTituloEmSer                  : Result := '11';
    toRetornoAbatimentoConcedido          : Result := '12';
    toRetornoAbatimentoCancelado          : Result := '13';
    toRetornoVencimentoAlterado           : Result := '14';
    toRetornoRecebimentoInstrucaoProtestar: Result := '19';
    toRetornoDebitoEmConta                : Result := '20';
    toRetornoNomeSacadoAlterado           : Result := '21';
    toRetornoEnderecoSacadoAlterado       : Result := '22';
    toRetornoEntradaEmCartorio            : Result := '23';
    toRetornoProtestoSustado              : Result := '24';
    toRetornoJurosDispensados             : Result := '25';
    toRetornoInstrucaoRejeitada           : Result := '26';
    toRetornoConfirmacaoAlteracaoJurosMora: Result := '27';
    toRetornoManutencaoTituloVencido      : Result := '28';
    toRetornoAlteracaoDadosRejeitados     : Result := '30';
    toRetornoDespesasProtesto             : Result := '96';
    toRetornoDespesasSustacaoProtesto     : Result := '97';
    toRetornoDebitoCustasAntecipadas      : Result := '98';
  else
    Result := '';
  end;
end;

function TACBrBancoCredisan.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  {Lista 257}
  Result := '';
  case StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0) of
    02: Result := '02-Confirmação de Entrada de Título';
    03: Result := '03-Comando recusado';
    04: Result := '04-Transferencia de Carteira - Entrada';
    05: Result := '05-Liquidado sem registro';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixa de Título';
    10: Result := '10-Baixa Solicitada';
    11: Result := '11-Titulos em Ser';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Alteração de Vencimento do Titulo';
    15: Result := '15-Liquidação em Cartório';
    19: Result := '19-Confirmação de recebimento de instruções para protesto';
    20: Result := '20-Débito em Conta';
    21: Result := '21-Alteração do Nome do Sacado';
    22: Result := '22-Alteração do Endereço do Sacado';
    23: Result := '23-Encaminhamento para protesto';
    24: Result := '24-Sustar Protesto';
    25: Result := '25-Dispensar Juros';
    26: Result := '26-Instrução Rejeitada';
    27: Result := '27-Confirmação de Alteração de Dados';
    28: Result := '28-Manutenção de titulo vencido';
    30: Result := '30-Alteração de Dados Rejeitada';
    96: Result := '96-Despesas de Protesto';
    97: Result := '97-Despesas de Sustação de Protesto';
    98: Result := '98-Débito de Custas Antecipadas';
  end;

  Result := ACBrStr(Result);
end;

function TACBrBancoCredisan.CalcularNomeArquivoRemessa: string;
var Boleto : TACBrBoleto;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  fNumeroRemessa := Boleto.NumeroArquivo;

  Boleto.NomeArqRemessa := PadLeft(Boleto.Cedente.Conta, 9, '0')
                           + PadLeft(Boleto.Cedente.ContaDigito,1 , '0')
                           + FormatDateTime('ddmmyyyy', Now)
                           + '.REM';

  Result := Boleto.DirArqRemessa
            + PathDelim
            + Boleto.NomeArqRemessa;
end;

function TACBrBancoCredisan.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  {Lista 257}
  Result := toTipoOcorrenciaNenhum;

  case CodOcorrencia of
    02 : Result := toRetornoRegistroConfirmado;
    03 : Result := toRetornoComandoRecusado;
    04 : Result := toRetornoTransferenciaCarteiraEntrada;
    05 : Result := toRetornoLiquidadoSemRegistro;
    06 : Result := toRetornoLiquidado;
    09 : Result := toRetornoBaixaAutomatica;
    10 : Result := toRetornoBaixaSolicitada;
    11 : Result := toRetornoTituloEmSer;
    12 : Result := toRetornoAbatimentoConcedido;
    13 : Result := toRetornoAbatimentoCancelado;
    14 : Result := toRetornoVencimentoAlterado;
    19 : Result := toRetornoRecebimentoInstrucaoProtestar;
    20 : Result := toRetornoDebitoEmConta;
    21 : Result := toRetornoNomeSacadoAlterado;
    22 : Result := toRetornoEnderecoSacadoAlterado;
    23 : Result := toRetornoEntradaEmCartorio;
    24 : Result := toRetornoProtestoSustado;
    25 : Result := toRetornoJurosDispensados;
    26 : Result := toRetornoInstrucaoRejeitada;
    27 : Result := toRetornoConfirmacaoAlteracaoJurosMora;
    28 : Result := toRetornoManutencaoTituloVencido;
    30 : Result := toRetornoAlteracaoDadosRejeitados;
    96 : Result := toRetornoDespesasProtesto;
    97 : Result := toRetornoDespesasSustacaoProtesto;
    98 : Result := toRetornoDebitoCustasAntecipadas;
  end;
end;

function TACBrBancoCredisan.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar;
    03: Result := toRemessaDebitarEmConta;
    04: Result := toRemessaConcederAbatimento;
    05: Result := toRemessaCancelarAbatimento;
    06: Result := toRemessaAlterarVencimento;
    07: Result := toRemessaAlterarControleParticipante;
    08: Result := toRemessaAlterarNumeroControle;
    09: Result := toRemessaProtestar;
    10: Result := toRemessaCancelarInstrucaoProtestoBaixa;
    11: Result := toRemessaDispensarJuros;
    12: Result := toRemessaAlterarNomeEnderecoSacado;
    30: Result := toRemessaRecusaAlegacaoSacado;
    31: Result := toRemessaOutrasOcorrencias;
    34: Result := toRemessaBaixaporPagtoDiretoCedente;
    else
      Result := toRemessaRegistrar;
  end;
end;

procedure TACBrBancoCredisan.LerRetorno400(ARetorno: TStringList);
var
  LTitulo                             : TACBrTitulo;
  LContLinha, LCodOcorrencia, LCodMotivo,
  LTamanhoMaximoNossoNum: Integer;
  LAgencia, LDigitoAgencia, LConta   : String;
  LDigitoConta                       : String;
  LLinha, LCedente                   : String;
  LConvenioCedente                   : String;
  LBoleto : TACBrBoleto;
begin
  try
    LTamanhoMaximoNossoNum := fpTamanhoMaximoNossoNum;
    fpTamanhoMaximoNossoNum := 11;
    if StrToIntDef(copy(ARetorno.Strings[ 0 ], 77, 3), - 1) <> Numero then
      raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));
    LBoleto := ACBrBanco.ACBrBoleto;
    LCedente       := Trim(copy(ARetorno[ 0 ], 47, 30));
    LAgencia       := Trim(copy(ARetorno[ 0 ], 27, 4));
    LDigitoAgencia := copy(ARetorno[ 0 ], 31, 1);
    LConta         := Trim(copy(ARetorno[ 0 ], 32, 8));
    LDigitoConta   := copy(ARetorno[ 0 ], 40, 1);

    LConvenioCedente := copy(ARetorno[ 0 ], 41, 6);

    LBoleto.NumeroArquivo := StrToIntDef(copy(ARetorno[ 0 ], 101, 7), 0);
    LBoleto.DataArquivo   := StringToDateTimeDef(copy(ARetorno[ 0 ], 95, 2) + '/' + copy(ARetorno[ 0 ], 97, 2) + '/' + copy(ARetorno[ 0 ], 99, 2), 0, 'DD/MM/YY');

    ValidarDadosRetorno(LAgencia, LConta);

    if LBoleto.LeCedenteRetorno then
    begin
      LBoleto.Cedente.Nome          := LCedente;
      LBoleto.Cedente.Agencia       := LAgencia;
      LBoleto.Cedente.AgenciaDigito := LDigitoAgencia;
      LBoleto.Cedente.Conta         := LConta;
      LBoleto.Cedente.ContaDigito   := LDigitoConta;
      LBoleto.Cedente.Convenio      := LConvenioCedente;
    end;

    LBoleto.ListadeBoletos.Clear;


    for LContLinha := 1 to ARetorno.Count - 2 do
    begin
      LLinha := ARetorno[ LContLinha ];

      if (copy(LLinha, 1, 1) <> '7') and (copy(LLinha, 1, 1) <> '1') then
        Continue;

      LTitulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      LTitulo.SeuNumero               := copy(LLinha, 38, 25);
      LTitulo.NumeroDocumento         := copy(LLinha, 117, 10);
      LCodOcorrencia                  := StrToIntDef(copy(LLinha, 109, 2), 0);
      LTitulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(LCodOcorrencia);

      if (LCodOcorrencia in [ 5 .. 8, 15, 46 ]) then
      begin
        LTitulo.CodigoLiquidacao          := copy(LLinha, 109, 2);
        LTitulo.CodigoLiquidacaoDescricao := TipoOcorrenciaToDescricao(LTitulo.OcorrenciaOriginal.Tipo);
      end;

      if (LCodOcorrencia in [ 2 .. 10, 85, 86 ]) then
      begin
        LCodMotivo := StrToIntDef(copy(LLinha, 81, 2), 0);
        LTitulo.MotivoRejeicaoComando.Add(copy(LLinha, 81, 2));
        LTitulo.DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(LTitulo.OcorrenciaOriginal.Tipo, LCodMotivo));
      end;

      LTitulo.DataOcorrencia := StringToDateTimeDef(copy(LLinha, 111, 2) + '/' + copy(LLinha, 113, 2) + '/' + copy(LLinha, 115, 2), 0, 'DD/MM/YY');

      LTitulo.Vencimento := StringToDateTimeDef(copy(LLinha, 147, 2) + '/' + copy(LLinha, 149, 2) + '/' + copy(LLinha, 151, 2), 0, 'DD/MM/YY');

      LTitulo.ValorDocumento      := StrToFloatDef(copy(LLinha, 153, 13), 0) / 100;
      LTitulo.ValorDespesaCobranca := StrToFloatDef(copy(LLinha, 182, 07), 0) / 100;
      LTitulo.ValorOutrasDespesas  := StrToFloatDef(copy(LLinha, 189, 13), 0) / 100;
      LTitulo.ValorIOF            := StrToFloatDef(copy(LLinha, 215, 13), 0) / 100;
      LTitulo.ValorAbatimento     := StrToFloatDef(copy(LLinha, 228, 13), 0) / 100;
      LTitulo.ValorDesconto       := StrToFloatDef(copy(LLinha, 241, 13), 0) / 100;
      LTitulo.ValorRecebido       := StrToFloatDef(copy(LLinha, 254, 13), 0) / 100;
      LTitulo.ValorMoraJuros      := StrToFloatDef(copy(LLinha, 267, 13), 0) / 100;
      LTitulo.ValorOutrosCreditos := StrToFloatDef(copy(LLinha, 280, 13), 0) / 100;
      LTitulo.Carteira            := copy(LLinha, 107, 2);

      if LBoleto.LerNossoNumeroCompleto then
      begin
        fpTamanhoMaximoNossoNum := 10;
        LTitulo.NossoNumero          := copy(LLinha, 63, 10);
      end else
        LTitulo.NossoNumero          := copy(LLinha, 63, 11);

      if StrToIntDef(copy(LLinha, 176, 6), 0) <> 0 then
        LTitulo.DataCredito := StringToDateTimeDef(copy(LLinha, 176, 2) + '/' + copy(LLinha, 178, 2) + '/' + copy(LLinha, 180, 2), 0, 'DD/MM/YY');
    end;
  finally
    fpTamanhoMaximoNossoNum := LTamanhoMaximoNossoNum;
  end;
end;

end.
