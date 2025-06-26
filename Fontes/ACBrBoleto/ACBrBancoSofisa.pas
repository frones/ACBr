{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Giovanne Fontenele Trevia, Renato Rubinho       }
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

{$i ACBr.inc}
unit ACBrBancoSofisa;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  ACBrBoleto;

type
  { TACBrBancoSofisa }

  TACBrBancoSofisa = class(TACBrBancoClass)
  private
  protected
    vTotalTitulos : Double;
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList);  override;

    procedure LerRetorno240(ARetorno:TStringList); override;
    procedure LerRetorno400(ARetorno:TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
  {$ifdef COMPILER6_UP} DateUtils {$else} ACBrD5 {$endif},
  StrUtils, ACBrBoletoConversao, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TACBrBancoSofisa }

constructor TACBrBancoSofisa.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpDigito := 9;
  fpNome := 'Banco Sofisa';
  fpNumero := 637;
  fpTamanhoMaximoNossoNum := 10;
  fpTamanhoCarteira := 3;
  fpTamanhoConta := 10;
end;

function TACBrBancoSofisa.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
var
  sCampo: string;
  Documento: string;
  Contador, Peso: integer;
  Digito: integer;
begin
  Digito := 0;
  Try
    sCampo := PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') +
              Trim(ACBrTitulo.Carteira) +
              ACBrTitulo.NossoNumero;
    Documento := '';
    Peso := 2;
    for Contador := Length(sCampo) downto 1 do
    begin
      Try
        Documento := IntToStr(StrToInt(sCampo[Contador]) * Peso) + Documento;
      except
      end;
      if Peso = 1 then
        Peso := 2
      else
        Peso := 1;
    end;

    for Contador := 1 to Length(Documento) do
      Digito := Digito + StrToInt(Documento[Contador]);
    Digito := 10 - (Digito mod 10);
    if (Digito >= 10) then
      Digito := 0;
  finally
    Result := IntToStr(Digito);
  end;
end;

function TACBrBancoSofisa.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  CodigoBarras: String;
  FatorVencimento: String;
  DigitoCodBarras: String;
  DigitoNossoNumero: String;
  Boleto: TACBrBoleto;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);
  FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

  CodigoBarras := '637'+
                  '9'+
                  FatorVencimento +
                  IntToStrZero(Round(ACBrTitulo.ValorDocumento*100), 10) +
                  PadLeft(Trim(Boleto.Cedente.Agencia), 4, '0') +
                  Trim(ACBrTitulo.Carteira) +
                  PadLeft(Boleto.Cedente.Operacao, 7, '0') +
                  PadLeft(ACBrTitulo.NossoNumero, 10, '0') +
                  DigitoNossoNumero;

  DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);

  Result:= Copy(CodigoBarras,1,4) + DigitoCodBarras + Copy(CodigoBarras,5,43);
end;

function TACBrBancoSofisa.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  case StrToIntDef(ACBrTitulo.Carteira, 0) of
    1: ACBrTitulo.Carteira := '121';
    4: ACBrTitulo.Carteira := '101';
    5: ACBrTitulo.Carteira := '102';
    6: ACBrTitulo.Carteira := '201';
  end;

  Result := PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') +
            PadLeft(ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito, 1, '0') + '/'+
            PadLeft(ACBrTitulo.Carteira, 3, '0') + '/' +
            PadLeft( ACBrTitulo.NossoNumero, 10, '0') + '-' +
            CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoSofisa.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia +
            ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito + '/' +
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoSofisa.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  DigitoNossoNumero, Ocorrencia,aEspecie, sChaveNFe: String;
  Protesto, Multa, valorMulta, aAgencia, TipoSacado, wLinha: String;
  Boleto: TACBrBoleto;
  ADataDesconto: String;
  aCarteira, I, mensagemBranco, multiplicadorMulta: Integer;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  aCarteira := StrToIntDef(ACBrTitulo.Carteira, 0);

  if aCarteira = 102 then
    aCarteira := 5
  else if aCarteira = 201 then
    aCarteira := 6
  else if aCarteira = 101 then
    aCarteira := 4
  else if aCarteira = 121 then
    aCarteira := 6
  else
    aCarteira := 1;

  aAgencia := PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia) +
                      ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito, 5, '0');

  vTotalTitulos := vTotalTitulos + ACBrTitulo.ValorDocumento;
  DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

  // Pegando Código da Ocorrencia
  case ACBrTitulo.OcorrenciaOriginal.Tipo of
    toRemessaBaixar                   : Ocorrencia := '02'; // Pedido de Baixa
    toRemessaConcederAbatimento       : Ocorrencia := '04'; // Concessão de Abatimento
    toRemessaCancelarAbatimento       : Ocorrencia := '05'; // Cancelamento de Abatimento concedido
    toRemessaAlterarVencimento        : Ocorrencia := '06'; // Alteração de vencimento
    toRemessaProtestar                : Ocorrencia := '09'; // Pedido de protesto
    toRemessaNaoProtestar             : Ocorrencia := '10'; // Sustar protesto antes do início do ciclo de protesto
    toRemessaCancelarInstrucaoProtesto: Ocorrencia := '18'; // Sustar protesto e manter na carteira
  else
    Ocorrencia := '01';                                     // Remessa
  end;

  // Pegando Especie
  if Trim(ACBrTitulo.EspecieDoc) = 'DM' then
    aEspecie:= '01'
  else if Trim(ACBrTitulo.EspecieDoc) = 'NP' then
    aEspecie:= '02'
  else if Trim(ACBrTitulo.EspecieDoc) = 'CH' then
    aEspecie:= '03'
  else if Trim(ACBrTitulo.EspecieDoc) = 'LC' then
    aEspecie:= '04'
  else if Trim(ACBrTitulo.EspecieDoc) = 'RC' then
    aEspecie:= '05'
  else if Trim(ACBrTitulo.EspecieDoc) = 'AS' then
    aEspecie:= '08'
  else if Trim(ACBrTitulo.EspecieDoc) = 'DS' then
    aEspecie:= '12'
  else if Trim(ACBrTitulo.EspecieDoc) = 'CC' then
    aEspecie:= '31'
  else if Trim(ACBrTitulo.EspecieDoc) = 'OU' then
    aEspecie:= '99'
  else
    aEspecie := ACBrTitulo.EspecieDoc;

  // Pegando campo Intruções
  Protesto := '00';
  if (ACBrTitulo.DataProtesto > 0) and (ACBrTitulo.DataProtesto > ACBrTitulo.Vencimento) then
    Protesto := IntToStrZero(DaysBetween(ACBrTitulo.DataProtesto, ACBrTitulo.Vencimento), 2);

  // Pegando Dias Multa
  Multa := '00';
  if (ACBrTitulo.DataMulta > 0) and (ACBrTitulo.DataMulta > ACBrTitulo.Vencimento) then
    Multa := IntToStrZero(DaysBetween(ACBrTitulo.DataMulta, ACBrTitulo.Vencimento), 2);

  // Define Valor Multa
  if ACBrTitulo.MultaValorFixo then
    multiplicadorMulta := 100
  else
    multiplicadorMulta := 10000;

  valorMulta := IntToStrZero(Round(ACBrTitulo.PercentualMulta * multiplicadorMulta ), 13);

  // Pegando Tipo de Sacado
  case ACBrTitulo.Sacado.Pessoa of
    pFisica  : TipoSacado := '01';
    pJuridica: TipoSacado := '02';
  else
    TipoSacado := '99';
  end;

  if ACBrTitulo.DataDesconto > 0 then
    ADataDesconto := FormatDateTime('ddmmyy', ACBrTitulo.DataDesconto)
  else
    ADataDesconto := '000000';

  if (ACBrTitulo.ListaDadosNFe.Count > 0) then
    sChaveNFe := ACBrTitulo.ListaDadosNFe[0].ChaveNFe
  else
    sChaveNFe := '';

  wLinha := '1'                                                +  // 001 a 001 ID Registro
    IfThen(Boleto.Cedente.TipoInscricao = pJuridica,'02','01') +  // 002 a 003 Identificação do Tipo de Inscrição da empresa
    PadLeft(Trim(OnlyNumber(Boleto.Cedente.CNPJCPF)),14,'0')   +  // 004 a 017 Número de Inscrição da Empresa
    PadRight(Trim(Boleto.Cedente.CodigoTransmissao),20)        +  // 018 a 037 Identificação da empresa no Banco
    PadRight(ACBrTitulo.SeuNumero, 25)                         +  // 038 a 062 Identificação do Título na empresa
    PadLeft(RightStr(ACBrTitulo.NossoNumero,10),10,'0') +
            DigitoNossoNumero                                  +  // 063 a 073 Identificação do Título no Banco
    Space(13)                                                  +  // 074 a 086 Cobrança direta Título Correspondente
    Space(3)                                                   +  // 087 a 089 Modalidade de Cobrança com bancos correspondentes.
    IfThen(ACBrTitulo.PercentualMulta > 0,'2','0')             +  // 090 a 090 Código da Multa
    valorMulta                                                 +  // 091 a 103 Valor ou Taxa de Multa
    Multa                                                      +  // 104 a 105 Número de dias após o vencimento para aplicar a multa
    Space(2)                                                   +  // 106 a 107 Identificação da Operação no Banco
    IntToStr(aCarteira)                                        +  // 108 a 108 Código da Carteira
    Ocorrencia                                                 +  // 109 a 110 Identificação da Ocorrência
    PadRight(ACBrTitulo.NumeroDocumento, 10)                   +  // 111 a 120 Nro documento de Cobrança
    FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)            +  // 121 a 126 Data de vencimento do título
    IntToStrZero(Round( ACBrTitulo.ValorDocumento * 100), 13)  +  // 127 a 139 Valor Nominal do Título
    '637'                                                      +  // 140 a 142 Nro do Banco na Câmara de Compensação Bancária
    '0000'                                                     +  // 143 a 146 Agência encarregada da cobrança
    '0'                                                        +  // 147 a 147 Dígito de auto conferência da agência cobradora
    PadLeft(aEspecie, 2)                                       +  // 148 a 149 Espécie do título
    'N'                                                        +  // 150 a 150 Identificação do Título aceito ou não aceito
    FormatDateTime('ddmmyy', ACBrTitulo.DataDocumento )        +  // 151 a 156 Data da emissão do título
    PadLeft(Trim(ACBrTitulo.Instrucao1),2,'0')                 +  // 157 a 158 1a Instrução de Cobrança
    PadLeft(Trim(ACBrTitulo.Instrucao2),2,'0')                 +  // 159 a 160 2a Instrução de Cobrança
    IntToStrZero( Round(ACBrTitulo.ValorMoraJuros * 100 ), 13) +  // 161 a 173 Valor de mora por dia de atraso
    PadLeft(ADataDesconto, 6, '0')                             +  // 174 a 179 Data Limite para concessão de desconto                                                                                 + // 202 a 207 Data limite para concessão dodesconto 1
    IntToStrZero(Round(ACBrTitulo.ValorDesconto * 100), 13)    +  // 180 a 192 Valor do desconto a ser concedido
    IntToStrZero(Round(ACBrTitulo.ValorIOF * 100), 13)         +  // 193 a 205 Valor do I.O.F. a ser recolhido pelo Banco no caso de Notas de Seguro
    IntToStrZero(Round(ACBrTitulo.ValorAbatimento * 100), 13)  +  // 206 a 218 Valor do abatimento a ser concedido
    TipoSacado                                                 +  // 219 a 220 Identificação do tipo de inscrição do sacado
    PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF),14,'0')      +  // 221 a 234 Número de Inscrição do Sacado
    PadRight(ACBrTitulo.Sacado.NomeSacado, 30)                 +  // 235 a 264 Nome do Sacado
    Space(10)                                                  +  // 265 a 274 Complementação do Registro
    PadRight(Trim(ACBrTitulo.Sacado.Logradouro + ' ' +
                  ACBrTitulo.Sacado.Numero), 40)               +  // 275 a 314 Rua, Número e Complemento do Sacado
    PadRight(ACBrTitulo.Sacado.Bairro, 12)                     +  // 315 a 326 Bairro do Sacado
    PadRight(OnlyNumber(ACBrTitulo.Sacado.CEP), 8)             +  // 327 a 334 Código de Endereçamento Postal do Sacado
    PadRight(ACBrTitulo.Sacado.Cidade, 15)                     +  // 335 a 349 Cidade do Sacado
    PadRight(ACBrTitulo.Sacado.UF, 2)                          +  // 350 a 351 Estado (UF - Unidade da Federação ) do Sacado
    PadRight(ACBrTitulo.Sacado.NomeSacado, 30)                 +  // 352 a 381 Nome do Sacador ou Avalista
    Space(4)                                                   +  // 382 a 385 Complementação do Registro
    Space(6)                                                   +  // 386 a 391 Brancos
    Protesto                                                   +  // 392 a 393 Quantidade de dias para início da Ação de Protesto
    '0'                                                        +  // 394 a 394 Moeda
    IntToStrZero(aRemessa.Count + 1, 6)                        +  // 395 a 400 Número Sequencial do Registro no Arquivo
    sChaveNFe;                                                    // 401 a 444 Nº da Chave da Nota Fiscal Eletrônica

  wLinha := UpperCase(wLinha);
  if ACBrTitulo.Mensagem.Count > 0 then
  begin
    wLinha := wLinha + #13#10 +
      '2'                                                      +  // 001 a 001 Identificação do Registro
      '0';                                                        // 002 a 002 Zero

    for I := 0 to ACBrTitulo.Mensagem.Count - 1 do
    begin
      if i = 5  then
        Break;

      wLinha := wLinha +
        PadRight(ACBrTitulo.Mensagem[I],69);                      // 003 a 071 Mensagem Livre 69 posições
    end;                                                          // 072 a 140 Mensagem Livre 69 posições
                                                                  // 141 a 209 Mensagem Livre 69 posições
    mensagemBranco := (5 - i) * 69;                               // 210 a 278 Mensagem Livre 69 posições
    wLinha := wLinha + Space(mensagemBranco);                     // 279 a 347 Mensagem Livre 69 posições

    wLinha := wLinha + Space(47);                                 // 348 a 394 Brancos
    wLinha := wLinha + IntToStrZero(aRemessa.Count  + 2, 6);      // 395 a 400 Número Sequencial do Registro no Arquivo
  end;

  aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoSofisa.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.Create('Não permitido para o layout deste banco.');
end;

procedure TACBrBancoSofisa.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
  Boleto: TACBrBoleto;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  vTotalTitulos := 0;

  wLinha:= '0'                                            + // 001-001 ID do Registro
           '1'                                            + // 002-002 ID do Arquivo( 1 - Remessa)
           'REMESSA'                                      + // 003-009 Literal de Remessa
           '01'                                           + // 010-011 Código do Tipo de Serviço
           PadRight('COBRANCA', 15)                       + // 012-026 Descrição do tipo de serviço
           PadRight(Boleto.Cedente.CodigoTransmissao, 20) + // 027-046 Codigo da Empresa no Banco
           PadRight(Boleto.Cedente.Nome, 30)              + // 047-076 Nome da Empresa
           '637'                                          + // 077-079 Código
           PadRight('BANCO SOFISA SA', 15)                + // 080-094 Nome do Banco
           FormatDateTime('ddmmyy', Now)                  + // 095-100 Data de geração do arquivo
           Space(294)                                     + // 101-394 Brancos
           IntToStrZero(1,6);                               // 395-400 Nr. Sequencial de Remessa

  aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
end;

procedure TACBrBancoSofisa.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
  wLinha := '9'                                         + // 001-001 Identificação do Registro Trailler
            Space(393)                                  + // 002-394 Complmentação do Registro (Brancos)
            IntToStrZero(ARemessa.Count + 1, 6);          // 395-400 Nr. Sequencial de Remessa

  ARemessa.Text := ARemessa.Text + wLinha;
end;

procedure TACBrBancoSofisa.LerRetorno240(ARetorno: TStringList);
begin
  inherited;

  raise Exception.Create('Leitura de retorno padrão CNAB 240 não implementada para Banco Sofisa SA!');
end;

Procedure TACBrBancoSofisa.LerRetorno400(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  ContLinha, CodOcorrencia: Integer;
  CodMotivo: String;
  Linha, rCedente, rAgencia, rConta, rDigitoConta, rCNPJCPF: String;
  wCodBanco: Integer;
  LPosicao : Integer;
  I: Integer;
  Boleto: TACBrBoleto;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  wCodBanco := StrToIntDef(Copy(ARetorno.Strings[0], 77, 3), -1);

  if wCodBanco <> Numero then
    raise Exception.Create(Boleto.NomeArqRetorno +
                           'não é um arquivo de retorno do ' +
                           Nome);

  rCedente := Trim(Copy(ARetorno[0], 47, 30));
  rAgencia := Trim(Copy(ARetorno[1], 169, 4));
  rConta := '';
  rDigitoConta := '';

  if StrToIntDef(Copy(ARetorno[1], 2, 2), 0) = 0 then
  begin
    rCNPJCPF := Copy(ARetorno[1], 7, 11);
    Boleto.Cedente.TipoInscricao := pFisica;
  end
  else
  begin
    Boleto.Cedente.TipoInscricao := pJuridica;
    rCNPJCPF := Copy(ARetorno[1], 4, 14);
  end;

  Boleto.DataCreditoLanc :=
    StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                        Copy(ARetorno[0], 97, 2) + '/' +
                        Copy(ARetorno[0], 99, 2), 0, 'dd/mm/yy');

  ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);

  Boleto.Cedente.Nome := rCedente;
  Boleto.Cedente.CNPJCPF := rCNPJCPF;
  Boleto.Cedente.Agencia := rAgencia;
  Boleto.Cedente.AgenciaDigito := Copy(ARetorno[1],173,1);
  Boleto.Cedente.Conta := rConta;
  Boleto.Cedente.ContaDigito := rDigitoConta;

  Boleto.NumeroArquivo := StrToIntDef(Trim(Copy(ARetorno[0], 109, 5)),0);
  Boleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                                            Copy(ARetorno[0], 97, 2) + '/' +
                                            Copy(ARetorno[0], 99, 2), 0, 'dd/mm/yy');

  Boleto.ListadeBoletos.Clear;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if Copy(Linha, 1, 1) <> '1' then
      Continue;

    Titulo := Boleto.CriarTituloNaLista;

    Titulo.SeuNumero := Trim(Copy(Linha, 38, 25));
    Titulo.NossoNumero := Copy(Linha, 63, 10);
    Titulo.NossoNumeroCorrespondente := Copy(Linha, 95, 13);
    Titulo.Carteira := Copy(Linha, 108, 1);

    Titulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(Linha, 109, 2), 0));

    Titulo.DataOcorrencia := StringToDateTimeDef(Copy(Linha, 111, 2) + '/' +
                                                 Copy(Linha, 113, 2) + '/' +
                                                 Copy(Linha, 115, 2), 0, 'dd/mm/yy');

    Titulo.NumeroDocumento := Copy(Linha, 117, 10);

    CodOcorrencia := StrToIntDef(Copy(Linha, 109, 2),0);

    if CodOcorrencia > 0 then
    begin
      LPosicao := 378;
      for I := 1 to 4 do
      begin
        CodMotivo := Copy(Linha, LPosicao, 2);
        if CodMotivo <> '00' then
        begin
          Titulo.MotivoRejeicaoComando.Add(Copy(Linha, LPosicao, 2));
          Titulo.DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(Titulo.OcorrenciaOriginal.Tipo, CodMotivo));
          Inc(LPosicao, 2);
        end;
      end;
    end;

    Titulo.Vencimento := StringToDateTimeDef(Copy(Linha, 147, 2) + '/' +
                                             Copy(Linha, 149, 2) + '/' +
                                             Copy(Linha, 151, 2), 0, 'dd/mm/yy');

    Titulo.ValorDocumento := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;

    case StrToIntDef(Copy(Linha, 174, 2), 0) of
      01: Titulo.EspecieDoc:= 'DM';
      02: Titulo.EspecieDoc:= 'NP';
      03: Titulo.EspecieDoc:= 'CH';
      04: Titulo.EspecieDoc:= 'LC';
      05: Titulo.EspecieDoc:= 'RC';
      08: Titulo.EspecieDoc:= 'AS';
      12: Titulo.EspecieDoc:= 'DS';
      31: Titulo.EspecieDoc:= 'CC';
      99: Titulo.EspecieDoc:= 'OU';
    end;

    Titulo.ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100;
    Titulo.ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
    Titulo.ValorIOF             := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
    Titulo.ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
    Titulo.ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
    Titulo.ValorRecebido        := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;

    if StrToIntDef(Copy(Linha, 386, 6), 0) <> 0 then
      Titulo.DataCredito := StringToDateTimeDef(Copy(Linha, 386, 2) + '/' +
                                                Copy(Linha, 388, 2) + '/' +
                                                Copy(Linha, 390, 2), 0, 'dd/mm/yy');
  end;
end;

function TACBrBancoSofisa.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    raise Exception.Create('c240 não implementado!')
  else
  begin
    case CodOcorrencia of
      02: Result := 'Entrada Confirmada';
      03: Result := 'Entrada Rejeitada';
      05: Result := 'Campo Livre Alterado';
      06: Result := 'Liquidação Normal';
      08: Result := 'Liquidação em Cartório';
      09: Result := 'Baixa Automática';
      10: Result := 'Baixa por ter sido liquidado';
      12: Result := 'Confirma Abatimento';
      13: Result := 'Abatimento Cancelado';
      14: Result := 'Vencimento Alterado';
      15: Result := 'Baixa Rejeitada';
      16: Result := 'Instrução Rejeitada';
      19: Result := 'Confirma Recebimento de Ordem de Protesto';
      20: Result := 'Confirma Recebimento de Ordem de Sustação';
      22: Result := 'Seu número alterado';
      23: Result := 'Título enviado para cartório';
      24: Result := 'Confirma recebimento de ordem de não protestar';
      28: Result := 'Débito de Tarifas/Custas - Correspondentes';
      40: Result := 'Tarifa de Entrada (debitada na Liquidação)';
      43: Result := 'Baixado por ter sido protestado';
      96: Result := 'Tarifa Sobre Instruções - Mês anterior';
      97: Result := 'Tarifa Sobre Baixas - Mês Anterior';
      98: Result := 'Tarifa Sobre Entradas - Mês Anterior';
      99: Result := 'Tarifa Sobre Instruções de Protesto/Sustação - Mês Anterior';
    end;
  end;
end;

function TACBrBancoSofisa.CodOcorrenciaToTipo(const CodOcorrencia: Integer ): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
    raise Exception.Create('Código de ocorrência não implementado para c240!')
  else
  begin
    case CodOcorrencia of
      02: Result := toRetornoEntradaConfirmadaNaCip;              // Entrada Confirmada
      03: Result := toRetornoRemessaRejeitada;                    // Entrada Rejeitada
      // 05:                                                         Campo Livre Alterado
      06: Result :=  toRetornoLiquidado;                          // Liquidação Normal
      08: Result := toRetornoLiquidadoEmCartorio;                 // Liquidação em Cartório
      09: Result := toRetornoBaixaAutomatica;                     // Baixa Automática
      10: Result := toRetornoLiquidado;                           // Baixa por ter sido liquidado
      12: Result := toRetornoAbatimentoConcedido;                 // Confirma Abatimento
      13: Result := toRetornoAbatimentoCancelado;                 // Abatimento Cancelado
      14: Result := toRetornoVencimentoAlterado;                  // Vencimento Alterado
      15: Result := toRetornoBaixaRejeitada;                      // Baixa Rejeitada
      16: Result := toRetornoInstrucaoRejeitada;                  // Instrução Rejeitada
      19: Result := toRetornoConfInstrucaoProtesto;               // Confirma Recebimento de Ordem de Protesto
      20: Result := toRetornoConfInstrucaoSustacaoProtesto;       // Confirma Recebimento de Ordem de Sustação
      22: Result := toRetornoAlteracaoSeuNumero;                  // Seu número alterado
      23: Result := toRetornoEncaminhadoACartorio;                // Título enviado para cartório
      24: Result := toRetornoRecebimentoInstrucaoNaoProtestar;    // Confirma recebimento de ordem de não protestar
      28: Result := toRetornoDebitoTarifas;                       // Débito de Tarifas/Custas - Correspondentes
      40: Result := toRetornoDebitoTarifas;                       // Tarifa de Entrada (debitada na Liquidação)
      43: Result := toRetornoBaixaPorProtesto;                    // Baixado por ter sido protestado
      // 96                                                          Tarifa Sobre Instruções - Mês anterior
      // 97                                                          Tarifa Sobre Baixas - Mês Anterior
      // 98                                                          Tarifa Sobre Entradas - Mês Anterior
      // 99                                                          Tarifa Sobre Instruções de Protesto/Sustação - Mês Anterior
    else
      if CodOcorrencia > 0 then
        Result := toRetornoOutrasOcorrencias;                     // Outras
    end;
  end;
end;

function TACBrBancoSofisa.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar;                      // Pedido de Baixa
    04: Result := toRemessaConcederAbatimento;          // Concessão de Abatimento
    05: Result := toRemessaCancelarAbatimento;          // Cancelamento de Abatimento concedido
    06: Result := toRemessaAlterarVencimento;           // Alteração de vencimento
    07: Result := toRemessaAlterarControleParticipante; // Alteração do controle do participante
    08: Result := toRemessaAlterarNumeroControle;       // Alteração de seu número
    09: Result := toRemessaProtestar;                   // Pedido de protesto
    18: Result := toRemessaCancelarInstrucaoProtesto;   // Sustar protesto e manter na carteira
    98: Result := toRemessaNaoProtestar;                // Sustar protesto antes do início do ciclo de protesto
  else
    Result := toRemessaRegistrar;                       // Remessa
  end;
end;

function TACBrBancoSofisa.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
    raise Exception.Create('c240 não implementado!')
  else
  begin
    case TipoOcorrencia of
      toRetornoEntradaConfirmadaNaCip: Result := '02';
      toRetornoRemessaRejeitada: Result := '03';
      toRetornoLiquidado: Result := '06';
      toRetornoLiquidadoEmCartorio: Result := '08';
      toRetornoBaixaAutomatica: Result := '09';
      // toRetornoLiquidado: Result := 'Baixa por ter sido liquidado';
      toRetornoAbatimentoConcedido: Result := '12';
      toRetornoAbatimentoCancelado: Result := '13';
      toRetornoVencimentoAlterado: Result := '14';
      toRetornoBaixaRejeitada: Result := '15';
      toRetornoInstrucaoRejeitada: Result := '16';
      toRetornoConfInstrucaoProtesto: Result := '19';
      toRetornoConfInstrucaoSustacaoProtesto: Result := '20';
      toRetornoAlteracaoSeuNumero: Result := '22';
      toRetornoEncaminhadoACartorio: Result := '23';
      toRetornoRecebimentoInstrucaoNaoProtestar: Result := '24';
      toRetornoDebitoTarifas: Result := '28';
      // toRetornoDebitoTarifas: Result := '40 Tarifa de Entrada (debitada na Liquidação)';
      toRetornoBaixaPorProtesto: Result := '43';
    else
      raise Exception.Create('Código de tipo de ocorrência identerminado!');
    end;
  end;
end;

function TACBrBancoSofisa.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: string): String;
const
  Motivos03: array [0..101] of string = (
    '03 CEP inválido - Não temos cobrador - Cobrador não Localizado',
    '04 Sigla do Estado inválida',
    '05 Data de Vencimento inválida ou fora do prazo mínimo',
    '06 Código do Banco inválido',
    '08 Nome do sacado não informado',
    '10 Logradouro não informado',
    '14 Registro em duplicidade',
    '19 Data de desconto inválida ou maior que a data de vencimento',
    '20 Valor de IOF não numérico',
    '21 Movimento para título não cadastrado no sistema',
    '22 Valor de desconto + abatimento maior que o valor do título',
    '25 CNPJ ou CPF do sacado inválido (aceito com restrições)',
    '26 Espécie de documento inválida',
    '27 Data de emissão do título inválida',
    '28 Seu número não informado',
    '29 CEP é igual a espaço ou zeros; ou não numérico',
    '30 Valor do título não numérico ou inválido',
    '36 Valor de permanência (mora) não numérico',
    '37 Valor de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título',
    '38 Valor de desconto/abatimento não numérico ou inválido',
    '39 Valor de abatimento não numérico',
    '42 Título já existente em nossos registros. Nosso número não aceito',
    '43 Título enviado em duplicidade nesse movimento',
    '44 Título zerado ou em branco; ou não numérico na remessa',
    '46 Título enviado fora da faixa de Nosso Número, estipulada para o cliente',
    '51 Tipo/Número de Inscrição Sacador/Avalista Inválido',
    '52 Sacador/Avalista não informado',
    '53 Prazo de vencimento do título excede ao da contratação',
    '54 Banco informado não é nosso correspondente 140-142',
    '55 Banco correspondente informado não cobra este CEP ou não possui faixas de CEP cadastradas',
    '56 Nosso número no correspondente não foi informado',
    '57 Remessa contendo duas instruções incompatíveis - não protestar e dias de protesto ou prazo para protesto inválido',
    '58 Entradas Rejeitadas - Reprovado no Represamento para Análise',
    '60 CNPJ/CPF do sacado inválido - título recusado',
    '87 Excede Prazo máximo entre emissão e vencimento',
    'AA Serviço de cobrança inválido',
    'AB Serviço de "0" ou "5" e banco cobrador <> zeros',
    'AE Título não possui abatimento',
    'AG Movto não permitido para título À Vista/Contra Apresentação',
    'AH Cancelamento de Valores Inválidos',
    'AI Nossa carteira inválida',
    'AJ Modalidade com bancos correspondentes inválida',
    'AK Título pertence a outro cliente',
    'AL Sacado impedido de entrar nesta cobrança',
    'AT Valor Pago Inválido',
    'AU Data da ocorrência inválida',
    'AV Valor da tarifa de cobrança inválida',
    'AX Título em pagamento parcial',
    'AY Título em Aberto e Vencido para acatar protestol',
    'BA Banco Correspondente Recebedor não é o Cobrador Atual',
    'BB Título deve estar em cartório para baixar',
    'BC Análise gerencial-sacado inválido p/operação crédito',
    'BD Análise gerencial-sacado inadimplente',
    'BE Análise gerencial-sacado difere do exigido',
    'BF Análise gerencial-vencto excede vencto da operação de crédito',
    'BG Análise gerencial-sacado com baixa liquidez',
    'BH Análise gerencial-sacado excede concentração',
    'CC Valor de iof incompatível com a espécie documento',
    'CD Efetivação de protesto sem agenda válida',
    'CE Título não aceito - pessoa física',
    'CF Excede prazo máximo da entrada ao vencimento',
    'CG Título não aceito - por análise gerencial',
    'CH Título em espera - em análise pelo banco',
    'CJ Análise gerencial-vencto do titulo abaixo przcurto',
    'CK Análise gerencial-vencto do titulo abaixo przlongo',
    'CS Título rejeitado pela checagem de duplicatas',
    'DA Análise gerencial - Entrada de Título Descontado com limite cancelado',
    'DB Análise gerencial - Entrada de Título Descontado com limite vencido',
    'DC Análise gerencial - cedente com limite cancelado',
    'DD Análise gerencial - cedente é sacado e teve seu limite cancelado',
    'DE Análise gerencial - apontamento no Serasa',
    'DG Endereço sacador/avalista não informado',
    'DH Cep do sacador/avalista não informado',
    'DI Cidade do sacador/avalista não informado',
    'DJ Estado do sacador/avalista inválido ou n informado',
    'DM Cliente sem Código de Flash cadastrado no cobrador',
    'DN Título Descontado com Prazo ZERO - Recusado',
    'DP Data de Referência menor que a Data de Emissão do Título',
    'DT Nosso Número do Correspondente não deve ser informado',
    'EB HSBC não aceita endereço de sacado com mais de 38 caracteres',
    'G1 Endereço do sacador incompleto (lei 12.039)',
    'G2 Sacador impedido de movimentar',
    'G3 Concentração de cep não permitida',
    'G4 Valor do título não permitido',
    'HA Serviço e Modalidade Incompatíveis',
    'HB Inconsistências entre Registros Título e Sacador',
    'HC Ocorrência não disponível',
    'HD Título com Aceite',
    'HF Baixa Liquidez do Sacado',
    'HG Sacado Informou que não paga Boletos',
    'HH Sacado não confirmou a Nota Fiscal',
    'HI Checagem Prévia não Efetuada',
    'HJ Sacado desconhece compra e Nota Fiscal',
    'HK Compra e Nota Fiscal canceladas pelo sacado',
    'HL Concentração além do permitido pela área de Crédito',
    'HM Vencimento acima do permitido pelo área de Crédito',
    'HN Excede o prazo limite da operação',
    'IX Título de Cartão de Crédito não aceita instruções',
    'JB Título de Cartão de Crédito inválido para o Produto',
    'JC Produto somente para Cartão de Crédito',
    'JH CB Direta com operação de Desconto Automático',
    'JI Espécie de Documento incompatível para produto de Cartão de Crédito');

  Motivos15: array [0..2] of string = (
    '05 Solicitação de baixa para título já baixado ou liquidado',
    '06 Solicitação de baixa para título não registrado no sistema',
    '08 Solicitação de baixa para título em float');

  Motivos16: array [0..37] of string = (
    '04 Data de vencimento não numérica ou inválida',
    '05 Data de Vencimento inválida ou fora do prazo mínimo',
    '14 Registro em duplicidade',
    '19 Data de desconto inválida ou maior que a data de vencimento',
    '20 Campo livre não informado',
    '21 Título não registrado no sistema',
    '22 Título baixado ou liquidado',
    '26 Espécie de documento inválida',
    '27 Instrução não aceita, por não ter sido emitida ordem de protesto ao cartório',
    '28 Título tem instrução de cartório ativa',
    '29 Título não tem instrução de carteira ativa',
    '30 Existe instrução de não protestar, ativa para o título',
    '36 Valor de permanência (mora) não numérico',
    '37 Título Descontado - Instrução não permitida para a carteira',
    '38 Valor do abatimento não numérico ou maior que a soma do valor do título + permanência + multa',
    '39 Título em cartório',
    '40 Instrução recusada - Reprovado no Represamento para Análise',
    '44 Título zerado ou em branco; ou não numérico na remessa',
    '51 Tipo/Número de Inscrição Sacador/Avalista Inválido',
    '53 Prazo de vencimento do título excede ao da contratação',
    '57 Remessa contendo duas instruções incompatíveis - não protestar e dias de protesto ou prazo para protesto inválido.',
    'AA Serviço de cobrança inválido',
    'AE Título não possui abatimento',
    'AG Movimento não permitido - Título à vista ou contra apresentação',
    'AH Cancelamento de valores inválidos',
    'AI Nossa carteira inválida',
    'AK Título pertence a outro cliente',
    'AU Data da ocorrência inválida',
    'AY Título deve estar em aberto e vencido para acatar protesto',
    'CB Título possui protesto efetivado/a efetivar hoje',
    'CT Título já baixado',
    'CW Título já transferido',
    'DO Título em Prejuízo',
    'JK Produto não permite alteração de valor de título',
    'JQ Título em Correspondente - Não alterar Valor',
    'JS Título possui Descontos/Abto/Mora/Multa',
    'JT Título possui Agenda de Protesto/Devolução',
    '99 Ocorrência desconhecida na remessa');

var
  i: integer;
begin
  Result := '';
  if ACBrBanco.ACBrBoleto.LayoutRemessa = c400 then
  begin
    case TipoOcorrencia of
      toRetornoRemessaRejeitada:
      begin
        for i := Low(Motivos03) to High(Motivos03) do
        begin
          if Pos(CodMotivo, Motivos03[i]) = 1 then
          begin
            Result := Motivos03[i];
            Exit;
          end;
          Result := 'Código de motivo não reconhecido!';
        end;
      end;
      toRetornoBaixaRejeitada:
      begin
        for i := Low(Motivos15) to High(Motivos15) do
        begin
          if Pos(CodMotivo, Motivos15[i]) = 1 then
          begin
            Result := Motivos15[i];
            Exit;
          end;
          Result := 'Código de motivo não reconhecido!';
        end;
      end;
      toRetornoInstrucaoRejeitada:
      begin
        for i := Low(Motivos16) to High(Motivos16) do
        begin
          if Pos(CodMotivo, Motivos15[i]) = 1 then
          begin
            Result := Motivos16[i];
            Exit;
          end;
          Result := 'Código de motivo não reconhecido!';
        end;
      end;
    end;
  end
  else // 240
  begin
    raise Exception.Create('Cnab 240 não implementado!');
  end;
end;

end.
