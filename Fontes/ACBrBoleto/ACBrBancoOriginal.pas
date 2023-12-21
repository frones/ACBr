{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Murilo Pavan                             }
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
unit ACBrBancoOriginal;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  ACBrBoleto,
  ACBrBoletoConversao;

type
  { TACBrBancoOriginal }
  TACBrBancoOriginal = class(TACBrBancoClass)
  protected
    function GetLocalPagamento: String; override;
  private
    function MontarConvenio: string;
    procedure MontarRegistroMensagem400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string; override;
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

constructor TACBrBancoOriginal.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpNome := 'ORIGINAL';
  fpNumero := 212;
  fpDigito := 7;
  fpTamanhoMaximoNossoNum := 10;
  fpTamanhoAgencia := 4;
  fpTamanhoConta := 7;
  fpTamanhoCarteira := 3;
end;

function TACBrBancoOriginal.CalcularDigitoVerificador(
  const ACBrTitulo: TACBrTitulo): string;
begin
  Modulo.FormulaDigito := frModulo10;
  Modulo.MultiplicadorFinal := 1;
  Modulo.MultiplicadorInicial := 2;
  Modulo.Documento :=
    ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.Carteira
    + PadLeft(ACBrTitulo.NossoNumero, fpTamanhoMaximoNossoNum, '0');
  Modulo.Calcular;
  Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoOriginal.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras: string;
  CampoLivre: string;
  Boleto: TACBrBoleto;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  if StrToInt64Def(ACBrTitulo.NossoNumero, 0) = 0 then
    raise Exception.create(ACBrStr('Banco ' + Boleto.Banco.Nome + ':: Nosso Número não Informado'));

  FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

  CampoLivre := Boleto.Cedente.Agencia
    + ACBrTitulo.Carteira
    + Boleto.Cedente.Operacao
    + ACBrTitulo.NossoNumero
    + CalcularDigitoVerificador(ACBrTitulo);

  CodigoBarras := IntToStrZero(Boleto.Banco.Numero, 3)
    + '9'
    + FatorVencimento
    + IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10)
    + CampoLivre;

  DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);

  Result := copy(CodigoBarras, 1, 4)
    + DigitoCodBarras
    + copy(CodigoBarras, 5, 39);
end;

function TACBrBancoOriginal.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
var
  NossoNr, DV: string;
begin
  NossoNr := PadLeft(ACBrTitulo.NossoNumero, 10, '0');
  DV := CalcularDigitoVerificador(ACBrTitulo);
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
    + '/' + ACBrTitulo.Carteira
    + '/' + NossoNr
    + '-' + DV;
end;

function TACBrBancoOriginal.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
    + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito
    + '/'
    + ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoOriginal.GerarRegistroHeader400(NumeroRemessa: Integer;
  aRemessa: TStringList);
var
  wLinha: string;
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;

  wLinha := '0' + //                        001 a 001 Identificação do registro
  '1' + //                                  002 a 002 Identificação do arquivo remessa
  'REMESSA' + //                            003 a 009 Literal remessa
  '01' + //                                 010 a 011 Código de serviço
  PadRight('COBRANCA', 15, ' ') + //        012 a 026 Literal serviço
  PadRight(MontarConvenio, 20, ' ') + //    027 a 046 Código da empresa
  PadRight(Beneficiario.Nome, 30, ' ') + // 047 a 076 Nome da empresa
  IntToStrZero(ACBrBanco.Numero, 3) + //    077 a 079 Número do Original na câmara de compensação
  PadRight('BANCO ORIGINAL', 15, ' ') + //  080 a 094 Nome do banco por extenso
  FormatDateTime('ddmmyy', Now) + //        095 a 100 Data da gravação do arquivo
  Space(294) + //                           101 a 394 Brancos
  '000001'; //                              395 a 400 Nº sequencial do registro de um em um

  ARemessa.Add(UpperCase(wLinha));
end;

procedure TACBrBancoOriginal.GerarRegistroTransacao400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  Boleto: TACBrBoleto;
  CodigoMulta: string;
  wLinha: string;
  I: Integer;
  ValorMulta, DiasMulta: Integer;
  Linha: TStringList;
  TipoBoleto: Char;
  EspecieDoc: string;
  DiasProtesto: Integer;
begin
  Boleto := ACBrTitulo.ACBrBoleto;

  CodigoMulta := '0';

  if (ACBrTitulo.PercentualMulta > 0) then
    CodigoMulta := '2' //percentual
  else if (ACBrTitulo.MultaValorFixo) then
    CodigoMulta := '1'; //Valor fixo

  ValorMulta := 0;

  if (ACBrTitulo.PercentualMulta > 0) then
    ValorMulta := Round(ACBrTitulo.PercentualMulta * 100)
  else if (ACBrTitulo.MultaValorFixo) then
    ValorMulta := Round(ACBrTitulo.ValorDocumento * ACBrTitulo.PercentualMulta);

  DiasMulta := 0;

  if ValorMulta > 0 then
    DiasMulta := 1;

  {Pegando Tipo de Boleto}
  case Boleto.Cedente.ResponEmissao of
    tbCliEmite: TipoBoleto := 'D'; //codigo 6 Cobrança Expressa - Orientação do banco enviar D
    tbBancoReemite: TipoBoleto := '1';
    tbBancoEmite: TipoBoleto := '1';
  else
    TipoBoleto := 'D'; //Cobrança Expressa
  end;

  if ACBrTitulo.EspecieDoc = 'DM' then
    EspecieDoc := '01'
  else if ACBrTitulo.EspecieDoc = 'NP' then
    EspecieDoc := '02'
  else if ACBrTitulo.EspecieDoc = 'CH' then
    EspecieDoc := '03'
  else if ACBrTitulo.EspecieDoc = 'LC' then
    EspecieDoc := '04'
  else if ACBrTitulo.EspecieDoc = 'RC' then
    EspecieDoc := '05'
  else if ACBrTitulo.EspecieDoc = 'AP' then
    EspecieDoc := '08'
  else if ACBrTitulo.EspecieDoc = 'DS' then
    EspecieDoc := '12'
  else
    EspecieDoc := '99';

  DiasProtesto := ACBrTitulo.DiasDeProtesto;

  if DiasProtesto = 0 then
    if ACBrTitulo.DataProtesto > 0 then
      DiasProtesto := DaysBetween(ACBrTitulo.DataProtesto, ACBrTitulo.Vencimento);

  wLinha := '';
  Linha := TStringList.Create;
  try
    Linha.Add('1'); //                                                          001 a 001 Identificação do registro de transação
    Linha.Add(IfThen(Boleto.Cedente.TipoInscricao = pFisica, '01', '02')); //   002 a 003 Identificação do Tipo de Inscrição da empresa
    Linha.Add(PadLeft(OnlyNumber(Boleto.Cedente.CNPJCPF), 14, '0')); //         004 a 017 Número de Inscrição da Empresa (CNPJ/CPF)
    Linha.Add(PadRight(MontarConvenio, 20, ' ')); //                            018 a 037 Identificação da empresa no Original
    Linha.Add(PadRight(ACBrTitulo.SeuNumero, 25, ' ')); //                      038 a 062 Identificação do Título na empresa
    Linha.Add(PadLeft(ACBrTitulo.NossoNumero, 10, '0')); //                     063 a 072 Identificação do Título no Banco
    Linha.Add(CalcularDigitoVerificador(ACBrTitulo)); //                        073 0 073 DV Nosso Número
    Linha.Add(Space(13)); //                                                    074 a 086 Cobrança Direta Título Correspondente
    Linha.Add(Space(03)); //                                                    087 a 089 Modalidade de Cobrança com bancos correspondentes
    Linha.Add(CodigoMulta); //                                                  090 a 090 Código da Multa 0 - Sem multa 1 - Valor fixo 2 - percentual
    Linha.Add(IntToStrZero(ValorMulta, 13)); //                                 091 a 103 Código da Multa 0 - Sem multa 1 - Valor fixo 2 - percentual
    Linha.Add(IntToStrZero(DiasMulta, 2)); //                                   104 a 105 Número de Dias Após o Vencimento para aplicar a Multa
    Linha.Add(Space(02)); //                                                    106 a 107 Número de Brancos
    Linha.Add(TipoBoleto); //                                                   108 a 108 Código da Carteira :: Carteira 6 alterada segundo suporte do banco para carteira D
    Linha.Add('01'); //                                                         109 a 110 Identificação da ocorrência 01 REMESSA
    Linha.Add(PadRight(ACBrTitulo.SeuNumero, 10, ' ')); //                      111 a 120 N? documento de Cobrança (Duplicata, Promissória etc.)
    Linha.Add(FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)); //              121 a 126 Data de vencimento do título
    Linha.Add(IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 13)); //     127 a 139 Valor do título
    Linha.Add('212'); //                                                        140 a 142 Código do Banco
    Linha.Add(IntToStrZero(0, 4)); //                                           143 a 146  Agência encarregada da cobrança ZEROS
    Linha.Add(IntToStrZero(0, 1)); //                                           147 a 147  DV Agência encarregada da cobrança ZEROS
    Linha.Add(EspecieDoc); //                                                   148 a 149 Espécie do título
    Linha.Add(IfThen(ACBrTitulo.Aceite = atSim, 'A', 'N')); //                  150 a 150 Aceite (A ou N)
    Linha.Add(FormatDateTime('ddmmyy', ACBrTitulo.DataDocumento)); //           151 a 156 Data da emissão do título
    Linha.Add(IfThen(DiasProtesto > 0, '00', '10')); //                         157 a 158 Instrução 1 - Se “INSTRO1 ou INSTRO2 = 10”, o Sistema entenderá que o cedente não deseja, de forma alguma, que ao título seja anexada a informação de DIAS DE PROTESTO.
    Linha.Add('00'); //                                                         159 a 160 Instrução 2
    Linha.Add(IntToStrZero(Round(ACBrTitulo.ValorMoraJuros * 100), 13)); //     161 a 173 Valor de mora por dia de atraso
    Linha.Add(IfThen(ACBrTitulo.DataDesconto > 0,
      FormatDateTime('ddmmyy', ACBrTitulo.DataDesconto), '000000')); //         174 a 179 Data Limite para concessão de desconto
    Linha.Add(IntToStrZero(Round(ACBrTitulo.ValorDesconto * 100), 13)); //      180 a 192 Valor do desconto a ser concedido
    Linha.Add(IntToStrZero(0, 13)); //                                          193 a 205 Valor do I.O.F. a ser recolhido pelo Banco no caso de seguro
    Linha.Add(IntToStrZero(Round(ACBrTitulo.ValorAbatimento * 100), 13)); //    206 a 218 Valor do abatimento a ser concedido
    Linha.Add(IfThen(ACBrTitulo.Sacado.Pessoa = pFisica, '01', '02')); //       219 a 220 Identificação do tipo de inscrição do sacado
    Linha.Add(PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF), 14, '0')); //      221 a 234 Número de Inscrição do Sacado
    Linha.Add(PadRight(TiraAcentos(ACBrTitulo.Sacado.NomeSacado), 30, ' ')); // 234 a 264 Nome do Sacado
    Linha.Add(Space(10)); //                                                    265 a 274 Complementação do Registro - Brancos
    Linha.Add(PadRight(TiraAcentos(ACBrTitulo.Sacado.Logradouro) + ' ' +
      ACBrTitulo.Sacado.Numero + ' ' +
      ACBrTitulo.Sacado.Complemento, 40, ' ')); //                              275 a 314 Rua, Número e Complemento do Sacado
    Linha.Add(PadRight(TiraAcentos(ACBrTitulo.Sacado.Bairro), 12, ' ')); //     315 a 326 Bairro do Sacado
    Linha.Add(PadLeft(OnlyNumber(ACBrTitulo.Sacado.CEP), 8, '0')); //           327 a 334 CEP do Sacado
    Linha.Add(PadRight(TiraAcentos(ACBrTitulo.Sacado.Cidade), 15, ' ')); //     335 a 349 Cidade do Sacado
    Linha.Add(PadRight(TiraAcentos(ACBrTitulo.Sacado.UF), 2, ' ')); //          350 a 351 Bairro do Sacado
    linha.Add(Space(30)); //                                                    352 a 381 Nome do Sacador ou Avalista
    linha.Add(Space(10)); //                                                    382 a 391 Complementação do Registro - Brancos
    Linha.Add(IfThen(DiasProtesto > 0, IntToStrZero(DiasProtesto, 2), '00')); //392 a 393 Quantidade de dias para início da Ação de Protesto
    Linha.Add('0'); //                                                          394 a 394 Moeda 0 ou 1 Moeda Corrente Nacional
    Linha.Add(IntToStrZero(ARemessa.Count + 1, 6)); //                          395 a 400 Número Seqüencial do Registro no Arquivo

    for I := 0 to Pred(Linha.Count) do
      wLinha := wLinha + Linha[I];
  finally
    ARemessa.Add(UpperCase(wLinha));
    MontarRegistroMensagem400(ACBrTitulo, aRemessa);
    FreeAndNil(Linha);
  end;
end;

procedure TACBrBancoOriginal.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: string;
begin
  wLinha := '9' + //                      001 a 001 Identificação registro
  Space(393) + //                         002 a 394 Branco
  IntToStrZero(ARemessa.Count + 1, 6); // 395 a 400 Número sequencial de registro

  ARemessa.Add(UpperCase(wLinha));
end;

function TACBrBancoOriginal.MontarConvenio: string;
var
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;
  Result := '090';
  Result := Result + PadLeft(Beneficiario.Agencia, 4, '0');
  Result := Result + PadLeft(Beneficiario.AgenciaDigito, 1, '0');
  Result := Result + PadLeft(Beneficiario.Operacao, 7, '0');
end;

procedure TACBrBancoOriginal.MontarRegistroMensagem400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  Mensagem: array[0..4] of string;
  i: integer;
  wLinha: string;
begin
  wLinha := EmptyStr;
  //5 linhas de mensagens
  for i := 0 to Pred(ACBrTitulo.Informativo.Count) do
  begin
    if i > 4 then Break;
    Mensagem[i] := ACBrTitulo.Informativo[i];
  end;

  //Cobrança garantida, Original exige uma mensagem informativa
  //Pedimos que seja incluída a mensagem abaixo, por tratar-se de Cobrança com garantia de duplicatas:
  //TÍTULO CEDIDO AO BANCO ORIGINAL S/A. FICA VEDADO PAGTO DE QUALQUER OUTRA FORMA QUE NÃO ATRAVÉS DO PRESENTE

  if NaoEstaVazio(ACBrTitulo.Informativo.Text) then
  begin
    wLinha := '2' + //                          001 a 001 Identificação registro
    '0' + //                                    002 a 002 Zero
    PadRight(TiraAcentos(Mensagem[0]), 69) + // 003 a 071 Mensagem Livre 1 69 posições
    PadRight(TiraAcentos(Mensagem[1]), 69) + // 072 a 140 Mensagem Livre 2 69 posições
    PadRight(TiraAcentos(Mensagem[2]), 69) + // 141 a 209 Mensagem Livre 3 69 posições
    PadRight(TiraAcentos(Mensagem[3]), 69) + // 210 a 278 Mensagem Livre 4 69 posições
    PadRight(TiraAcentos(Mensagem[4]), 69) + // 279 a 347 Mensagem Livre 5 69 posições
    Space(47) + //                              280 a 394 Brancos
    IntToStrZero(ARemessa.Count + 1, 6); //     395 a 400 Número sequencial de registro

    ARemessa.Add(UpperCase(wLinha));
  end;
end;

procedure TACBrBancoOriginal.LerRetorno400(ARetorno: TStringList);
var
  ContLinha: Integer;
  Linha: string;
  Titulo: TACBrTitulo;
  Boleto: TACBrBoleto;
  ColunaMotivoRejeicao : integer;
  QtdeMotivosRejeicao : integer;
begin
  Boleto := ACBrBanco.ACBrBoleto;

  if (StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero) then
    raise Exception.create(ACBrStr(Boleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

  Boleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2)
    + '/'
    + Copy(ARetorno[0], 97, 2)
    + '/'
    + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

  Boleto.Cedente.Nome := Trim(Copy(ARetorno[0], 47, 30));
  Boleto.Cedente.CNPJCPF := Trim(Copy(ARetorno[1], 4, 14));
  Boleto.Cedente.Operacao := Trim(Copy(ARetorno[1], 26, 12));

  case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
    01: Boleto.Cedente.TipoInscricao := pFisica;
  else
    Boleto.Cedente.TipoInscricao := pJuridica;
  end;

  Boleto.ListadeBoletos.Clear;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if (Copy(Linha, 1, 1) <> '1') then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    case StrToInt(copy(Linha, 108, 1)) of
      1: Titulo.CaracTitulo := tcSimples;
      2: Titulo.CaracTitulo := tcVinculada;
      3: Titulo.CaracTitulo := tcCaucionada;
      4: Titulo.CaracTitulo := tcDescontada;
    end;

    Titulo.SeuNumero := copy(Linha, 117, 10);
    Titulo.NumeroDocumento := copy(Linha, 38, 25);
    Titulo.Carteira := copy(Linha, 83, 3);

    Titulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 109, 2), 0));

    if Titulo.OcorrenciaOriginal.Tipo in [toRetornoRegistroRecusado,
      toRetornoBaixaRejeitada, toRetornoInstrucaoRejeitada] then
    begin
      // Este banco permite 3 motivos rejeição
      ColunaMotivoRejeicao := 378; // posição da primeira rejeicao
      for QtdeMotivosRejeicao := 1 to 3 do
      begin
        if Copy(Linha, ColunaMotivoRejeicao, 2)<>'00' then
        begin
          Titulo.MotivoRejeicaoComando.Add(Copy(Linha, ColunaMotivoRejeicao, 2));
          Titulo.DescricaoMotivoRejeicaoComando.add(CodMotivoRejeicaoToDescricao(Titulo.OcorrenciaOriginal.Tipo,copy(Linha, ColunaMotivoRejeicao, 2)));
        end;
        ColunaMotivoRejeicao := ColunaMotivoRejeicao + 2; // incrementa 2 posicoes para próxima rejeicao
      end;


    end;

    Titulo.DataOcorrencia := StringToDateTimeDef(Copy(Linha, 111, 2) + '/' + Copy(Linha, 113, 2) + '/' + Copy(Linha, 115, 2), 0, 'DD/MM/YY');

    case StrToIntDef(Copy(Linha, 174, 2), 0) of
      01: Titulo.EspecieDoc := 'DM';
      02: Titulo.EspecieDoc := 'NP';
      03: Titulo.EspecieDoc := 'CH';
      04: Titulo.EspecieDoc := 'LC';
      05: Titulo.EspecieDoc := 'RC';
      08: Titulo.EspecieDoc := 'AS';
      12: Titulo.EspecieDoc := 'DS';
      31: Titulo.EspecieDoc := 'CC';
      99: Titulo.EspecieDoc := 'Outros';
    end;

    if (StrToIntDef(Copy(Linha, 147, 6), 0) <> 0) then
      Titulo.Vencimento := StringToDateTimeDef(Copy(Linha, 147, 2)
        + '/'
        + Copy(Linha, 149, 2)
        + '/'
        + Copy(Linha, 151, 2), 0, 'DD/MM/YY');

    Titulo.ValorDocumento       := StrToFloatDef(Copy(Linha, 153, 13), 0) / 100;
    Titulo.ValorRecebido        := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
    Titulo.ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
    Titulo.ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
    Titulo.ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
    Titulo.ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100;

    Titulo.NossoNumero := Copy(Linha, 63, TamanhoMaximoNossoNum);// TamanhoNossoNumero 11
    if (StrToIntDef(Copy(Linha, 386, 6), 0) <> 0) then
      Titulo.DataCredito := StringToDateTimeDef(Copy(Linha, 386, 2)
        + '/'
        + Copy(Linha, 388, 2)
        + '/'
        + Copy(Linha, 390, 2), 0, 'DD/MM/YY');
  end;
end;


function TACBrBancoOriginal.CodMotivoRejeicaoToDescricao(
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
  result := ACBrStr(result);


end;


function TACBrBancoOriginal.CodOcorrenciaToTipo(
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

function TACBrBancoOriginal.TipoOcorrenciaToDescricao(
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
  result := ACBrStr(Result);
end;

function TACBrBancoOriginal.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoRegistro);
end;

end.

