{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit CNAB240.LerTxtRetorno;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrPagForLerTxt, ACBrPagForClass, ACBrPagForConversao;

type
  { TArquivoR_CNAB240 }

  TArquivoR_CNAB240 = class(TArquivoRClass)
  private
    FArquivoTXT: TStringList;

  protected
    Linha: string;

    procedure LerRegistro0; virtual;

    procedure LerRegistro1(nLinha: Integer); virtual;

    procedure LerRegistro5(nLinha: Integer); virtual;

    procedure LerRegistro9(nLinha: Integer); virtual;

    procedure LerSegmentoA(nLinha: Integer); virtual;

    procedure LerSegmentoB(mSegmentoBList: TSegmentoBList; nLinha: Integer); virtual;

    procedure LerSegmentoC(mSegmentoCList: TSegmentoCList; nLinha: Integer); virtual;

    procedure LerSegmentoE(nLinha: Integer); virtual;

    procedure LerSegmentoF(mSegmentoFList: TSegmentoFList; nLinha: Integer); virtual;

    procedure LerSegmentoG(nLinha: Integer); virtual;

    procedure LerSegmentoH(mSegmentoHList: TSegmentoHList; nLinha: Integer); virtual;

    procedure LerSegmentoJ(nLinha: Integer; var LeuRegistroJ: boolean); virtual;

    procedure LerSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List; nLinha: Integer); virtual;

    procedure LerSegmentoJ99(mSegmentoJ99List: TSegmentoJ99List; nLinha: Integer); virtual;

    procedure LerSegmentoN(mSegmentoN: TSegmentoN); virtual;

    procedure LerSegmentoN1(nLinha: Integer); virtual;

    procedure LerSegmentoN2(nLinha: Integer); virtual;

    procedure LerSegmentoN3(nLinha: Integer); virtual;

    procedure LerSegmentoN4(nLinha: Integer); virtual;

    procedure LerSegmentoN567(nLinha: Integer); virtual;

    procedure LerSegmentoN8(nLinha: Integer); virtual;

    procedure LerSegmentoN9(nLinha: Integer); virtual;

    procedure LerSegmentoO(nLinha: Integer); virtual;

    procedure LerSegmentoW(mSegmentoWList: TSegmentoWList; nLinha: Integer); virtual;

    procedure LerSegmentoZ(mSegmentoZList: TSegmentoZList; nLinha: Integer); virtual;

    procedure LerLote;
    procedure GerarAvisos(const aCodOcorrencia, aSegmento, aSegmentoFilho,
                          aSeuNumero: string);

    function GetOcorrencia(aOcorrencia: TOcorrencia): string; virtual;
  public
    function LerTxt: Boolean; override;

    property ArquivoTXT: TStringList read FArquivoTXT write FArquivoTXT;
  end;

implementation

{ TArquivoR_CNAB240 }

procedure TArquivoR_CNAB240.GerarAvisos(const aCodOcorrencia, aSegmento,
  aSegmentoFilho, aSeuNumero: string);
var
  xCodigo, xOcorrencias, xDescricao: string;
  xOcorrencia: TOcorrencia;
  Ok: Boolean;
begin
  // O código de ocorrencia pode ter até 5 códigos de 2 dígitos cada
  xOcorrencias := aCodOcorrencia;

  while length(xOcorrencias) > 0 do
  begin
    xCodigo := Copy(xOcorrencias, 1, 2);
    xOcorrencia := StrToTOcorrencia(Ok, xCodigo);

    if xCodigo <> '  ' then
    begin
      PagFor.Registro0.Aviso.New;

      if Ok then
        xDescricao := GetOcorrencia(xOcorrencia)
      else
        xDescricao := '(' + xCodigo + ') Retorno Nao Identificado';

      with PagFor.Registro0.Aviso.Last do
      begin
        CodigoRetorno := xCodigo;
        MensagemRetorno := xDescricao;
        Segmento := aSegmento;
        SegmentoFilho := aSegmentoFilho;
        SeuNumero := aSeuNumero;
      end;
    end;

    Delete(xOcorrencias, 1, 2);
  end;
end;

function TArquivoR_CNAB240.GetOcorrencia(aOcorrencia: TOcorrencia): string;
begin
  case aOcorrencia of
    to00: Result := 'Credito ou Debito Efetivado';
    to01: Result := 'Insuficiencia de Fundos - Debito Nao Efetuado';
    to02: Result := 'Credito ou Debito Cancelado pelo Pagador/Credor';
    to03: Result := 'Debito Autorizado pela Agencia - Efetuado';

    toAA: Result := 'Controle Invalido';
    toAB: Result := 'Tipo de Operacao Invalido';
    toAC: Result := 'Tipo de Servico Invalido';
    toAD: Result := 'Forma de Lancamento Invalida';
    toAE: Result := 'Tipo/Numero de Inscricao Invalido';
    toAF: Result := 'Codigo de Convenio Invalido';
    toAG: Result := 'Agencia/Conta Corrente/DV Invalido';
    toAH: Result := 'N. Sequencial do Registro no Lote Invalido';
    toAI: Result := 'Codigo de Segmento de Detalhe Invalido';
    toAJ: Result := 'Tipo de Movimento Invalido';
    toAK: Result := 'Codigo da Camara de Compensacao do Banco Favorecido/Depositario Invalido';
    toAL: Result := 'Codigo do Banco Favorecido, Instituicao de Pagamento ou Depositario Invalido';
    toAM: Result := 'Agencia Mantenedora da Conta Corrente do Favorecido Invalida';
    toAN: Result := 'Conta Corrente/DV/Conta de Pagamento do Favorecido Invalido';
    toAO: Result := 'Nome do Favorecido Nao Informado';
    toAP: Result := 'Data Lancamento Invalido';
    toAQ: Result := 'Tipo/Quantidade da Moeda Invalido';
    toAR: Result := 'Valor do Lancamento Invalido';
    toAS: Result := 'Aviso ao Favorecido - Identificacao Invalida';
    toAT: Result := 'Tipo/Numero de Inscricao do Favorecido Invalido';
    toAU: Result := 'Logradouro do Favorecido Nao Informado';
    toAV: Result := 'N. do Local do Favorecido Nao Informado';
    toAW: Result := 'Cidade do Favorecido Nao Informada';
    toAX: Result := 'CEP/Complemento do Favorecido Invalido';
    toAY: Result := 'Sigla do Estado do Favorecido Invalida';
    toAZ: Result := 'Codigo/Nome do Banco Depositario Invalido';

    toBA: Result := 'Codigo/Nome da Agencia Depositaria Nao Informado';
    toBB: Result := 'Seu Numero Invalido';
    toBC: Result := 'Nosso Numero Invalido';
    toBD: Result := 'Inclusao Efetuada com Sucesso';
    toBE: Result := 'Alteracao Efetuada com Sucesso';
    toBF: Result := 'Exclusao Efetuada com Sucesso';
    toBG: Result := 'Agencia/Conta Impedida Legalmente/Bloqueada';
    toBH: Result := 'Empresa nao pagou salario';
    toBI: Result := 'Falecimento do mutuario';
    toBJ: Result := 'Empresa nao enviou remessa do mutuario';
    toBK: Result := 'Empresa nao enviou remessa no vencimento';
    toBL: Result := 'Valor da parcela invalida';
    toBM: Result := 'Identificacao do contrato invalida';
    toBN: Result := 'Operacao de Consignacao Incluida com Sucesso';
    toBO: Result := 'Operacao de Consignacao Alterada com Sucesso';
    toBP: Result := 'Operacao de Consignacao Excluida com Sucesso';
    toBQ: Result := 'Operacao de Consignacao Liquidada com Sucesso';
    toBR: Result := 'Reativacao Efetuada com Sucesso';
    toBS: Result := 'Suspensao Efetuada com Sucesso';

    toCA: Result := 'Codigo de Barras - Codigo do Banco Invalido';
    toCB: Result := 'Codigo de Barras - Codigo da Moeda Invalido';
    toCC: Result := 'Codigo de Barras - Digito Verificador Geral Invalido';
    toCD: Result := 'Codigo de Barras - Valor do Titulo Invalido';
    toCE: Result := 'Codigo de Barras - Campo Livre Invalido';
    toCF: Result := 'Valor do Documento Invalido';
    toCG: Result := 'Valor do Abatimento Invalido';
    toCH: Result := 'Valor do Desconto Invalido';
    toCI: Result := 'Valor de Mora Invalido';
    toCJ: Result := 'Valor da Multa Invalido';
    toCK: Result := 'Valor do IR Invalido';
    toCL: Result := 'Valor do ISS Invalido';
    toCM: Result := 'Valor do IOF Invalido';
    toCN: Result := 'Valor de Outras Deducoes Invalido';
    toCO: Result := 'Valor de Outros Acrescimos Invalido';
    toCP: Result := 'Valor do INSS Invalido';

    toHA: Result := 'Lote Nao Aceito';
    toHB: Result := 'Inscricao da Empresa Invalida para o Contrato';
    toHC: Result := 'Convenio com a Empresa Inexistente/Invalido para o Contrato';
    toHD: Result := 'Agencia/Conta Corrente da Empresa Inexistente/Invalido para o Contrato';
    toHE: Result := 'Tipo de Servico Invalido para o Contrato';
    toHF: Result := 'Conta Corrente da Empresa com Saldo Insuficiente';
    toHG: Result := 'Lote de Servico Fora de Sequencia';
    toHH: Result := 'Lote de Servico Invalido';
    toHI: Result := 'Arquivo nao aceito';
    toHJ: Result := 'Tipo de Registro Invalido';
    toHK: Result := 'Codigo Remessa / Retorno Invalido';
    toHL: Result := 'Versao de layout invalida';
    toHM: Result := 'Mutuario nao identificado';
    toHN: Result := 'Tipo do beneficio nao permite emprestimo';
    toHO: Result := 'Beneficio cessado/suspenso';
    toHP: Result := 'Beneficio possui representante legal';
    toHQ: Result := 'Beneficio e do tipo PA (Pensao alimenticia)';
    toHR: Result := 'Quantidade de contratos permitida excedida';
    toHS: Result := 'Beneficio nao pertence ao Banco informado';
    toHT: Result := 'Inicio do desconto informado ja ultrapassado';
    toHU: Result := 'Numero da parcela invalida';
    toHV: Result := 'Quantidade de parcela invalida';
    toHW: Result := 'Margem consignavel excedida para o mutuario dentro do prazo do contrato';
    toHX: Result := 'Emprestimo ja cadastrado';
    toHY: Result := 'Emprestimo inexistente';
    toHZ: Result := 'Emprestimo ja encerrado';
    toH1: Result := 'Arquivo sem trailer';
    toH2: Result := 'Mutuario sem credito na competencia';
    toH3: Result := 'Nao descontado – outros motivos';
    toH4: Result := 'Retorno de Credito nao pago';
    toH5: Result := 'Cancelamento de emprestimo retroativo';
    toH6: Result := 'Outros Motivos de Glosa';
    toH7: Result := 'Margem consignavel excedida para o mutuario acima do prazo do contrato';
    toH8: Result := 'Mutuario desligado do empregador';
    toH9: Result := 'Mutuario afastado por licenca';

    toIA: Result := 'Primeiro nome do mutuario diferente do primeiro nome do movimento do censo ou diferente da base de Titular do Beneficio';
    toIB: Result := 'Beneficio suspenso/cessado pela APS ou Sisobi';
    toIC: Result := 'Beneficio suspenso por dependencia de calculo';
    toID: Result := 'Beneficio suspenso/cessado pela inspetoria/auditoria';
    toIE: Result := 'Beneficio bloqueado para emprestimo pelo beneficiario';
    toIF: Result := 'Beneficio bloqueado para emprestimo por TBM';
    toIG: Result := 'Beneficio esta em fase de concessao de PA ou desdobramento';
    toIH: Result := 'Beneficio cessado por obito';
    toII: Result := 'Beneficio cessado por fraude';
    toIJ: Result := 'Beneficio cessado por concessao de outro beneficio';
    toIK: Result := 'Beneficio cessado: estatutario transferido para orgao de origem';
    toIL: Result := 'Emprestimo suspenso pela APS';
    toIM: Result := 'Emprestimo cancelado pelo banco';
    toIN: Result := 'Credito transformado em PAB';
    toIO: Result := 'Termino da consignacao foi alterado';
    toIP: Result := 'Fim do emprestimo ocorreu durante periodo de suspensao ou concessao';
    toIQ: Result := 'Emprestimo suspenso pelo banco';
    toIR: Result := 'Nao averbacao de contrato – quantidade de parcelas/competencias informadas ultrapassou a data limite da extincao de cota do dependente titular de beneficios';

    toPA: Result := 'Pix nao efetivado';
    toPB: Result := 'Transacao interrompida devido a erro no PSP do Recebedor';
    toPC: Result := 'Numero da conta transacional encerrada no PSP do Recebedor';
    toPD: Result := 'Tipo incorreto para a conta transacional especificada';
    toPE: Result := 'Tipo de transacao nao e suportado/autorizado na conta transacional especificada';
    toPF: Result := 'CPF/CNPJ do usuario recebedor nao e consistente com o titular da conta transacional especificada';
    toPG: Result := 'CPF/CNPJ do usuario recebedor incorreto';
    toPH: Result := 'Ordem rejeitada pelo PSP do Recebedor';
    toPI: Result := 'ISPB do PSP do Pagador invalido ou inexistente';
    toPJ: Result := 'Chave nao cadastrada no DICT';
    toPK: Result := 'QR Code invalido/vencido';
    toPL: Result := 'Forma de iniciacao invalida';
    toPM: Result := 'Chave de pagamento invalida';
    toPN: Result := 'Chave de pagamento nao informada';

    toTA: Result := 'Lote Nao Aceito - Totais do Lote com Diferenca';

    toYA: Result := 'Titulo Nao Encontrado';
    toYB: Result := 'Identificador Registro Opcional Invalido';
    toYC: Result := 'Codigo Padrao Invalido';
    toYD: Result := 'Codigo de Ocorrencia Invalido';
    toYE: Result := 'Complemento de Ocorrencia Invalido';
    toYF: Result := 'Alegacao ja Informada';
    // Observação:
    // As ocorrências iniciadas com 'ZA' tem caráter informativo para o cliente
    toZA: Result := 'Agencia / Conta do Favorecido Substituida';
    toZB: Result := 'Divergencia entre o primeiro e ultimo nome do beneficiario versus primeiro e ultimo nome na Receita Federal';
    toZC: Result := 'Confirmacao de Antecipacao de Valor';
    toZD: Result := 'Antecipacao parcial de valor';
    toZE: Result := 'Titulo bloqueado na base';
    toZF: Result := 'Sistema em contingencia – titulo valor maior que referencia';
    toZG: Result := 'Sistema em contingencia – titulo vencido';
    toZH: Result := 'Sistema em contingencia – titulo indexado';
    toZI: Result := 'Beneficiario divergente';
    toZJ: Result := 'Limite de pagamentos parciais excedido';
    toZK: Result := 'Boleto ja liquidado';
  else
    Result := '';
  end;
end;

procedure TArquivoR_CNAB240.LerRegistro0;
var
  mOk: Boolean;
begin
  PagFor.Registro0.Aviso.Clear;
  Linha := ArquivoTXT.Strings[0];

  PagFor.Geral.Banco := StrToBanco(mOk, LerCampo(Linha, 1, 3, tcStr));

  with PagFor.Registro0.Empresa do
  begin
    Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
    Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);
    Convenio := LerCampo(Linha, 33, 20, tcStr);

    ContaCorrente.Agencia.Codigo := LerCampo(Linha, 53, 5, tcInt);
    ContaCorrente.Agencia.DV := LerCampo(Linha, 58, 1, tcStr);
    ContaCorrente.Conta.Numero := LerCampo(Linha, 59, 12, tcInt64);
    ContaCorrente.Conta.DV := LerCampo(Linha, 71, 1, tcStr);
    ContaCorrente.DV := LerCampo(Linha, 72, 1, tcStr);

    Nome := LerCampo(Linha, 73, 30, tcStr);
  end;

  with PagFor.Registro0 do
  begin
    NomeBanco := LerCampo(Linha, 103, 30, tcStr);
    // 10 Brancos entre o NomeBanco e Codigo
    Arquivo.Codigo := StrToTpArquivo(mOk, LerCampo(Linha, 143, 1, tcStr));
    Arquivo.DataGeracao := LerCampo(Linha, 144, 8, tcDat);
    Arquivo.HoraGeracao := LerCampo(Linha, 152, 6, tcHor);
    Arquivo.Sequencia := LerCampo(Linha, 158, 6, tcInt);
    // 3 Versão do Layout do Arquivo
    Arquivo.Densidade := LerCampo(Linha, 167, 5, tcInt);
    // 20 Reservado ao Banco
    // 20 Reservado a Empresa
    // 29 Para uso exclusivo FEBRABAN
  end;
end;

procedure TArquivoR_CNAB240.LerRegistro1(nLinha: Integer);
var
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];

  PagFor.Lote.New;

  with PagFor.Lote.Last.Registro1.Servico do
  begin
    Operacao := StrToTpOperacao(mOk, LerCampo(Linha, 9, 1, tcStr));
    TipoServico := StrToTpServico(mOk, LerCampo(Linha, 10, 2, tcStr));
    FormaLancamento := StrToFmLancamento(mOk, LerCampo(Linha, 12, 2, tcStr));
  end;

  with PagFor.Lote.Last.Registro1.Empresa do
  begin
    Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
    Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);
    Convenio := LerCampo(Linha, 33, 20, tcStr);

    ContaCorrente.Agencia.Codigo := LerCampo(Linha, 53, 5, tcInt);
    ContaCorrente.Agencia.DV := LerCampo(Linha, 58, 1, tcStr);
    ContaCorrente.Conta.Numero := LerCampo(Linha, 59, 12, tcInt64);
    ContaCorrente.Conta.DV := LerCampo(Linha, 71, 1, tcStr);
    ContaCorrente.DV := LerCampo(Linha, 72, 1, tcStr);

    Nome := LerCampo(Linha, 73, 30, tcStr);
  end;

  PagFor.Lote.Last.Registro1.Informacao1 := LerCampo(Linha, 103, 40, tcStr);

  case PagFor.Lote.Last.Registro1.Servico.TipoServico of
    tsConciliacaoBancaria:
      begin
        with PagFor.Lote.Last.Registro1 do
        begin
          Data := LerCampo(Linha, 143, 8, tcDat);
          Valor := LerCampo(Linha, 151, 18, tcDe2);
          Situacao := LerCampo(Linha, 169, 1, tcStr);
          Status := LerCampo(Linha, 170, 1, tcStr);
          TipoMoeda := LerCampo(Linha, 171, 3, tcStr);
          Sequencia := LerCampo(Linha, 174, 5, tcInt);
        end;
      end;
  else
    begin
      with PagFor.Lote.Last.Registro1.Endereco do
      begin
        Logradouro := LerCampo(Linha, 143, 30, tcStr);
        Numero := LerCampo(Linha, 173, 5, tcInt);
        Complemento := LerCampo(Linha, 178, 15, tcStr);
        Cidade := LerCampo(Linha, 193, 20, tcStr);
        CEP := LerCampo(Linha, 213, 8, tcInt);
        Estado := LerCampo(Linha, 221, 2, tcStr);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerRegistro5(nLinha: Integer);
begin
  Linha := ArquivoTXT.Strings[nLinha];

  case PagFor.Lote.Last.Registro1.Servico.TipoServico of
    tsConciliacaoBancaria:
      begin
        with PagFor.Lote.Last.Registro5 do
        begin
          BloqueadoAcima24h := LerCampo(Linha, 89, 18, tcDe2);
          Limite := LerCampo(Linha, 107, 18, tcDe2);
          BloqueadoAte24h := LerCampo(Linha, 125, 18, tcDe2);
          Data := LerCampo(Linha, 143, 8, tcDat);
          Valor := LerCampo(Linha, 151, 18, tcDe2);
          Situacao := LerCampo(Linha, 169, 1, tcStr);
          Status := LerCampo(Linha, 170, 1, tcStr);
          QtdeRegistros := LerCampo(Linha, 171, 6, tcInt);
          ValorDebitos := LerCampo(Linha, 177, 18, tcDe2);
          ValorCreditos := LerCampo(Linha, 195, 18, tcDe2);
        end;
      end;
  else
    begin
      with PagFor.Lote.Last.Registro5 do
      begin
        Valor := LerCampo(Linha, 24, 18, tcDe2);
        QtdeMoeda := LerCampo(Linha, 42, 18, tcDe5);
        NumAvisoDebito := LerCampo(Linha, 60, 6, tcInt);
      end;
    end;
  end;

  with PagFor.Lote.Last.Registro5 do
  begin
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, '5', '', '');
  end;
end;

procedure TArquivoR_CNAB240.LerRegistro9(nLinha: Integer);
begin
  Linha := ArquivoTXT.Strings[nLinha];

  PagFor.Registro9.Totais.QtdeLotes := LerCampo(Linha, 18, 6, tcInt);
  PagFor.Registro9.Totais.QtdeRegistros := LerCampo(Linha, 24, 6, tcInt);
  PagFor.Registro9.Totais.QtdeContasConciliadas := LerCampo(Linha, 30, 6, tcInt);
end;

procedure TArquivoR_CNAB240.LerSegmentoA(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
  x: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3A' then
    Exit;

  PagFor.Lote.Last.SegmentoA.New;

  with PagFor.Lote.Last.SegmentoA.Last do
  begin
    TipoMovimento := StrToTpMovimento(mOk, LerCampo(Linha, 15, 1, tcStr));
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    Favorecido.Camara := LerCampo(Linha, 18, 3, tcInt);
    Favorecido.Banco := StrToBanco(mOk, LerCampo(Linha, 21, 3, tcStr));

    with Favorecido do
    begin
      ContaCorrente.Agencia.Codigo := LerCampo(Linha, 24, 5, tcInt);
      ContaCorrente.Agencia.DV := LerCampo(Linha, 29, 1, tcStr);
      ContaCorrente.Conta.Numero := LerCampo(Linha, 30, 12, tcInt64);
      ContaCorrente.Conta.DV := LerCampo(Linha, 42, 1, tcStr);
      ContaCorrente.DV := LerCampo(Linha, 43, 1, tcStr);
    end;

    Favorecido.Nome := LerCampo(Linha, 44, 20, tcStr);
    Credito.SeuNumero := LerCampo(Linha, 74, 20, tcStr);
    Credito.DataPagamento := LerCampo(Linha, 94, 8, tcDat);

    with Credito do
    begin
      Moeda.Tipo := StrToTpMoeda(mOk, LerCampo(Linha, 102, 3, tcStr));
      Moeda.Qtde := LerCampo(Linha, 105, 15, tcDe5);
      ValorPagamento := LerCampo(Linha, 120, 15, tcDe2);
      NossoNumero := LerCampo(Linha, 135, 20, tcStr);
      DataReal := LerCampo(Linha, 155, 8, tcDat);
      ValorReal := LerCampo(Linha, 163, 15, tcDe2);
    end;

    Informacao2 := LerCampo(Linha, 178, 40, tcStr);
    CodigoDOC := LerCampo(Linha, 218, 2, tcStr);

    Aviso := LerCampo(Linha, 230, 1, tcInt);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'A', '', Credito.SeuNumero);
  end;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0 do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida
    {opcionais do segmento A}
    LerSegmentoB(PagFor.Lote.Last.SegmentoA.Last.SegmentoB, nLinha);
    LerSegmentoC(PagFor.Lote.Last.SegmentoA.Last.SegmentoC, nLinha);
//    LerSegmentoE(PagFor.Lote.Last.SegmentoA.Last.SegmentoE, I);
//    LerSegmentoF(PagFor.Lote.Last.SegmentoA.Last.SegmentoF, I);
//    LerSegmentoZ(PagFor.Lote.Last.SegmentoA.Last.SegmentoZ, I);

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'B',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'C',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;
    (*
    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoE.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoE.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'E',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoF.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoF.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'F',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;
    *)

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoB(mSegmentoBList: TSegmentoBList;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3B') then
    Exit;

  if LerCampo(Linha, 15, 2, tcStr) <> '' then    // PIX
  begin
    // Por padrão nada a ser implementado
  end
  else
  begin
    mSegmentoBList.New;

    with mSegmentoBList.Last do
    begin
      Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
      Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);

      Endereco.Logradouro := LerCampo(Linha, 33, 30, tcStr);
      Endereco.Numero := LerCampo(Linha, 63, 5, tcStr);
      Endereco.Complemento := LerCampo(Linha, 68, 15, tcStr);
      Endereco.Bairro := LerCampo(Linha, 83, 15, tcStr);
      Endereco.Cidade := LerCampo(Linha, 98, 20, tcStr);
      Endereco.CEP := LerCampo(Linha, 118, 8, tcInt);
      Endereco.Estado := LerCampo(Linha, 126, 2, tcStr);

      DataVencimento := LerCampo(Linha, 128, 8, tcDat);
      Valor := LerCampo(Linha, 136, 15, tcDe2);
      Abatimento := LerCampo(Linha, 151, 15, tcDe2);
      Desconto := LerCampo(Linha, 166, 15, tcDe2);
      Mora := LerCampo(Linha, 181, 15, tcDe2);
      Multa := LerCampo(Linha, 196, 15, tcDe2);
      CodigoDoc := LerCampo(Linha, 211, 15, tcStr);
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoC(mSegmentoCList: TSegmentoCList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3C') then
    Exit;

  mSegmentoCList.New;

  with mSegmentoCList.Last do
  begin
    ValorIR := LerCampo(Linha, 18, 15, tcDe2);
    ValorISS := LerCampo(Linha, 33, 15, tcDe2);
    ValorIOF := LerCampo(Linha, 48, 15, tcDe2);
    Deducoes := LerCampo(Linha, 63, 15, tcDe2);
    Acrescimos := LerCampo(Linha, 78, 15, tcDe2);

    Substituta.ContaCorrente.Agencia.Codigo := LerCampo(Linha, 93, 5, tcInt);
    Substituta.ContaCorrente.Agencia.DV := LerCampo(Linha, 98, 1, tcStr);
    Substituta.ContaCorrente.Conta.Numero := LerCampo(Linha, 99, 12, tcInt64);
    Substituta.ContaCorrente.Conta.DV := LerCampo(Linha, 111, 1, tcStr);
    Substituta.ContaCorrente.DV := LerCampo(Linha, 112, 1, tcStr);

    ValorINSS := LerCampo(Linha, 113, 15, tcDe2);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoE(nLinha: Integer);
var
  RegSeg: string;
  Ok: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3E') then
    Exit;

  PagFor.Lote.Last.SegmentoE.New;

  with PagFor.Lote.Last.SegmentoE.Last do
  begin
    Convenio := LerCampo(Linha, 33, 20, tcStr);

    ContaCorrente.Agencia.Codigo := LerCampo(Linha, 53, 5, tcInt);
    ContaCorrente.Agencia.DV := LerCampo(Linha, 58, 1, tcStr);
    ContaCorrente.Conta.Numero := LerCampo(Linha, 59, 12, tcInt64);
    ContaCorrente.Conta.DV := LerCampo(Linha, 71, 1, tcStr);
    ContaCorrente.DV := LerCampo(Linha, 72, 1, tcStr);

    Nome := LerCampo(Linha, 73, 30, tcStr);

    NaturezaLanc := StrToNaturezaLanc(Ok, LerCampo(Linha, 109, 3, tcStr));
    TipoComplemento := LerCampo(Linha, 112, 2, tcInt);
    Complemento := LerCampo(Linha, 114, 20, tcStr);
    CPMF := LerCampo(Linha, 134, 1, tcStr);
    DataContabil := LerCampo(Linha, 135, 8, tcDat);

    DataLancamento := LerCampo(Linha, 143, 8, tcDat);
    Valor := LerCampo(Linha, 151, 18, tcDe2);
    TipoLancamento := LerCampo(Linha, 169, 1, tcStr);
    Categoria := LerCampo(Linha, 170, 3, tcInt);
    CodigoHistorico := LerCampo(Linha, 173, 4, tcStr);
    Historico := LerCampo(Linha, 177, 25, tcStr);
    NumeroDocumento := LerCampo(Linha, 202, 39, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoF(mSegmentoFList: TSegmentoFList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3F') then
    Exit;

  mSegmentoFList.New;

  with mSegmentoFList.Last do
  begin
    InformacaoComplementar := LerCampo(Linha, 18, 144, tcStr);
//    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
  end;

  // Falta Implementar
end;

procedure TArquivoR_CNAB240.LerSegmentoG(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3G') then
    Exit;

  PagFor.Lote.Last.SegmentoG.New;

  with PagFor.Lote.Last.SegmentoG.Last do
  begin
    CodigoBarras := LerCampo(Linha, 18, 44, tcStr);
    Cedente.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 62, 1, tcStr));

    if Cedente.Inscricao.Tipo = tiCNPJ then
      Cedente.Inscricao.Numero := LerCampo(Linha, 64, 14, tcStr)
    else
      Cedente.Inscricao.Numero := LerCampo(Linha, 67, 11, tcStr);

    Cedente.Nome := LerCampo(Linha, 78, 30, tcStr);

    Vencimento := LerCampo(Linha, 108, 8, tcDat);
    ValorTitulo := LerCampo(Linha, 116, 15, tcDe2);
    QtdeMoeda := LerCampo(Linha, 131, 15, tcDe5);
    CodigoMoeda := LerCampo(Linha, 146, 2, tcInt);
    NumeroDocumento := LerCampo(Linha, 148, 15, tcStr);
    AgenciaCobradora := LerCampo(Linha, 163, 5, tcInt);
    DVCobradora := LerCampo(Linha, 168, 1, tcStr);
    Praca := LerCampo(Linha, 169, 10, tcStr);
    Carteira := LerCampo(Linha, 179, 1, tcStr);
    EspecieTitulo := LerCampo(Linha, 180, 2, tcInt);
    DataEmissao := LerCampo(Linha, 182, 8, tcDat);
    JurosMora := LerCampo(Linha, 190, 15, tcDe2);

    //Em algumas situações o banco manda tudo como 999... ou 555...
    //Multa não pode ultrapassar 10%
    if JurosMora > (ValorTitulo * 0.1) then
      JurosMora := 0;

    Desconto1.Codigo := LerCampo(Linha, 205, 1, tcInt);
    Desconto1.Data := LerCampo(Linha, 206, 8, tcDat);
    Desconto1.Valor := LerCampo(Linha, 214, 15, tcDe2);

    CodigoProtesto := LerCampo(Linha, 229, 1, tcInt);
    PrazoProtesto := LerCampo(Linha, 230, 2, tcInt);
    DataLimite := LerCampo(Linha, 232, 8, tcDat);
  end;

  LerSegmentoH(PagFor.Lote.Last.SegmentoG.Last.SegmentoH, nLinha + 1);
end;

procedure TArquivoR_CNAB240.LerSegmentoH(mSegmentoHList: TSegmentoHList;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3H') then
    Exit;

  mSegmentoHList.New;

  with mSegmentoHList.Last do
  begin
    Avalista.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));

    if Avalista.Inscricao.Tipo = tiCNPJ then
      Avalista.Inscricao.Numero := LerCampo(Linha, 20, 14, tcStr)
    else
      Avalista.Inscricao.Numero := LerCampo(Linha, 23, 11, tcStr);

    Avalista.Nome := LerCampo(Linha, 34, 40, tcStr);

    Desconto2.Codigo := LerCampo(Linha, 74, 1, tcInt);
    Desconto2.Data := LerCampo(Linha, 75, 8, tcDat);
    Desconto2.Valor := LerCampo(Linha, 83, 15, tcDe2);

    Desconto3.Codigo := LerCampo(Linha, 98, 1, tcInt);
    Desconto3.Data := LerCampo(Linha, 99, 8, tcDat);
    Desconto3.Valor := LerCampo(Linha, 107, 15, tcDe2);

    Multa.Codigo := LerCampo(Linha, 122, 1, tcInt);
    Multa.Data := LerCampo(Linha, 123, 8, tcDat);
    Multa.Valor := LerCampo(Linha, 131, 15, tcDe2);

    Abatimento := LerCampo(Linha, 146, 15, tcDe2);
    Informacao1 := LerCampo(Linha, 161, 40, tcStr);
    Informacao2 := LerCampo(Linha, 201, 40, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoJ(nLinha: Integer; var LeuRegistroJ: boolean);
var
  mOk: Boolean;
  RegSeg, RegOpc: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  if (RegSeg <> '3J') and (RegSeg <> '3B') and (RegSeg <> '3C') and (RegSeg <> '3Z') then
    Exit;

  if (RegSeg = '3J') then
    LeuRegistroJ := True;

  if (RegOpc <> '52') and (RegOpc <> '99') and
     (RegSeg <> '3B') and (RegSeg <> '3C') and (RegSeg <> '3Z') then
  begin
    PagFor.Lote.Last.SegmentoJ.New;

    with PagFor.Lote.Last.SegmentoJ.Last do
    begin
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      CodigoBarras := LerCampo(Linha, 18, 44, tcStr);
      NomeCedente := LerCampo(Linha, 62, 30, tcStr);
      DataVencimento := LerCampo(Linha, 92, 8, tcDat);

      ValorTitulo := LerCampo(Linha, 100, 15, tcDe2);
      Desconto := LerCampo(Linha, 115, 15, tcDe2);
      Acrescimo := LerCampo(Linha, 130, 15, tcDe2);
      DataPagamento := LerCampo(Linha, 145, 8, tcDat);
      ValorPagamento := LerCampo(Linha, 153, 15, tcDe2);
      QtdeMoeda := LerCampo(Linha, 168, 15, tcDe5);
      ReferenciaSacado := LerCampo(Linha, 183, 20, tcStr);
      NossoNumero := LerCampo(Linha, 203, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

      GerarAvisos(CodOcorrencia, 'J', '', ReferenciaSacado);
    end;
  end;

  // Segmentos B, C, Z, etc. também existem para outros tipos de segmento que
  // não sejam o J, portanto, só deve processar nessa rotina se o lote que está
  // sendo processado é realmente de tipos J.
  // O Itau, por exemplo, retorna arquivo com segmentos A contendo segmentos B
  // quando é pagamento de PIX e nesse caso, não pode processar o segmento B
  // nessa rotina pois não se refere a segmentos J.

  if not LeuRegistroJ then
    exit;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  while (Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0) or
        (RegSeg = '3J') and (Pos(RegOpc, '52/99/') > 0) do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida

    {opcionais segmento J}
    LerSegmentoJ52(PagFor.Lote.Last.SegmentoJ.Last.SegmentoJ52, nLinha);
    LerSegmentoJ99(PagFor.Lote.Last.SegmentoJ.Last.SegmentoJ99, nLinha);
//    LerSegmentoB(PagFor.Lote.Last.SegmentoJ.Last.SegmentoB, I);
//    LerSegmentoC(PagFor.Lote.Last.SegmentoJ.Last.SegmentoC, I);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoJ.Last.SegmentoZ, nLinha);

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
    RegOpc := LerCampo(Linha, 18, 2, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSegOpc: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSegOpc := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr) +
               LerCampo(Linha, 18, 2, tcStr);

  if RegSegOpc <> '3J52' then
    Exit;

  mSegmentoJ52List.New;

  with mSegmentoJ52List.Last do
  begin
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));

    Pagador.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 20, 1, tcStr));
    Pagador.Inscricao.Numero := LerCampo(Linha, 21, 15, tcStr);
    Pagador.Nome := LerCampo(Linha, 36, 40, tcStr);

    Beneficiario.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 76, 1, tcStr));
    Beneficiario.Inscricao.Numero := LerCampo(Linha, 77, 15, tcStr);
    Beneficiario.Nome := LerCampo(Linha, 92, 40, tcStr);

    SacadorAvalista.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 132, 1, tcStr));
    SacadorAvalista.Inscricao.Numero := LerCampo(Linha, 133, 15, tcStr);
    SacadorAvalista.Nome := LerCampo(Linha, 148, 40, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoJ99(mSegmentoJ99List: TSegmentoJ99List;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSegOpc: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSegOpc := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr) +
               LerCampo(Linha, 18, 2, tcStr);

  if RegSegOpc <> '3J99' then
    Exit;

  mSegmentoJ99List.New;

  with mSegmentoJ99List.Last do
  begin
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    CodAutenticacao := LerCampo(Linha, 20, 10, tcInt);
    NunDocumento := LerCampo(Linha, 30, 25, tcStr);
    DataHoraPagamento := LerCampo(Linha, 55, 8, tcDat);
    ProtocoloPagamento := LerCampo(Linha, 39, 70, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN(mSegmentoN: TSegmentoN);
var
  mOk: Boolean;
begin
  with mSegmentoN do
  begin
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    SeuNumero := LerCampo(Linha, 18, 20, tcStr);
    NossoNumero := LerCampo(Linha, 38, 20, tcStr);
    NomeContribuinte := LerCampo(Linha, 58, 30, tcStr);
    DataPagamento := LerCampo(Linha, 88, 8, tcDat);
    ValorPagamento := LerCampo(Linha, 96, 15, tcDe2);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN1(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N1 - GPS
  if LerCampo(Linha, 133, 2, tcInt) <> 17 then
    Exit;

  PagFor.Lote.Last.SegmentoN1.New;

  with PagFor.Lote.Last.SegmentoN1.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    MesAnoCompetencia := LerCampo(Linha, 135, 6, tcInt);
    ValorTributo := LerCampo(Linha, 141, 15, tcDe2);
    ValorOutrasEntidades := LerCampo(Linha, 156, 15, tcDe2);
    AtualizacaoMonetaria := LerCampo(Linha, 171, 15, tcDe2);
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN1.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN2(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N2 - DARF NORMAL
  if LerCampo(Linha, 133, 2, tcInt) <> 16 then
    Exit;

  PagFor.Lote.Last.SegmentoN2.New;

  with PagFor.Lote.Last.SegmentoN2.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    Referencia := LerCampo(Linha, 143, 17, tcStr);
    ValorPrincipal := LerCampo(Linha, 160, 15, tcDe2);
    Multa := LerCampo(Linha, 175, 15, tcDe2);
    Juros := LerCampo(Linha, 190, 15, tcDe2);
    DataVencimento := LerCampo(Linha, 205, 8, tcDat);
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN2.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN3(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N3 - DARF SIMPLES
  if LerCampo(Linha, 133, 2, tcInt) <> 18 then
    Exit;

  PagFor.Lote.Last.SegmentoN3.New;

  with PagFor.Lote.Last.SegmentoN3.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN3.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN4(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
  Tributo: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N4 - GARE-SP (ICMS/DR/ITCMD)
  Tributo := LerCampo(Linha, 133, 2, tcInt);

  if  not (Tributo in [22, 23, 24]) then
    Exit;

  PagFor.Lote.Last.SegmentoN4.New;

  with PagFor.Lote.Last.SegmentoN4.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
//    Periodo := LerCampo(Linha, 135, 8, tcDat);
//    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
//    Percentual := LerCampo(Linha, 158, 7, tcDe2);
//    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN4.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN567(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
  Tributo: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N567 - IPVA, DPVAT,  LICENCIAMENTO
  Tributo := LerCampo(Linha, 133, 2, tcInt);

  if  not (Tributo in [25, 27, 26]) then
    Exit;

  PagFor.Lote.Last.SegmentoN567.New;

  with PagFor.Lote.Last.SegmentoN567.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    {
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
    }
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN567.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN8(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N8 - DARJ
  if LerCampo(Linha, 133, 2, tcInt) <> 21 then
    Exit;

  PagFor.Lote.Last.SegmentoN8.New;

  with PagFor.Lote.Last.SegmentoN8.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
//    Periodo := LerCampo(Linha, 135, 8, tcDat);
//    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
//    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
//    Juros := LerCampo(Linha, 195, 15, tcDe2);
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN8.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoN9(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

{
  Verificar se é o Segmento N9 mesmo

  Tributos Municipais:
  19 = Tributo - IPTU – Prefeituras
}

  PagFor.Lote.Last.SegmentoN9.New;

  with PagFor.Lote.Last.SegmentoN9.Last do
  begin
    LerSegmentoN(SegmentoN);
    {
    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
    }
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN9.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoO(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3O' then
    Exit;

  PagFor.Lote.Last.SegmentoO.New;

  with PagFor.Lote.Last.SegmentoO.Last do
  begin
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    CodigoBarras := LerCampo(Linha, 18, 44, tcStr);
    NomeConcessionaria := LerCampo(Linha, 62, 30, tcStr);
    DataVencimento := LerCampo(Linha, 92, 8, tcDat);
    DataPagamento := LerCampo(Linha, 100, 8, tcDat);
    ValorPagamento := LerCampo(Linha, 108, 15, tcDe2);
    SeuNumero := LerCampo(Linha, 123, 20, tcStr);
    NossoNumero := LerCampo(Linha, 143, 15, tcStr);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'O', '', SeuNumero);
  end;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3Z') > 0 do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida

    {opcionais segmento O}
    LerSegmentoW(PagFor.Lote.Last.SegmentoO.Last.SegmentoW, nLinha);
    LerSegmentoB(PagFor.Lote.Last.SegmentoO.Last.SegmentoB, nLinha);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoO.Last.SegmentoZ, nLinha);

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoW(mSegmentoWList: TSegmentoWList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3W' then
    Exit;

  PagFor.Lote.Last.SegmentoW.New;

  with PagFor.Lote.Last.SegmentoW.Last do
  begin
    ComplementoRegistro := LerCampo(Linha, 15, 1, tcInt);
    Informacoes1ou2 := LerCampo(Linha, 16, 1, tcStr);
    Informacoes1 := LerCampo(Linha, 17, 80, tcStr);
    Informacoes2 := LerCampo(Linha, 97, 80, tcStr);
    Informacoes3 := LerCampo(Linha, 177, 50, tcStr);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'W', '', '');
  end;
end;

procedure TArquivoR_CNAB240.LerSegmentoZ(mSegmentoZList: TSegmentoZList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3Z' then
    Exit;

  mSegmentoZList.New;

  with mSegmentoZList.Last do
  begin
    Autenticacao := LerCampo(Linha, 15, 64, tcStr);
    SeuNumero := LerCampo(Linha, 79, 20, tcStr);
    NossoNumero := LerCampo(Linha, 104, 15, tcStr);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'Z', '', '');
  end;
end;

procedure TArquivoR_CNAB240.LerLote;
var
  I: Integer;
  // Necessario para ler os segmentos opcionais (B, C, etc)
  // apenas se eles pertencem a um segmento J
  LeuRegistroJ: boolean;
  xReg: string;
begin
  LeuRegistroJ := False;

  try
    for I := 1 to ArquivoTXT.Count - 1 do
    begin
      xReg := Copy(ArquivoTXT.Strings[I], 8, 1);

      if xReg = '1' then
      begin
        LerRegistro1(I);
        LeuRegistroJ := False; // Sempre reseta em um novo lote
      end;

      if (xReg <> '1') and (xReg <> '5') and (xReg <> '9') then
      begin
        LerSegmentoA(I);
        LerSegmentoE(I);
        LerSegmentoG(I);
        LerSegmentoJ(I, LeuRegistroJ);
        LerSegmentoN1(I);
        LerSegmentoN2(I);
        LerSegmentoN3(I);
        LerSegmentoN4(I);
        LerSegmentoN567(I);
        LerSegmentoN8(I);
        LerSegmentoN9(I);
        LerSegmentoO(I);
      end;

      if xReg = '5' then
        LerRegistro5(I);
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create('Nao Foi Possível ler os registros no arquivo.' + #13 +
        E.Message);
    end;
  end;
end;

function TArquivoR_CNAB240.LerTxt: Boolean;
begin
  ArquivoTXT := TStringList.Create;

  try
    try
      ArquivoTXT.Text := Arquivo;

      LerRegistro0;

      LerLote;

      LerRegistro9(ArquivoTXT.Count - 1);

      Result := True;
    except
      on E: Exception do
      begin
        raise Exception.Create('Não Foi Possível ler os Registros do Arquivo' + #13 +
          E.Message);
      end;
    end;
  finally
    ArquivoTXT.Free;
  end;
end;

end.
